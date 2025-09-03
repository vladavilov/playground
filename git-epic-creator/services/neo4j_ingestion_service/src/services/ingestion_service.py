import logging
import inspect
import os
import time
from pathlib import Path
from uuid import UUID
from typing import Any, Dict

from utils.neo4j_client import Neo4jClient
from utils.blob_storage import get_blob_storage_client
from ingestion.graphrag_pipeline import prepare_document_from_json, run_documents
from config import get_graphrag_settings

class Neo4jIngestionService:
    
    def __init__(self, client: Neo4jClient):
        self.client = client
        self.logger = logging.getLogger(__name__)

    def _resolve_config_path(self, filename: str) -> str:
        """
        Resolve path to a config file located under this service's source tree.
        Order:
        - Default: <package_root>/config/<filename>
        """
        package_root = Path(__file__).resolve().parent.parent
        return str(package_root / "config" / filename)

    async def run_graphrag_pipeline(self, project_id: str) -> Dict[str, Any]:
        if not isinstance(project_id, str) or not project_id.strip():
            raise ValueError("project_id must be a non-empty string")

        start = time.time()

        documents = 0

        # 1) Sync inputs from Blob → WORKDIR/{project_id}/input (JSON files only)
        try:
            workspace_root = Path(get_graphrag_settings().RAG_WORKSPACE_ROOT)
            input_dir = workspace_root / project_id / "input"
            input_dir.mkdir(parents=True, exist_ok=True)

            prefix = "output/"

            blob_client = get_blob_storage_client()
            # Try to scope by project-specific container when project_id is a valid UUID
            parsed_project_id = None
            try:
                parsed_project_id = UUID(project_id)
            except Exception:
                self.logger.warning(
                    "project_id is not a UUID; falling back to base container for blob operations (%s)",
                    project_id,
                )

            list_result = blob_client.list_files(parsed_project_id, prefix)
            if not getattr(list_result, "success", False):
                raise RuntimeError("Blob listing failed: success flag is False")
            if getattr(list_result, "success", False):
                documents_to_ingest = []
                blobs_to_delete: list[str] = []
                
                for blob_name in list_result.file_list or []:
                    if not str(blob_name).lower().endswith(".json"):
                        continue
                    dest_path = input_dir / os.path.basename(blob_name)
                    blob_client.download_file(blob_name, str(dest_path), project_id=parsed_project_id)
                    
                    try:
                        # Prepare document for ingestion
                        document = prepare_document_from_json(str(dest_path), project_id)
                        documents_to_ingest.append(document)
                        documents += 1
                        # Mark for deletion after successful pipeline run
                        blobs_to_delete.append(blob_name)
                    except Exception as e:
                        self.logger.warning("Failed to prepare document %s: %s", blob_name, str(e))
                        continue
                
                # Run the GraphRAG pipeline with all prepared documents
                if documents_to_ingest:
                    try:
                        config_path = self._resolve_config_path("kg_builder_config.yaml")
                        result = run_documents(self.client.driver, documents_to_ingest, passes=3, config_path=config_path)
                        # Support both async (real) and sync (test mock) callables
                        if inspect.isawaitable(result):
                            await result
                        self.logger.info("Successfully processed %d documents through GraphRAG pipeline", len(documents_to_ingest))
                        # Cleanup source JSONs only after successful ingestion
                        for blob_name in blobs_to_delete:
                            try:
                                blob_client.delete_file(blob_name, project_id=parsed_project_id)
                            except Exception:
                                pass
                    except Exception as e:
                        self.logger.error("Failed to run GraphRAG pipeline: %s", str(e))
                        raise
        except Exception as e:
            self.logger.error("Blob ingestion failed: %s", str(e))
            raise

        return {
            "success": True,
            "counts": {
                "documents": documents
            },
            "duration_ms": round((time.time() - start) * 1000, 2),
        }
