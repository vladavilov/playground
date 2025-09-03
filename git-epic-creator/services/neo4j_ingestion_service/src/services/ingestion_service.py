import logging
import os
import time
from pathlib import Path
from uuid import UUID
from typing import Any, Dict

from utils.neo4j_client import Neo4jClient
from utils.blob_storage import get_blob_storage_client
from configuration.common_config import get_app_settings
from ingestion.graphrag_pipeline import prepare_document_from_json, run_documents, run_community_analysis

class Neo4jIngestionService:
    
    def __init__(self, client: Neo4jClient):
        self.client = client
        self.logger = logging.getLogger(__name__)

    async def run_graphrag_pipeline(self, project_id: str) -> Dict[str, Any]:
        if not isinstance(project_id, str) or not project_id.strip():
            raise ValueError("project_id must be a non-empty string")

        start = time.time()

        documents = 0

        # 1) Sync inputs from Blob → WORKDIR/{project_id}/input (JSON files only)
        try:
            workspace_root = Path(get_app_settings().graphrag.RAG_WORKSPACE_ROOT)
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
                        await run_documents(self.client.driver, documents_to_ingest)
                        self.logger.info("Successfully processed %d documents through GraphRAG pipeline", len(documents_to_ingest))
                        
                        # Run community analysis after successful GraphRAG ingestion
                        try:
                            self.logger.info("Starting community analysis pipeline")
                            community_result = await run_community_analysis(self.client.driver)

                            if community_result.get("success"):
                                self.logger.info(
                                    "Community analysis completed: %d communities detected, %d summarized",
                                    community_result.get("communities_detected", 0),
                                    community_result.get("communities_summarized", 0),
                                )
                            else:
                                self.logger.warning(
                                    "Community analysis failed: %s",
                                    community_result.get("error", "Unknown error"),
                                )
                        except Exception as e:
                            self.logger.warning(
                                "Community analysis failed, continuing with cleanup: %s",
                                str(e),
                            )
                        
                        # Cleanup source JSONs only after successful ingestion
                        for blob_name in blobs_to_delete:
                            try:
                                blob_client.delete_file(blob_name, project_id=parsed_project_id)
                            except Exception:
                                pass
                    except Exception as e:
                        self.logger.error("Failed to run GraphRAG pipeline: %s", str(e))
        except Exception as e:
            self.logger.warning("Blob sync skipped due to error: %s", str(e))

        return {
            "success": True,
            "counts": {
                "documents": documents
            },
            "duration_ms": round((time.time() - start) * 1000, 2),
        }
