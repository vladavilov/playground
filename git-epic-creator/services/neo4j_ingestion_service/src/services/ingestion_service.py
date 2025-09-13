import logging
import os
import time
from pathlib import Path
from uuid import UUID
from typing import Any, Dict

from utils.neo4j_client import Neo4jClient
from utils.blob_storage import get_blob_storage_client
from ingestion_ms.runner import run_graphrag_pipeline as run_ms_pipeline
from config import get_graphrag_settings

class Neo4jIngestionService:
    
    def __init__(self, client: Neo4jClient):
        self.client = client
        self.logger = logging.getLogger(__name__)

    async def run_graphrag_pipeline(self, project_id: str) -> Dict[str, Any]:
        if not isinstance(project_id, str) or not project_id.strip():
            raise ValueError("project_id must be a non-empty string")

        start = time.time()

        documents = 0
        pipeline_output: Dict[str, Any] = {}

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
                blobs_to_delete: list[str] = []
                
                for blob_name in list_result.file_list or []:
                    if not str(blob_name).lower().endswith(".json"):
                        continue
                    dest_path = input_dir / os.path.basename(blob_name)
                    blob_client.download_file(blob_name, str(dest_path), project_id=parsed_project_id)
                    documents += 1
                    # Mark for deletion after successful pipeline run
                    blobs_to_delete.append(blob_name)
                
                if documents:
                    try:
                        result = await run_ms_pipeline(project_id)
                        if isinstance(result, dict):
                            pipeline_output = result
                        self.logger.info(
                            "Successfully processed %d documents with Microsoft GraphRAG",
                            documents,
                        )
                        
                        # Cleanup source JSONs only after successful ingestion
                        for blob_name in blobs_to_delete:
                            try:
                                blob_client.delete_file(blob_name, project_id=parsed_project_id)
                            except Exception:
                                pass
                    except Exception as e:
                        self.logger.error("Failed to run Microsoft GraphRAG pipeline: %s", str(e))
                        raise
        except Exception as e:
            self.logger.error("Blob ingestion failed: %s", str(e))
            raise

        return {
            "success": True,
            "counts": {
                "documents": documents
            },
            "pipeline": pipeline_output,
            "duration_ms": round((time.time() - start) * 1000, 2),
        }
