import logging
import os
import time
from pathlib import Path
from typing import Any, Dict

from utils.neo4j_client import Neo4jClient
from utils.blob_storage import get_blob_storage_client
from ingestion.graphrag_runner import run_index
from ingestion.importer import import_graphrag_outputs

class Neo4jIngestionService:
    
    def __init__(self, client: Neo4jClient):
        self.client = client
        self.logger = logging.getLogger(__name__)

    def run_graphrag_pipeline(self, project_id: str) -> Dict[str, Any]:
        if not isinstance(project_id, str) or not project_id.strip():
            raise ValueError("project_id must be a non-empty string")

        start = time.time()

        documents = 0
        text_units = 0
        entities = 0
        relationships = 0
        communities = 0
        community_reports = 0

        # 1) Sync inputs from Blob → WORKDIR/input
        try:
            workspace_root = Path(os.getenv("RAG_WORKSPACE_ROOT", "./graphrag"))
            input_dir = workspace_root / project_id / "input"
            input_dir.mkdir(parents=True, exist_ok=True)

            prefix_tmpl = os.getenv("AZURE_BLOB_PREFIX_TEMPLATE", "inputs/{project_id}")
            prefix = prefix_tmpl.format(project_id=project_id)

            blob_client = get_blob_storage_client()
            list_result = blob_client.list_files(prefix=prefix)
            if getattr(list_result, "success", False):
                for blob_name in list_result.file_list or []:
                    dest_path = input_dir / os.path.basename(blob_name)
                    blob_client.download_file(blob_name, str(dest_path))
        except Exception as e:
            self.logger.warning("Blob sync skipped due to error: %s", str(e))

        # 2) Run GraphRAG index for the project
        workdir: Path
        try:
            workdir = run_index(project_id)
        except Exception as e:
            self.logger.warning("GraphRAG index skipped due to error: %s", str(e))
            workdir = Path(os.getenv("RAG_WORKSPACE_ROOT", "./graphrag")) / project_id

        # 3) Import GraphRAG parquet outputs into Neo4j (counts only in this environment)
        try:
            counts = import_graphrag_outputs(workdir, self.client)
            documents = counts.get("documents", 0)
            text_units = counts.get("text_units", 0)
            entities = counts.get("entities", 0)
            relationships = counts.get("relationships", 0)
            communities = counts.get("communities", 0)
            community_reports = counts.get("community_reports", 0)
        except Exception as e:
            self.logger.warning("Parquet import skipped due to error: %s", str(e))

        return {
            "counts": {
                "documents": documents,
                "text_units": text_units,
                "entities": entities,
                "relationships": relationships,
                "communities": communities,
                "community_reports": community_reports,
            },
            "duration_ms": round((time.time() - start) * 1000, 2),
        }
