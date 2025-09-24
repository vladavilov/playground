import structlog
from pathlib import Path
from typing import Any, Dict
import shutil

from utils.neo4j_client import get_neo4j_client
import graphrag.api as graphrag_api

from config import get_graphrag_settings
from .settings import configure_settings_for_json
from graphrag.config.create_graphrag_config import create_graphrag_config
from .parquet_reader import ParquetReader
from .lancedb_reader import LanceDBReader
from .neo4j_ingestor import Neo4jIngestor
from .callbacks import IngestionWorkflowCallbacks
 

logger = structlog.get_logger(__name__)


async def _run(workspace: Path, cb: IngestionWorkflowCallbacks) -> None:
    """Run GraphRAG indexing pipeline programmatically using the library API.

    Uses the common logger; no separate file handler/log file is configured here.
    """
    # Build config dict programmatically and create GraphRAG config without writing files
    config = create_graphrag_config(configure_settings_for_json(), str(workspace))

    try:
        callbacks = [cb]
        await graphrag_api.build_index(config=config, callbacks=callbacks)
        # Ensure all pending progress publishes are flushed
        try:
            await cb.drain()
        except Exception:
            pass
    except Exception as e:
        logger.exception("GraphRAG indexing failed")
        raise RuntimeError("GraphRAG pipeline failed") from e


def ensure_workspace_initialized(workspace_root: Path, project_id: str) -> Path:
    """Ensure workspace folder exists under <root>/<project_id> with input/output dirs."""
    workspace = Path(workspace_root) / project_id
    workspace.mkdir(parents=True, exist_ok=True)
    (workspace / "input").mkdir(parents=True, exist_ok=True)
    (workspace / "output").mkdir(parents=True, exist_ok=True)
    # Ensure cache directories exist for GraphRAG cached LLM calls
    (workspace / "cache").mkdir(parents=True, exist_ok=True)
    (workspace / "cache" / "extract_graph").mkdir(parents=True, exist_ok=True)
    return workspace


async def run_graphrag_pipeline(project_id: str) -> Dict[str, Any]:
    if not isinstance(project_id, str) or not project_id.strip():
        raise ValueError("project_id must be a non-empty string")

    settings = get_graphrag_settings()
    root = Path(settings.RAG_WORKSPACE_ROOT)

    workspace = ensure_workspace_initialized(root, project_id)

    cb = IngestionWorkflowCallbacks(project_id=project_id)

    await _run(workspace, cb)

    # Post-processing: load outputs into Neo4j
    output_dir = workspace / "output"
    client = get_neo4j_client()
    driver = client.driver
    try:
        # Initialize readers and ingestor
        pq = ParquetReader()
        ingestor = Neo4jIngestor(driver=driver, project_id=project_id)

        # Read parquet records and ingest
        records = {
            "documents": pq.read_documents(output_dir),
            "chunks": pq.read_chunks(output_dir),
            "entities": pq.read_entities(output_dir),
            "entity_relationships": pq.read_entity_relationships(output_dir),
            "community_reports": pq.read_community_reports(output_dir),
            "communities": pq.read_communities(output_dir),
        }
        imported = ingestor.ingest_all_parquet(records)

        # Read LanceDB vectors and ingest
        ldb = LanceDBReader()
        vectors = ldb.read_all_embeddings(workspace, cb)
        vector_counts = ingestor.ingest_all_vectors(vectors, callbacks=cb)

        # Backfills
        ingestor.backfill_entity_relationship_ids(cb)
        ingestor.backfill_community_membership(cb)

        logger.info("Neo4j import counts", imported=imported, vectors=vector_counts)
    finally:
        try:
            driver.close()
        except Exception:
            pass

    try:
        shutil.rmtree(workspace, ignore_errors=True)
    except Exception:
        logger.warning("Failed to remove workspace", workspace=str(workspace))

    return {
        "success": True,
        "output_dir": str(output_dir),
        "neo4j_import": imported,
        "vector_import": vector_counts,
    }


