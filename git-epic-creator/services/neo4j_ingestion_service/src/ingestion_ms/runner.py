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


async def _run(workspace: Path, cb: IngestionWorkflowCallbacks) -> Dict[str, Any]:
    """Run GraphRAG indexing pipeline programmatically using the library API.

    Uses the common logger; no separate file handler/log file is configured here.
    
    Returns:
        Dict with pipeline execution status:
        {
            'success': bool,
            'output_dir': str,
            'error': str (if failed)
        }
    """
    # Build config dict programmatically and create GraphRAG config without writing files
    settings_dict = configure_settings_for_json()
    
    # Log critical extraction settings for debugging schema drift issues
    extract_config = settings_dict.get("extract_graph", {})
    logger.info(
        "GraphRAG extraction configuration",
        max_gleanings=extract_config.get("max_gleanings"),
        temperature=extract_config.get("temperature"),
        top_p=extract_config.get("top_p"),
        n=extract_config.get("n"),
        model_id=extract_config.get("model_id"),
    )
    
    config = create_graphrag_config(settings_dict, str(workspace))
    output_dir = workspace / "output"

    try:
        callbacks = [cb]
        await graphrag_api.build_index(config=config, callbacks=callbacks)
        # Ensure all pending progress publishes are flushed
        try:
            await cb.drain()
        except Exception:
            pass
        
        logger.info("GraphRAG pipeline completed successfully", output_dir=str(output_dir))
        return {
            'success': True,
            'output_dir': str(output_dir),
        }
    except Exception as e:
        logger.exception("GraphRAG indexing failed")
        return {
            'success': False,
            'output_dir': str(output_dir),
            'error': str(e),
        }


def ensure_workspace_initialized(workspace_root: Path, project_id: str) -> Path:
    """Ensure workspace folder exists under <root>/<project_id> with input/output dirs."""
    workspace = Path(workspace_root) / project_id
    workspace.mkdir(parents=True, exist_ok=True)
    (workspace / "input").mkdir(parents=True, exist_ok=True)
    (workspace / "output").mkdir(parents=True, exist_ok=True)
    return workspace


async def run_graphrag_pipeline(project_id: str) -> Dict[str, Any]:
    if not isinstance(project_id, str) or not project_id.strip():
        raise ValueError("project_id must be a non-empty string")

    settings = get_graphrag_settings()
    root = Path(settings.RAG_WORKSPACE_ROOT)

    workspace = ensure_workspace_initialized(root, project_id)

    cb = IngestionWorkflowCallbacks(project_id=project_id)

    # Run pipeline and capture result
    pipeline_result = await _run(workspace, cb)
    
    # Abort if pipeline failed
    if not pipeline_result.get('success'):
        error_msg = pipeline_result.get('error', 'Unknown error')
        logger.error(
            "GraphRAG pipeline failed - aborting ingestion",
            project_id=project_id,
            error=error_msg
        )
        raise RuntimeError(f"GraphRAG pipeline failed: {error_msg}")

    # Post-processing: load outputs into Neo4j
    output_dir = workspace / "output"
    
    # Validate critical artifacts exist before proceeding
    entities_parquet = output_dir / "entities.parquet"
    communities_parquet = output_dir / "communities.parquet"
    
    entities_exist = entities_parquet.exists() and entities_parquet.stat().st_size > 0
    communities_exist = communities_parquet.exists() and communities_parquet.stat().st_size > 0
    
    if not entities_exist and not communities_exist:
        error_msg = (
            "GraphRAG pipeline produced no entities or communities. "
            "This indicates the pipeline failed to extract any meaningful graph structure. "
            "Possible causes: empty input documents, LLM extraction failures, or schema mismatches."
        )
        logger.error(
            "Critical artifacts missing after pipeline execution",
            project_id=project_id,
            entities_exist=entities_exist,
            communities_exist=communities_exist,
            output_dir=str(output_dir)
        )
        raise RuntimeError(error_msg)
    
    if not entities_exist:
        logger.warning(
            "entities.parquet missing or empty - graph will have no entity nodes",
            project_id=project_id,
            output_dir=str(output_dir)
        )
    
    if not communities_exist:
        logger.warning(
            "communities.parquet missing or empty - DRIFT search will be degraded",
            project_id=project_id,
            output_dir=str(output_dir)
        )
    
    client = get_neo4j_client()
    driver = client.driver
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
    ingestor.backfill_community_hierarchy(cb)
    ingestor.backfill_community_ids(cb)
    
    # Sync entity relationship metadata (ensures property matches graph state)
    ingestor.sync_entity_relationship_ids(cb)
    
    # Comprehensive embedding validation (all node types) - CRITICAL for DRIFT search
    embedding_validation = ingestor.validate_all_embeddings(cb)
    
    # Check for critical embedding issues
    if embedding_validation.get("has_critical_issues"):
        logger.error(
            "Pipeline completed with CRITICAL embedding issues",
            project_id=project_id,
            issues=embedding_validation.get("issues", []),
            suggestions=embedding_validation.get("suggestions", [])
        )
        # Note: Pipeline continues but marks issues in result for downstream handling
    elif embedding_validation.get("has_warnings"):
        logger.warning(
            "Pipeline completed with embedding warnings",
            project_id=project_id,
            issues=embedding_validation.get("issues", [])
        )

    # Cleanup: remove orphaned nodes after deduplication and merging
    orphaned_stats = {}
    try:
        # First detect orphaned nodes for logging
        orphaned_detection = ingestor.detect_orphaned_nodes()
        logger.info("Orphaned nodes detection completed", stats=orphaned_detection)
        
        total_orphaned = orphaned_detection.get("total_orphaned", 0)
        if total_orphaned > 0:
            logger.warning(
                "Orphaned nodes detected after ingestion",
                orphaned_chunks=orphaned_detection.get("orphaned_chunks", 0),
                orphaned_entities=orphaned_detection.get("orphaned_entities", 0),
                orphaned_communities=orphaned_detection.get("orphaned_communities", 0),
                unlinked_nodes=orphaned_detection.get("unlinked_nodes", 0),
                total=total_orphaned,
            )
            
            # Cleanup orphaned nodes
            orphaned_stats = ingestor.cleanup_orphaned_nodes(cb)
            logger.info("Orphaned nodes cleanup completed", stats=orphaned_stats)
    except Exception as exc:
        logger.error("Orphaned nodes cleanup failed", error=str(exc))

    # Validation: check relationship health after ingestion
    validation_stats = {}
    try:
        validation_stats = ingestor.validate_relationships()
        logger.info("Relationship validation completed", stats=validation_stats)
        
        # Log warning if duplicate relationships detected
        duplicate_count = validation_stats.get("duplicate_count", 0)
        if duplicate_count > 0:
            logger.warning(
                "Duplicate relationships detected after ingestion",
                duplicate_count=duplicate_count,
                duplication_ratio=validation_stats.get("duplication_ratio", 0),
            )
    except Exception as exc:
        logger.error("Relationship validation failed", error=str(exc))

    logger.info("Neo4j import counts", imported=imported, vectors=vector_counts)

    try:
        shutil.rmtree(workspace, ignore_errors=True)
    except Exception:
        logger.warning("Failed to remove workspace", workspace=str(workspace))

    return {
        "success": True,
        "output_dir": str(output_dir),
        "neo4j_import": imported,
        "vector_import": vector_counts,
        "embedding_validation": embedding_validation,
        "orphaned_cleanup": orphaned_stats,
        "validation": validation_stats,
    }


