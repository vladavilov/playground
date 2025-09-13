import logging
import asyncio
import json
from uuid import UUID
from pathlib import Path
from typing import Any, Dict

from utils.neo4j_client import get_neo4j_client
import graphrag.api as graphrag_api

from config import get_graphrag_settings
from .settings import configure_settings_for_json
from graphrag.config.create_graphrag_config import create_graphrag_config
from graphrag.callbacks.workflow_callbacks import WorkflowCallbacks
from graphrag.logger.progress import Progress
from graphrag.logger.standard_logging import init_console_logger
from .neo4j_ingestion import (
    ingest_workspace_output,
    backfill_entity_relationship_ids,
    backfill_community_membership,
    backfill_vector_indexes,
)
from graphrag.storage.file_pipeline_storage import FilePipelineStorage
from utils.redis_client import get_redis_client
from utils.unified_redis_messages import ProjectProgressMessage
from constants import UI_PROJECT_PROGRESS_CHANNEL
 

logger = logging.getLogger(__name__)


class IngestionWorkflowCallbacks(WorkflowCallbacks):
    """Callbacks to log progress and publish Redis updates for UI."""

    def __init__(self, project_id: str, redis_client) -> None:
        self.project_id = project_id
        self.redis_client = redis_client
        self._workflow_index = 0
        self._total_workflows = 0

    async def _publish(self, process_step: str, progress: Progress | None = None) -> None:
        try:
            project_uuid = None
            try:
                project_uuid = UUID(self.project_id)
            except Exception:
                # Skip publish if project_id is not a valid UUID
                return
            message = ProjectProgressMessage(
                project_id=project_uuid,
                status="rag_processing",
                process_step=process_step,
                processed_count=getattr(progress, "completed_items", None),
                total_count=getattr(progress, "total_items", None),
                processed_pct=getattr(progress, "percent", None),
            )
            await self.redis_client.publish(UI_PROJECT_PROGRESS_CHANNEL, json.dumps(message.to_dict()))
        except Exception:
            # Best-effort: ignore publish errors
            pass

    def pipeline_start(self, names: list[str]) -> None:  # type: ignore[override]
        self._workflow_index = 0
        self._total_workflows = len(names or [])
        logger.info("GraphRAG pipeline_start: %d workflows", self._total_workflows)
        asyncio.create_task(self._publish("pipeline_start"))

    def pipeline_end(self, results) -> None:  # type: ignore[override]
        logger.info("GraphRAG pipeline_end")
        asyncio.create_task(self._publish("pipeline_end"))

    def workflow_start(self, name: str, instance: object) -> None:  # type: ignore[override]
        self._workflow_index += 1
        logger.info("GraphRAG workflow_start: %s (%d/%d)", name, self._workflow_index, self._total_workflows)
        asyncio.create_task(self._publish("workflow_start"))

    def workflow_end(self, name: str, instance: object) -> None:  # type: ignore[override]
        logger.info("GraphRAG workflow_end: %s", name)
        asyncio.create_task(self._publish("workflow_end"))

    def progress(self, progress: Progress) -> None:  # type: ignore[override]
        # Log best-effort progress
        try:
            if progress.percent is not None:
                logger.info("GraphRAG progress: %.2f%% - %s", progress.percent * 100.0, progress.description or "")
            elif progress.completed_items is not None and progress.total_items is not None:
                logger.info("GraphRAG progress: %s/%s - %s", progress.completed_items, progress.total_items, progress.description or "")
            else:
                logger.info("GraphRAG progress: %s", progress.description or "")
        except Exception:
            pass
        asyncio.create_task(self._publish("progress", progress))


async def _run(workspace: Path, project_id: str) -> None:
    """Run GraphRAG indexing pipeline programmatically using the library API.

    Uses the common logger; no separate file handler/log file is configured here.
    """
    # Build config dict programmatically and create GraphRAG config without writing files
    config = create_graphrag_config(configure_settings_for_json(workspace), str(workspace))

    # Ensure Graphrag logs stay at INFO and do not propagate as WARNING via root handlers
    try:
        init_console_logger(verbose=False)
        gr_logger = logging.getLogger("graphrag")
        gr_logger.setLevel(logging.INFO)
        gr_logger.propagate = False
    except Exception:
        # Best-effort logging setup; continue even if configuration fails
        pass

    try:
        # Wire callbacks for logging + Redis updates
        redis_client = get_redis_client()
        callbacks = [IngestionWorkflowCallbacks(project_id=project_id, redis_client=redis_client)]
        results = await graphrag_api.build_index(config=config, callbacks=callbacks)
        # Log workflow results if available
        try:
            for workflow_result in results or []:
                workflow_name = getattr(workflow_result, "workflow", "unknown")
                errors = getattr(workflow_result, "errors", None)
                if errors:
                    logger.error("GraphRAG workflow %s failed: %s", workflow_name, errors)
                else:
                    logger.info("GraphRAG workflow %s succeeded", workflow_name)
        except Exception:
            # Best-effort logging; do not fail the run on logging issues
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
    return workspace


async def run_graphrag_pipeline(project_id: str) -> Dict[str, Any]:
    if not isinstance(project_id, str) or not project_id.strip():
        raise ValueError("project_id must be a non-empty string")

    settings = get_graphrag_settings()
    root = Path(settings.RAG_WORKSPACE_ROOT)

    workspace = ensure_workspace_initialized(root, project_id)

    await _run(workspace, project_id)

    # Post-processing: load parquet outputs into Neo4j
    output_dir = workspace / "output"
    client = get_neo4j_client()
    driver = client.driver
    try:
        imported = ingest_workspace_output(driver, output_dir)
        backfill_entity_relationship_ids(driver)
        backfill_community_membership(driver)
        backfill_vector_indexes(driver, workspace)
        logger.info("Neo4j import counts: %s", imported)
    finally:
        try:
            driver.close()
        except Exception:
            pass

    try:
        storage = FilePipelineStorage(root_dir=str(workspace))
        await storage.clear()
        logger.info("Workspace cleared: %s", workspace)
    except Exception:
        logger.warning("Failed to clear workspace: %s", workspace)

    return {
        "success": True,
        "output_dir": str(output_dir),
        "neo4j_import": imported,
    }


