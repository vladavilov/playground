import asyncio
import json
from uuid import UUID
import structlog

from graphrag.callbacks.workflow_callbacks import WorkflowCallbacks
from graphrag.logger.progress import Progress
from utils.redis_client import get_redis_client
from utils.unified_redis_messages import ProjectProgressMessage
from constants import UI_CHANNEL_PREFIX, UI_PROJECT_PROGRESS_NAME

logger = structlog.get_logger(__name__)

class IngestionWorkflowCallbacks(WorkflowCallbacks):
    """Lightweight callbacks to log progress and publish status updates."""

    def __init__(self, project_id: str) -> None:
        self.project_id = project_id
        self._workflow_index = 0
        self._total_workflows = 0
        self._pending_tasks: list[asyncio.Task] = []
        try:
            self._loop = asyncio.get_running_loop()
        except RuntimeError:
            self._loop = None

    async def _publish(self, process_step: str, progress: Progress | None = None) -> None:
        processed = getattr(progress, "completed_items", None)
        total = getattr(progress, "total_items", None)
        try:
            # Convert project_id to UUID for the message schema
            project_uuid = UUID(self.project_id)

            message = ProjectProgressMessage(
                project_id=project_uuid,
                status="rag_processing",
                process_step=process_step,
                processed_count=processed,
                total_count=total,
                processed_pct=(float(processed) / float(total) * 100.0) if (processed is not None and total) else None,
            )

            channel = f"{UI_CHANNEL_PREFIX}:{UI_PROJECT_PROGRESS_NAME}"
            redis = get_redis_client()
            await redis.publish(channel, json.dumps(message.to_dict()))
        except Exception:
            # Intentionally ignore publishing errors to avoid impacting the pipeline
            pass

    def _schedule(self, process_step: str, progress: Progress | None = None) -> None:
        def _create() -> None:
            try:
                task = asyncio.create_task(self._publish(process_step, progress))
                self._pending_tasks.append(task)
            except Exception:
                pass

        try:
            if self._loop and self._loop.is_running():
                self._loop.call_soon_threadsafe(_create)
            else:
                _create()
        except Exception:
            pass

    async def drain(self) -> None:
        pending = [t for t in self._pending_tasks if not t.done()]
        self._pending_tasks.clear()
        if not pending:
            return
        try:
            await asyncio.gather(*pending, return_exceptions=True)
        except Exception:
            pass

    # WorkflowCallbacks overrides
    def pipeline_start(self, names: list[str]) -> None:  # type: ignore[override]
        self._workflow_index = 0
        self._total_workflows = len(names or [])
        logger.info("GraphRAG pipeline_start", total_workflows=self._total_workflows)
        self._schedule("pipeline_start")

    def pipeline_end(self, results) -> None:  # type: ignore[override]
        logger.info("GraphRAG pipeline_end")
        self._schedule("pipeline_end")

    def workflow_start(self, name: str, instance: object) -> None:  # type: ignore[override]
        self._workflow_index += 1
        logger.info(
            "GraphRAG workflow_start",
            workflow=name,
            index=self._workflow_index,
            total=self._total_workflows,
        )
        self._schedule(f"workflow_start: {name} - {self._workflow_index}/{self._total_workflows}")

    def workflow_end(self, name: str, instance: object) -> None:  # type: ignore[override]
        logger.info("GraphRAG workflow_end", workflow=name)
        self._schedule(f"workflow_end: {name}")

    def progress(self, progress: Progress) -> None:
        logger.info("GraphRAG progress", description=(progress.description or ""))
        self._schedule(f"progress: {progress.description or ''}", progress)


