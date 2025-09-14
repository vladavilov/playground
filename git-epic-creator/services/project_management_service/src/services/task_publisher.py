from uuid import UUID

import structlog
from utils.celery_factory import get_celery_app
from utils.workflow_gating import gate_and_enqueue_async
from utils.redis_client import get_redis_client
from constants import (
    APP_NAME_PROJECT_MANAGEMENT,
    GATE_NS_DOCS,
    TASK_PROCESS_PROJECT_DOCS,
    QUEUE_DOCUMENT_PROCESSING,
)


logger = structlog.get_logger(__name__)


class TaskRequestPublisher:
    """
    Publisher that triggers document processing by enqueuing a Celery task directly.
    """

    def __init__(self) -> None:
        self.celery_app = get_celery_app(APP_NAME_PROJECT_MANAGEMENT)

    async def request_document_processing(self, project_id: UUID) -> bool:
        try:
            # Gate per project using unified lock-aware gating
            redis_client = get_redis_client()
            task_id_holder: dict[str, str | None] = {"id": None}

            def _enqueue() -> None:
                result = self.celery_app.send_task(
                    TASK_PROCESS_PROJECT_DOCS,
                    args=[str(project_id)],
                    queue=QUEUE_DOCUMENT_PROCESSING,
                )
                task_id_holder["id"] = getattr(result, "id", None)

            await gate_and_enqueue_async(
                GATE_NS_DOCS,
                str(project_id),
                client=redis_client,
                enqueue=_enqueue,
            )
            logger.info(
                "Enqueued document processing task via Celery",
                project_id=str(project_id),
                task_id=task_id_holder["id"],
                queue=QUEUE_DOCUMENT_PROCESSING,
            )
            return True
        except Exception as e:
            logger.error(
                "Failed to enqueue document processing task",
                project_id=str(project_id),
                error=str(e),
            )
            return False
