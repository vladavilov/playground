"""
Task request subscriber for document processing service.
Refactored to use shared TaskStreamSubscriber.
"""

import structlog
from utils.task_stream_subscriber import TaskStreamSubscriber
from constants import TASK_REQUEST_STREAM, DOCUMENT_PROCESSORS_CONSUMER_GROUP

logger = structlog.get_logger(__name__)


def create_task_subscriber(redis_client, process_project_documents_task) -> TaskStreamSubscriber:
    """Create task subscriber for document processing service using shared Streams subscriber."""
    stream_key = TASK_REQUEST_STREAM
    consumer_group = DOCUMENT_PROCESSORS_CONSUMER_GROUP
    consumer_name = "worker"

    def build_apply_kwargs(fields: dict) -> dict:
        if fields.get("task_type") != "process_project_documents":
            raise ValueError("unsupported task_type")
        return {
            "args": [str(fields.get("project_id"))],
            "queue": "document_processing",
            "correlation_id": str(fields.get("correlation_id")),
        }

    subscriber = TaskStreamSubscriber(
        redis_client=redis_client,
        stream_key=stream_key,
        consumer_group=consumer_group,
        consumer_name=consumer_name,
        task=process_project_documents_task,
        queue="document_processing",
        build_apply_kwargs=build_apply_kwargs,
    )
    return subscriber
