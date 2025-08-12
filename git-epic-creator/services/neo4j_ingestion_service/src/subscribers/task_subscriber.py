"""
Thin subscriber wrapper for Neo4j ingestion service using shared TaskStreamSubscriber.
"""

from utils.task_stream_subscriber import TaskStreamSubscriber
from constants import INGESTION_TRIGGER_STREAM


def create_task_subscriber(redis_client, run_graphrag_job_task) -> TaskStreamSubscriber:
    """Create task subscriber wired to the ingestion trigger stream and queue."""

    def build_apply_kwargs(fields: dict) -> dict:
        return {
            "args": [
                fields.get("job_id"),
                fields.get("project_id"),
                int(fields.get("attempts", 0) or 0),
            ],
            "queue": "neo4j_ingestion",
        }

    return TaskStreamSubscriber(
        redis_client=redis_client,
        stream_key=INGESTION_TRIGGER_STREAM,
        consumer_group="neo4j_ingestor",
        consumer_name="worker",
        task=run_graphrag_job_task,
        queue="neo4j_ingestion",
        build_apply_kwargs=build_apply_kwargs,
    )


