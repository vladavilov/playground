"""Shared Redis Streams constants."""

INGESTION_TRIGGER_STREAM = "ingestion.trigger"
INGESTION_DLQ_STREAM = "ingestion.trigger.deadletter"

TASK_REQUEST_STREAM = "task_streams:document_processing"
DOCUMENT_PROCESSORS_CONSUMER_GROUP = "document_processors"

UI_CHANNEL_PREFIX = "ui"
UI_PROJECT_PROGRESS_NAME = "project_progress"
UI_PROJECT_PROGRESS_CHANNEL = f"{UI_CHANNEL_PREFIX}:{UI_PROJECT_PROGRESS_NAME}"
