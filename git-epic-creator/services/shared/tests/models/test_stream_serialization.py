from uuid import uuid4

from models import IngestionTriggerMessage


def test_to_stream_fields_serializes_all_fields_to_strings():
    from models.ingestion_messages import to_stream_fields

    pid = uuid4()
    msg = IngestionTriggerMessage(job_id="job-xyz", project_id=pid, attempts=3)

    fields = to_stream_fields(msg)

    assert set(fields.keys()) == {"job_id", "project_id", "attempts"}
    assert isinstance(fields["job_id"], str)
    assert isinstance(fields["project_id"], str)
    assert isinstance(fields["attempts"], str)
    assert fields["job_id"] == "job-xyz"
    assert fields["project_id"] == str(pid)
    assert fields["attempts"] == "3"


def test_stream_constants_values():
    from constants.streams import (
        INGESTION_TRIGGER_STREAM,
        INGESTION_DLQ_STREAM,
    )

    assert INGESTION_TRIGGER_STREAM == "ingestion.trigger"
    assert INGESTION_DLQ_STREAM == "ingestion.trigger.deadletter"


