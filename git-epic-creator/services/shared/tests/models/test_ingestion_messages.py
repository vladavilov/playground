from uuid import uuid4
from models import IngestionTriggerMessage

def test_ingestion_trigger_message_validation_and_serialization():
    pid = uuid4()
    msg = IngestionTriggerMessage(job_id="job", project_id=pid)
    assert msg.attempts == 0
    as_dict = msg.model_dump()
    assert isinstance(as_dict["project_id"], str)
