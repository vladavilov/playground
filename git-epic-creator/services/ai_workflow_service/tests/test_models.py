from uuid import uuid4
from datetime import datetime
import pytest


def test_requirements_bundle_serialization_and_validation():
    from workflow_models.workflow_models import (
        Requirement,
        ClarificationQuestion,
        QuestionAnswer,
        RequirementsBundle,
    )
    prompt_id = uuid4()
    project_id = uuid4()

    br = Requirement(
        id="BR-1",
        title="User can upload files",
        description="Allow uploading PDF files",
        acceptance_criteria=[
            "Given a PDF file, when uploaded, then it is stored",
        ],
        priority="Must",
    )
    fr = Requirement(
        id="FR-1",
        title="Validate file type",
        description="Reject non-PDF files",
        acceptance_criteria=[
            "Given a DOCX file, when uploaded, then it is rejected",
        ],
        priority="Should",
    )

    cq = ClarificationQuestion(
        id="Q1",
        text="Which max file size?",
        expected_impact="Ensures feasibility and ACs for size limits",
        axis="completeness",
        priority=1,
        expected_score_gain=0.1,
        targets=["BR-1"],
        options=["10MB", "50MB"],
    )

    qa = QuestionAnswer(id="Q1", answer="50MB")

    bundle = RequirementsBundle(
        prompt_id=prompt_id,
        project_id=project_id,
        business_requirements=[br],
        functional_requirements=[fr],
        assumptions=["User has account"],
        risks=["Large files may slow processing"],
        score=0.8,
        clarification_questions=[cq],
    )

    dumped = bundle.model_dump()
    # UUIDs should serialize to strings
    assert isinstance(dumped["prompt_id"], str)
    assert isinstance(dumped["project_id"], str)
    # Score preserved
    assert dumped["score"] == 0.8
    # Nested models serialized
    assert dumped["business_requirements"][0]["id"] == "BR-1"
    assert dumped["functional_requirements"][0]["id"] == "FR-1"
    assert dumped["clarification_questions"][0]["id"] == "Q1"

    # Score must be in [0,1]
    with pytest.raises(Exception):
        RequirementsBundle(
            prompt_id=prompt_id,
            project_id=project_id,
            business_requirements=[br],
            functional_requirements=[fr],
            assumptions=[],
            risks=[],
            score=1.2,
        )


def test_workflow_progress_message_defaults_and_validation():
    from workflow_models.progress_messages import WorkflowProgressMessage

    project_id = uuid4()
    msg = WorkflowProgressMessage(
        project_id=project_id,
        status="evaluating",
        thought_summary="Scoring draft against retrieved context; gaps in completeness.",
        score=0.62,
    )

    assert msg.message_type == "ai_workflow_progress"
    # visibility removed; all messages are user-visible by default
    assert msg.message_id is not None
    assert isinstance(msg.timestamp, datetime)

    dumped = msg.model_dump()
    # UUIDs and datetimes should serialize to strings
    assert isinstance(dumped["project_id"], str)
    assert isinstance(dumped["message_id"], str)
    assert isinstance(dumped["timestamp"], str)
    assert dumped["status"] == "evaluating"

    # iteration must be >= 1 if provided
    with pytest.raises(Exception):
        WorkflowProgressMessage(
            project_id=project_id,
            status="evaluating",
            thought_summary="",
            iteration=0,
        )

    # score in [0,1]
    with pytest.raises(Exception):
        WorkflowProgressMessage(
            project_id=project_id,
            status="evaluating",
            thought_summary="",
            score=-0.1,
        )



