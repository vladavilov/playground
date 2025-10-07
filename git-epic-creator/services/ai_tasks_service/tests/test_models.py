"""Tests for data models."""

import pytest
from uuid import uuid4
from models.backlog_models import SimilarMatch, Task, Epic
from models.request_models import TasksChatRequest, GeneratedBacklogBundle, ClarificationQuestion
from models.progress_messages import BacklogProgressMessage, BacklogStatus
from models.agent_models import RequirementsAnalysis, BacklogDraft


def test_similar_match_model():
    """Test SimilarMatch model validation."""
    match = SimilarMatch(
        kind="epic",
        id="123",
        status="open",
        similarity=0.85,
        url="https://gitlab.com/epic/123",
    )
    
    assert match.kind == "epic"
    assert match.similarity == 0.85


def test_task_model():
    """Test Task model with acceptance criteria."""
    task = Task(
        id="T1",
        title="Implement OAuth2",
        description="Add OAuth2 authentication",
        acceptance_criteria=[
            "Given user credentials, When login attempted, Then token issued",
        ],
        dependencies=["T0"],
    )
    
    assert task.id == "T1"
    assert len(task.acceptance_criteria) == 1
    assert len(task.dependencies) == 1


def test_epic_model():
    """Test Epic model with tasks."""
    tasks = [
        Task(id="T1", title="Task 1", description="Desc 1", acceptance_criteria=[]),
        Task(id="T2", title="Task 2", description="Desc 2", acceptance_criteria=[]),
    ]
    
    epic = Epic(
        id="E1",
        title="User Authentication",
        description="Complete auth system",
        tasks=tasks,
    )
    
    assert epic.id == "E1"
    assert len(epic.tasks) == 2


def test_tasks_chat_request():
    """Test TasksChatRequest validation."""
    project_id = uuid4()
    
    request = TasksChatRequest(
        project_id=project_id,
        message="Generate backlog for user authentication",
        options={"top_k": 3},
    )
    
    assert request.project_id == project_id
    assert request.prompt_id is None
    assert "top_k" in request.options


def test_generated_backlog_bundle():
    """Test GeneratedBacklogBundle with clarification questions."""
    project_id = uuid4()
    prompt_id = uuid4()
    
    questions = [
        ClarificationQuestion(id="Q1", text="What auth methods?"),
    ]
    
    bundle = GeneratedBacklogBundle(
        prompt_id=prompt_id,
        project_id=project_id,
        epics=[],
        score=0.65,
        clarification_questions=questions,
    )
    
    assert bundle.score == 0.65
    assert len(bundle.clarification_questions) == 1


def test_backlog_progress_message():
    """Test BacklogProgressMessage serialization."""
    project_id = uuid4()
    prompt_id = uuid4()
    
    msg = BacklogProgressMessage(
        project_id=project_id,
        prompt_id=prompt_id,
        status=BacklogStatus.DRAFTING_BACKLOG,
        thought_summary="Drafting epics and tasks",
        score=0.8,
        iteration=2,
    )
    
    # Test serialization
    data = msg.model_dump()
    assert data["status"] == "drafting_backlog"
    assert data["score"] == 0.8
    assert data["iteration"] == 2


def test_requirements_analysis():
    """Test RequirementsAnalysis model."""
    analysis = RequirementsAnalysis(
        requirements_text="Build user auth system",
        intents=["Implement login", "Add OAuth2"],
        entities=["User", "Session"],
        constraints=["GDPR compliance"],
    )
    
    assert len(analysis.intents) == 2
    assert len(analysis.entities) == 2


def test_backlog_draft():
    """Test BacklogDraft model."""
    epic = Epic(id="E1", title="Auth", description="Auth system", tasks=[])
    
    draft = BacklogDraft(
        epics=[epic],
        assumptions=["Users have email"],
        risks=["OAuth provider downtime"],
    )
    
    assert len(draft.epics) == 1
    assert len(draft.assumptions) == 1


