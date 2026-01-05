"""
Tests for shared Pydantic schemas and model conversions.
"""

from datetime import datetime, timezone
from uuid import uuid4

import pytest
from pydantic import ValidationError

from models.project_rest import ProjectSet, ProjectResponse
from models.project_db import Project


def test_project_set_required_fields_only():
    v = ProjectSet(name="Test Project")
    assert v.name == "Test Project"
    assert v.description is None
    assert v.gitlab_repository_urls is None
    assert v.gitlab_backlog_project_urls is None


def test_project_set_accepts_repository_urls_and_backlog_urls():
    v = ProjectSet(
        name="Full Test Project",
        description="desc",
        gitlab_repository_urls=[
            "https://gitlab.example.com/group/repo.git",
            "git@gitlab.example.com:group/repo.git",
        ],
        gitlab_backlog_project_urls=[
            "https://gitlab.example.com/group/backlog-100",
            "https://gitlab.example.com/group/backlog-200",
        ],
    )
    assert v.gitlab_repository_urls and len(v.gitlab_repository_urls) == 2
    assert v.gitlab_backlog_project_urls and len(v.gitlab_backlog_project_urls) == 2


def test_project_set_repository_urls_rejects_invalid_values():
    with pytest.raises(ValidationError):
        ProjectSet(name="X", gitlab_repository_urls=["not-a-url"])


def test_project_set_backlog_urls_rejects_duplicates():
    with pytest.raises(ValidationError):
        ProjectSet(
            name="X",
            gitlab_backlog_project_urls=[
                "https://gitlab.example.com/group/backlog-100",
                "https://gitlab.example.com/group/backlog-100",
            ],
        )


def test_project_response_from_dict():
    now = datetime.now(timezone.utc)
    payload = {
        "id": str(uuid4()),
        "name": "Test Project",
        "description": "Test description",
        "gitlab_repository_urls": ["https://gitlab.example.com/test.git"],
        "gitlab_backlog_project_ids": ["100"],
        "gitlab_backlog_project_urls": ["https://gitlab.example.com/group/backlog-100"],
        "status": "active",
        "created_by": "user123",
        "created_at": now,
        "updated_at": now,
    }
    response = ProjectResponse(**payload)
    assert str(response.id) == payload["id"]
    assert response.gitlab_repository_urls == payload["gitlab_repository_urls"]


def test_project_response_from_sqlalchemy_model():
    project = Project(
        id=uuid4(),
        name="SQLAlchemy Project",
        description="From SQLAlchemy",
        gitlab_repository_urls=["https://gitlab.example.com/sqlalchemy.git"],
        gitlab_backlog_project_ids=["100"],
        gitlab_backlog_project_urls=["https://gitlab.example.com/group/backlog-100"],
        status="active",
        created_by="user456",
        created_at=datetime.now(timezone.utc),
        updated_at=datetime.now(timezone.utc),
    )
    response = ProjectResponse.model_validate(project)
    assert response.id == project.id
    assert response.gitlab_repository_urls == project.gitlab_repository_urls


