from datetime import datetime, timezone
from unittest.mock import Mock, AsyncMock
from uuid import uuid4

from fastapi.testclient import TestClient

from main import app
from models.project_db import Project
from routers.project_router import get_project_service, require_gateway_verified
from types import SimpleNamespace


def test_create_project_accepts_gitlab_repository_urls():
    client = TestClient(app)

    caller = SimpleNamespace(token="svc.jwt.token")
    x_user_id = "user-1"

    created = Project(
        id=uuid4(),
        name="Created",
        description="desc",
        gitlab_repository_urls=["https://gitlab.example.com/group/repo.git"],
        gitlab_backlog_project_ids=[],
        gitlab_backlog_project_urls=[],
        status="active",
        created_by=x_user_id,
        created_at=datetime.now(timezone.utc),
        updated_at=datetime.now(timezone.utc),
    )

    mock_project_service = Mock()
    mock_project_service.create_project = AsyncMock(return_value=created)

    app.dependency_overrides[require_gateway_verified] = lambda: caller
    app.dependency_overrides[get_project_service] = lambda: mock_project_service
    try:
        res = client.post(
            "/projects",
            headers={"x-user-id": x_user_id, "Authorization": "Bearer svc.jwt.token"},
            json={
                "name": "Created",
                "description": "desc",
                "gitlab_repository_urls": ["https://gitlab.example.com/group/repo.git"],
                "gitlab_backlog_project_urls": [],
            },
        )
        assert res.status_code == 201
        body = res.json()
        assert body["name"] == "Created"
        assert body["gitlab_repository_urls"] == ["https://gitlab.example.com/group/repo.git"]
    finally:
        app.dependency_overrides.clear()


def test_list_projects_returns_repository_urls():
    client = TestClient(app)

    caller = SimpleNamespace(token="svc.jwt.token")
    x_user_id = "user-1"

    p = Project(
        id=uuid4(),
        name="P1",
        description=None,
        gitlab_repository_urls=["git@gitlab.example.com:group/repo.git"],
        gitlab_backlog_project_ids=[],
        gitlab_backlog_project_urls=[],
        status="active",
        created_by=x_user_id,
        created_at=datetime.now(timezone.utc),
        updated_at=datetime.now(timezone.utc),
    )

    mock_project_service = Mock()
    mock_project_service.get_projects_by_user_and_roles.return_value = [p]

    app.dependency_overrides[require_gateway_verified] = lambda: caller
    app.dependency_overrides[get_project_service] = lambda: mock_project_service
    try:
        res = client.get("/projects", headers={"x-user-id": x_user_id, "Authorization": "Bearer svc.jwt.token"})
        assert res.status_code == 200
        body = res.json()
        assert len(body) == 1
        assert body[0]["gitlab_repository_urls"] == ["git@gitlab.example.com:group/repo.git"]
    finally:
        app.dependency_overrides.clear()


