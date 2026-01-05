"""Tests for single-task/epic enhancement endpoint."""
from uuid import uuid4

from unittest.mock import AsyncMock
from types import SimpleNamespace

from fastapi.testclient import TestClient


def test_enhance_epic_endpoint_returns_enhanced_item(monkeypatch):
    """Test that /tasks/enhance returns an enhanced epic."""
    import main  # type: ignore

    # Override Redis
    from utils.app_factory import get_redis_client_from_state
    import routers.tasks_router as router_mod
    mock_redis = AsyncMock()
    mock_redis.publish.return_value = 1
    main.app.dependency_overrides[get_redis_client_from_state] = lambda: mock_redis
    main.app.dependency_overrides[router_mod.require_gateway_verified] = lambda: SimpleNamespace(
        sub="api-gateway", token="svc.jwt.token"
    )

    client = TestClient(main.app)

    # Set minimum environment
    import os
    os.environ.setdefault("OAI_API_KEY", "test-key")
    os.environ.setdefault("OAI_BASE_URL", "http://localhost:9999")
    os.environ.setdefault("OAI_MODEL", "test-model")

    # Mock enhancement orchestration (avoid LangGraph/LLM in unit tests)
    async def _fake_run_single_task_enhancement(*args, **kwargs):  # noqa: ANN001, ARG002
        publisher = kwargs.get("publisher")
        if publisher:
            await publisher.publish_enhancement_progress(
                project_id=kwargs["project_id"],
                item_id=kwargs["item_id"],
                status="enhancing_item",
                thought_summary="Enhancing item...",
            )
        return {
            "item_id": "EPIC-001",
            "title": "Enhanced: User Management System",
            "description": "## Objective\nBuild comprehensive user management...",
            "acceptance_criteria": ["Given admin user When creating user Then user is created with ID"],
            "dependencies": [],
        }

    monkeypatch.setattr("routers.tasks_router.run_single_task_enhancement", _fake_run_single_task_enhancement, raising=True)

    project_id = str(uuid4())
    req = {
        "project_id": project_id,
        "item_id": "EPIC-001",
        "item_type": "epic",
        "current_content": {
            "id": "EPIC-001",
            "title": "User Management",
            "description": "Manage users",
            "acceptance_criteria": ["Users can be managed"],
            "dependencies": []
        },
        "parent_epic_content": None
    }

    resp = client.post("/tasks/enhance", json=req)

    assert resp.status_code == 200
    body = resp.json()

    # Response should be an EnhancedTask structure
    assert "item_id" in body
    assert "title" in body
    assert "description" in body
    assert "acceptance_criteria" in body
    assert isinstance(body["acceptance_criteria"], list)
    assert "dependencies" in body

    # At least one progress message should be published
    assert mock_redis.publish.await_count >= 1


def test_enhance_task_endpoint_with_parent_epic(monkeypatch):
    """Test enhancement of a task with parent epic context."""
    import main  # type: ignore

    # Override Redis
    from utils.app_factory import get_redis_client_from_state
    import routers.tasks_router as router_mod
    mock_redis = AsyncMock()
    mock_redis.publish.return_value = 1
    main.app.dependency_overrides[get_redis_client_from_state] = lambda: mock_redis
    main.app.dependency_overrides[router_mod.require_gateway_verified] = lambda: SimpleNamespace(
        sub="api-gateway", token="svc.jwt.token"
    )

    client = TestClient(main.app)

    # Set minimum environment
    import os
    os.environ.setdefault("OAI_API_KEY", "test-key")
    os.environ.setdefault("OAI_BASE_URL", "http://localhost:9999")
    os.environ.setdefault("OAI_MODEL", "test-model")

    async def _fake_run_single_task_enhancement(*args, **kwargs):  # noqa: ANN001, ARG002
        publisher = kwargs.get("publisher")
        if publisher:
            await publisher.publish_enhancement_progress(
                project_id=kwargs["project_id"],
                item_id=kwargs["item_id"],
                status="enhancing_item",
                thought_summary="Enhancing item...",
            )
        return {
            "item_id": "TASK-001",
            "title": "Enhanced: Implement User Login API",
            "description": "## Technical Context\nDevelop REST API for user authentication...",
            "acceptance_criteria": [
                "Given valid credentials When POST /api/login Then return 200 with JWT token",
                "Given invalid credentials When POST /api/login Then return 401",
            ],
            "dependencies": ["TASK-000"],
        }

    monkeypatch.setattr("routers.tasks_router.run_single_task_enhancement", _fake_run_single_task_enhancement, raising=True)

    project_id = str(uuid4())
    req = {
        "project_id": project_id,
        "item_id": "TASK-001",
        "item_type": "task",
        "current_content": {
            "id": "TASK-001",
            "title": "User Login API",
            "description": "Build login endpoint",
            "acceptance_criteria": ["API returns token"],
            "dependencies": []
        },
        "parent_epic_content": {
            "id": "EPIC-001",
            "title": "User Management System",
            "description": "## Objective\nBuild comprehensive user management with authentication, authorization, and profile management.\n\n## Architecture\n[Mermaid diagram showing auth flow]",
            "acceptance_criteria": [
                "Given admin user When managing users Then CRUD operations work",
                "Given user When logging in Then JWT token is returned"
            ]
        }
    }

    resp = client.post("/tasks/enhance", json=req)

    assert resp.status_code == 200
    body = resp.json()

    assert body["item_id"] == "TASK-001"
    assert "title" in body
    assert "description" in body
    assert isinstance(body["acceptance_criteria"], list)
    assert len(body["acceptance_criteria"]) > 0

    # Progress messages should be published
    assert mock_redis.publish.await_count >= 1

