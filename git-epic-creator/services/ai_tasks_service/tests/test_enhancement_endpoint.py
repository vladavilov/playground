"""Tests for single-task/epic enhancement endpoint."""
from uuid import uuid4

from unittest.mock import AsyncMock

from fastapi.testclient import TestClient


def test_enhance_epic_endpoint_returns_enhanced_item(monkeypatch):
    """Test that /tasks/enhance returns an enhanced epic."""
    import main  # type: ignore

    # Override Redis
    from utils.app_factory import get_redis_client_from_state
    mock_redis = AsyncMock()
    mock_redis.publish.return_value = 1
    main.app.dependency_overrides[get_redis_client_from_state] = lambda: mock_redis

    client = TestClient(main.app)

    # Set minimum environment
    import os
    os.environ.setdefault("OAI_API_KEY", "test-key")
    os.environ.setdefault("OAI_BASE_URL", "http://localhost:9999")
    os.environ.setdefault("OAI_MODEL", "test-model")

    # Mock LLM and HTTP client
    from unittest.mock import MagicMock
    from pydantic import BaseModel, Field
    from typing import List

    class EnhancedOut(BaseModel):
        id: str = Field(..., description="Item ID")
        title: str = Field(..., description="Enhanced title")
        description: str = Field(..., description="Enhanced description")
        acceptance_criteria: List[str] = Field(default_factory=list)
        dependencies: List[str] = Field(default_factory=list)

    fake_output = EnhancedOut(
        id="EPIC-001",
        title="Enhanced: User Management System",
        description="## Objective\nBuild comprehensive user management...",
        acceptance_criteria=["Given admin user When creating user Then user is created with ID"],
        dependencies=[]
    )

    fake_llm = MagicMock()
    fake_chain = MagicMock()
    fake_chain.ainvoke = AsyncMock(return_value=fake_output)
    fake_llm.with_structured_output.return_value = fake_chain

    # Mock GraphRAG client
    fake_http_client = MagicMock()
    fake_http_response = MagicMock()
    fake_http_response.status_code = 200
    fake_http_response.json.return_value = {
        "result": {
            "context": "Relevant context...",
            "key_facts": ["Fact 1"],
            "citations": []
        }
    }
    fake_http_client.post = AsyncMock(return_value=fake_http_response)
    fake_http_client.__aenter__ = AsyncMock(return_value=fake_http_client)
    fake_http_client.__aexit__ = AsyncMock(return_value=None)

    monkeypatch.setattr("orchestrator.experts.clients.llm.get_llm", lambda *args, **kwargs: fake_llm, raising=False)
    monkeypatch.setattr("httpx.AsyncClient", lambda *args, **kwargs: fake_http_client, raising=True)

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
    mock_redis = AsyncMock()
    mock_redis.publish.return_value = 1
    main.app.dependency_overrides[get_redis_client_from_state] = lambda: mock_redis

    client = TestClient(main.app)

    # Set minimum environment
    import os
    os.environ.setdefault("OAI_API_KEY", "test-key")
    os.environ.setdefault("OAI_BASE_URL", "http://localhost:9999")
    os.environ.setdefault("OAI_MODEL", "test-model")

    # Mock LLM and HTTP client
    from unittest.mock import MagicMock
    from pydantic import BaseModel, Field
    from typing import List

    class EnhancedOut(BaseModel):
        id: str = Field(..., description="Item ID")
        title: str = Field(..., description="Enhanced title")
        description: str = Field(..., description="Enhanced description")
        acceptance_criteria: List[str] = Field(default_factory=list)
        dependencies: List[str] = Field(default_factory=list)

    fake_output = EnhancedOut(
        id="TASK-001",
        title="Enhanced: Implement User Login API",
        description="## Technical Context\nDevelop REST API for user authentication...",
        acceptance_criteria=[
            "Given valid credentials When POST /api/login Then return 200 with JWT token",
            "Given invalid credentials When POST /api/login Then return 401"
        ],
        dependencies=["TASK-000"]
    )

    fake_llm = MagicMock()
    fake_chain = MagicMock()
    fake_chain.ainvoke = AsyncMock(return_value=fake_output)
    fake_llm.with_structured_output.return_value = fake_chain

    # Mock GraphRAG client
    fake_http_client = MagicMock()
    fake_http_response = MagicMock()
    fake_http_response.status_code = 200
    fake_http_response.json.return_value = {
        "result": {
            "context": "Technical context...",
            "key_facts": ["Use JWT for tokens"],
            "citations": []
        }
    }
    fake_http_client.post = AsyncMock(return_value=fake_http_response)
    fake_http_client.__aenter__ = AsyncMock(return_value=fake_http_client)
    fake_http_client.__aexit__ = AsyncMock(return_value=None)

    monkeypatch.setattr("orchestrator.experts.clients.llm.get_llm", lambda *args, **kwargs: fake_llm, raising=False)
    monkeypatch.setattr("httpx.AsyncClient", lambda *args, **kwargs: fake_http_client, raising=True)

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

