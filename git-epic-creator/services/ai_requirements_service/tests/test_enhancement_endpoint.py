"""Tests for single-requirement enhancement endpoint."""
from uuid import uuid4

from unittest.mock import AsyncMock

from fastapi.testclient import TestClient

from test_helpers import _FakeOpenAI, _FakeHTTPClient


def test_enhance_requirement_endpoint_returns_enhanced_requirement(monkeypatch):
    """Test that /workflow/enhance returns an enhanced requirement."""
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
    from test_helpers import make_fake_llm
    fake_llm = make_fake_llm()
    monkeypatch.setattr("orchestrator.experts.prompt_analyst.get_llm", lambda *args, **kwargs: fake_llm, raising=False)
    monkeypatch.setattr("utils.llm_client_factory.create_llm", lambda *args, **kwargs: fake_llm, raising=False)
    monkeypatch.setattr("httpx.AsyncClient", _FakeHTTPClient, raising=True)

    project_id = str(uuid4())
    req = {
        "project_id": project_id,
        "requirement_id": "BR-001",
        "requirement_type": "business",
        "current_content": {
            "id": "BR-001",
            "title": "User Authentication",
            "description": "Users must be able to log in",
            "acceptance_criteria": ["User can log in with credentials"],
            "priority": "Must"
        }
    }

    resp = client.post("/workflow/enhance", json=req)

    assert resp.status_code == 200
    body = resp.json()

    # Response should be a Requirement-like structure
    assert "id" in body
    assert "title" in body
    assert "description" in body
    assert "acceptance_criteria" in body
    assert isinstance(body["acceptance_criteria"], list)
    assert "priority" in body

    # At least one progress message should be published
    assert mock_redis.publish.await_count >= 1


def test_enhance_requirement_validates_requirement_type(monkeypatch):
    """Test that /workflow/enhance validates requirement_type field."""
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

    project_id = str(uuid4())
    req = {
        "project_id": project_id,
        "requirement_id": "BR-001",
        "requirement_type": "invalid_type",  # Invalid type
        "current_content": {
            "id": "BR-001",
            "title": "User Authentication",
            "description": "Users must be able to log in",
            "acceptance_criteria": ["User can log in with credentials"],
            "priority": "Must"
        }
    }

    resp = client.post("/workflow/enhance", json=req)

    # Should return 422 Unprocessable Entity for invalid data
    assert resp.status_code == 422


def test_enhance_functional_requirement(monkeypatch):
    """Test enhancement of a functional requirement."""
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
    from test_helpers import make_fake_llm
    fake_llm = make_fake_llm()
    monkeypatch.setattr("orchestrator.experts.prompt_analyst.get_llm", lambda *args, **kwargs: fake_llm, raising=False)
    monkeypatch.setattr("utils.llm_client_factory.create_llm", lambda *args, **kwargs: fake_llm, raising=False)
    monkeypatch.setattr("httpx.AsyncClient", _FakeHTTPClient, raising=True)

    project_id = str(uuid4())
    req = {
        "project_id": project_id,
        "requirement_id": "FR-001",
        "requirement_type": "functional",
        "current_content": {
            "id": "FR-001",
            "title": "Login API Endpoint",
            "description": "POST /api/auth/login",
            "acceptance_criteria": ["Returns JWT token on success"],
            "priority": "Must"
        }
    }

    resp = client.post("/workflow/enhance", json=req)

    assert resp.status_code == 200
    body = resp.json()

    assert body["id"] == "FR-001"
    assert "title" in body
    assert "description" in body
    assert isinstance(body["acceptance_criteria"], list)

    # Progress messages should be published
    assert mock_redis.publish.await_count >= 1

