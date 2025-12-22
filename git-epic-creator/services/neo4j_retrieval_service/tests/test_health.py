import sys
from pathlib import Path
import types
from typing import Any, Dict

import importlib
import pytest
from fastapi import FastAPI
from fastapi.testclient import TestClient

# Ensure `src/` is importable without external pytest plugins.
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))


class _FakeNeo4jDriver:
    def __init__(self):
        self.closed = False

    def session(self, **kwargs):
        return _FakeNeo4jSession()

    def close(self):
        self.closed = True


class _FakeNeo4jSession:
    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        return False

    def run(self, query: str, **params: Any):
        if "RETURN 1" in query:
            return [types.SimpleNamespace(value=lambda: 1)]
        return []


class _FakeHttpClient:
    def __init__(self, *args, **kwargs):
        pass

    def request(self, method: str, url: str):
        return types.SimpleNamespace(status_code=200)

    async def aclose(self):
        return None


def _mount_main_app():
    # `pyproject.toml` config sets pythonpath=["src"], so `main` is importable directly.
    return importlib.import_module("main")


@pytest.fixture(autouse=True)
def _env(monkeypatch):
    monkeypatch.setenv("OAI_BASE_URL", "http://mock:8000/v1")
    monkeypatch.setenv("OAI_KEY", "key")
    monkeypatch.setenv("NEO4J_URI", "bolt://mock:7687")
    monkeypatch.setenv("NEO4J_USERNAME", "neo4j")
    monkeypatch.setenv("NEO4J_PASSWORD", "pass")
    monkeypatch.setenv("NEO4J_DATABASE", "neo4j")
    yield


def test_health_endpoints_report_ready(monkeypatch):
    main_mod = _mount_main_app()

    # Patch internals used by health endpoints
    # httpx AsyncClient.request
    import httpx
    from unittest.mock import AsyncMock
    from utils.app_factory import get_redis_client_from_state

    async def _fake_request(self, method: str, url: str):
        return types.SimpleNamespace(status_code=200)

    monkeypatch.setattr(httpx.AsyncClient, "request", _fake_request, raising=True)

    # Override redis dependency to avoid real Redis in tests.
    healthy_redis = AsyncMock()
    healthy_redis.ping.return_value = True
    main_mod.app.dependency_overrides[get_redis_client_from_state] = lambda: healthy_redis

    app = main_mod.app if isinstance(main_mod.app, FastAPI) else None
    assert app is not None
    client = TestClient(app)

    r1 = client.get("/health/llm")
    assert r1.status_code == 200
    body1: Dict[str, Any] = r1.json()
    assert body1.get("healthy") is True

    # readiness aggregates components; since both mocked healthy, expect 200 and ready true
    r2 = client.get("/health/ready")
    assert r2.status_code == 200
    body2: Dict[str, Any] = r2.json()
    assert "components" in body2
    assert body2.get("ready") is True


