"""
Tests for AI Workflow Service FastAPI app and health endpoints.
"""

import os
from types import SimpleNamespace
from unittest.mock import AsyncMock, Mock, patch

from fastapi.testclient import TestClient


class TestAIWorkflowService:
    """Test cases for ai_workflow_service main module."""

    def setup_method(self):
        # Ensure service-specific env vars are set for tests
        os.environ.setdefault("GRAPH_RAG_BASE_URL", "http://localhost:8010")
        os.environ.setdefault("HTTP_READ_TIMEOUT", "90.0")

    def test_routes_exist(self):
        """App should expose base health, redis, and custom graphrag/ready endpoints."""
        # Import after any patches/env
        import main  # type: ignore  # noqa: F401  [[memory:7196169]]

        routes = [route.path for route in main.app.routes]
        assert "/health" in routes
        assert "/health/redis" in routes
        assert "/health/graphrag" in routes
        assert "/health/ready" in routes

    def test_graphrag_health_success(self):
        """GET /health/graphrag returns healthy when upstream responds OK."""
        import main  # type: ignore

        # Patch settings and httpx client used by the endpoint
        with patch("main.get_ai_requirements_settings") as mock_get_settings, \
             patch("httpx.AsyncClient") as mock_async_client:

            mock_get_settings.return_value = SimpleNamespace(
                GRAPH_RAG_BASE_URL="http://example.com",
                http=SimpleNamespace(READ_TIMEOUT=0.2),
            )

            mock_resp = Mock()
            mock_resp.status_code = 200
            mock_cm = Mock()
            mock_cm.__aenter__ = AsyncMock(return_value=mock_cm)
            mock_cm.__aexit__ = AsyncMock(return_value=None)
            mock_cm.request = AsyncMock(return_value=mock_resp)
            mock_async_client.return_value = mock_cm

            client = TestClient(main.app)
            resp = client.get("/health/graphrag")

            assert resp.status_code == 200
            data = resp.json()
            assert data["healthy"] is True
            assert data["status_code"] == 200

    def test_graphrag_health_failure(self):
        """GET /health/graphrag reports unhealthy on exceptions/timeouts."""
        import main  # type: ignore

        with patch("main.get_ai_requirements_settings") as mock_get_settings, \
             patch("httpx.AsyncClient") as mock_async_client:

            mock_get_settings.return_value = SimpleNamespace(
                GRAPH_RAG_BASE_URL="http://bad-host",
                http=SimpleNamespace(READ_TIMEOUT=0.01),
            )

            mock_cm = Mock()
            mock_cm.__aenter__ = AsyncMock(return_value=mock_cm)
            mock_cm.__aexit__ = AsyncMock(return_value=None)
            mock_cm.request = AsyncMock(side_effect=Exception("connect error"))
            mock_async_client.return_value = mock_cm

            client = TestClient(main.app)
            resp = client.get("/health/graphrag")

            assert resp.status_code == 200
            data = resp.json()
            assert data["healthy"] is False
            assert "error" in data

    def test_ready_aggregates_redis_and_graphrag(self):
        """GET /health/ready returns 200 only when both Redis and GraphRAG are healthy."""
        import main  # type: ignore
        from utils.app_factory import get_redis_client_from_state

        client = TestClient(main.app)

        # Healthy case
        with patch("main.get_ai_requirements_settings") as mock_get_settings, \
             patch("httpx.AsyncClient") as mock_async_client:

            # Override redis dependency to a healthy mock
            healthy_redis = AsyncMock()
            healthy_redis.ping.return_value = True
            healthy_redis.info.return_value = {"redis_version": "7.0"}
            main.app.dependency_overrides[get_redis_client_from_state] = lambda: healthy_redis

            mock_get_settings.return_value = SimpleNamespace(
                GRAPH_RAG_BASE_URL="http://example.com",
                http=SimpleNamespace(READ_TIMEOUT=0.2),
            )
            mock_resp = Mock()
            mock_resp.status_code = 200
            mock_cm = Mock()
            mock_cm.__aenter__ = AsyncMock(return_value=mock_cm)
            mock_cm.__aexit__ = AsyncMock(return_value=None)
            mock_cm.request = AsyncMock(return_value=mock_resp)
            mock_async_client.return_value = mock_cm

            ok = client.get("/health/ready")
            assert ok.status_code == 200
            body = ok.json()
            assert body["ready"] is True
            assert body["components"]["redis"]["healthy"] is True
            assert body["components"]["graphrag"]["healthy"] is True

        # Unhealthy GraphRAG -> 503
        with patch("main.get_ai_requirements_settings") as mock_get_settings, \
             patch("httpx.AsyncClient") as mock_async_client:

            unhealthy_redis = AsyncMock()
            unhealthy_redis.ping.return_value = True
            unhealthy_redis.info.return_value = {"redis_version": "7.0"}
            main.app.dependency_overrides[get_redis_client_from_state] = lambda: unhealthy_redis

            mock_get_settings.return_value = SimpleNamespace(
                GRAPH_RAG_BASE_URL="http://bad-host",
                http=SimpleNamespace(READ_TIMEOUT=0.01),
            )
            mock_cm = Mock()
            mock_cm.__aenter__ = AsyncMock(return_value=mock_cm)
            mock_cm.__aexit__ = AsyncMock(return_value=None)
            mock_cm.request = AsyncMock(side_effect=Exception("timeout"))
            mock_async_client.return_value = mock_cm

            bad = client.get("/health/ready")
            assert bad.status_code == 503
            body = bad.json()
            assert body["ready"] is False
            assert body["components"]["graphrag"]["healthy"] is False


