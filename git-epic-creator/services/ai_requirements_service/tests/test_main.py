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
        os.environ.setdefault("GRAPH_RAG_SERVICE_URL", "http://localhost:8010")
        os.environ.setdefault("HTTP_READ_TIMEOUT", "90.0")

    def test_routes_exist(self):
        """App should expose base health and redis health endpoints."""
        # Import after any patches/env
        import main  # type: ignore  # noqa: F401  [[memory:7196169]]

        routes = [route.path for route in main.app.routes]
        assert "/health" in routes
        assert "/health/redis" in routes
        # GraphRAG/ready endpoints are not exposed by this service.

    # NOTE: GraphRAG and aggregated readiness endpoints are not part of this service;
    # tests previously asserting /health/graphrag and /health/ready were removed.


