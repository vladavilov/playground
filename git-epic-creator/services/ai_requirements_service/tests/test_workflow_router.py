from uuid import uuid4

from unittest.mock import AsyncMock

from fastapi.testclient import TestClient

from test_helpers import _FakeOpenAI, _FakeHTTPClient, stub_valuation_axes


class TestWorkflowRouter:
    def setup_method(self):
        # Import after environment is prepared per-test
        import main  # type: ignore  # noqa: F401

    def _override_redis(self, app):
        from utils.app_factory import get_redis_client_from_state

        mock_redis = AsyncMock()
        mock_redis.publish.return_value = 1
        app.dependency_overrides[get_redis_client_from_state] = lambda: mock_redis
        return mock_redis

    def _set_min_env(self):
        import os
        os.environ.setdefault("OAI_API_KEY", "test-key")
        os.environ.setdefault("OAI_BASE_URL", "http://localhost:9999")
        os.environ.setdefault("OAI_MODEL", "test-model")

    def test_requirements_endpoint_returns_bundle_and_publishes_progress(self, monkeypatch):
        import main  # type: ignore

        mock_redis = self._override_redis(main.app)
        client = TestClient(main.app)

        self._set_min_env()

        # Mock only external dependencies (LLM via orchestrator.llm)
        from test_helpers import make_fake_llm
        fake = make_fake_llm()
        monkeypatch.setattr("orchestrator.experts.prompt_analyst.get_llm", lambda *args, **kwargs: fake, raising=False)
        monkeypatch.setattr("orchestrator.experts.requirements_engineer.get_llm", lambda *args, **kwargs: fake, raising=False)
        monkeypatch.setattr("orchestrator.experts.consistency_auditor.get_llm", lambda *args, **kwargs: fake, raising=False)
        monkeypatch.setattr("orchestrator.experts.question_strategist.get_llm", lambda *args, **kwargs: fake, raising=False)
        monkeypatch.setattr("httpx.AsyncClient", _FakeHTTPClient, raising=True)
        stub_valuation_axes(monkeypatch, 0.8)

        req = {
            "project_id": str(uuid4()),
            "prompt": "Design a simple file upload feature",
        }

        resp = client.post("/workflow/requirements", json=req)

        assert resp.status_code == 200
        body = resp.json()
        # Response should be a RequirementsBundle-like structure
        assert "prompt_id" in body and isinstance(body["prompt_id"], str)
        assert body["project_id"] == req["project_id"]
        assert isinstance(body["business_requirements"], list)
        assert isinstance(body["functional_requirements"], list)
        assert isinstance(body["assumptions"], list)
        assert isinstance(body["risks"], list)
        assert isinstance(body["score"], float)

        # At least one progress message should be published to UI channel
        assert mock_redis.publish.await_count >= 1

    def test_answers_endpoint_returns_bundle_and_publishes_progress(self, monkeypatch):
        import main  # type: ignore

        mock_redis = self._override_redis(main.app)
        client = TestClient(main.app)

        self._set_min_env()

        # Mock only external dependencies (LLM via orchestrator.llm)
        from test_helpers import make_fake_llm
        fake = make_fake_llm()
        monkeypatch.setattr("orchestrator.experts.prompt_analyst.get_llm", lambda *args, **kwargs: fake, raising=False)
        monkeypatch.setattr("orchestrator.experts.requirements_engineer.get_llm", lambda *args, **kwargs: fake, raising=False)
        monkeypatch.setattr("orchestrator.experts.consistency_auditor.get_llm", lambda *args, **kwargs: fake, raising=False)
        monkeypatch.setattr("orchestrator.experts.question_strategist.get_llm", lambda *args, **kwargs: fake, raising=False)
        monkeypatch.setattr("httpx.AsyncClient", _FakeHTTPClient, raising=True)
        stub_valuation_axes(monkeypatch, 0.4)

        req = {
            "project_id": str(uuid4()),
            "prompt_id": str(uuid4()),
            "prompt": "Design a simple file upload feature",
            "answers": [
                {"id": "Q1", "answer": "50MB"},
                {"id": "Q2", "answer": "Authenticated users only"},
            ],
        }

        resp = client.post("/workflow/answers", json=req)

        assert resp.status_code == 200
        body = resp.json()
        assert body["project_id"] == req["project_id"]
        assert body["prompt_id"] == req["prompt_id"]
        assert isinstance(body["business_requirements"], list)
        assert isinstance(body["functional_requirements"], list)
        assert isinstance(body["assumptions"], list)
        assert isinstance(body["risks"], list)
        assert isinstance(body["score"], float)

        assert mock_redis.publish.await_count >= 1


