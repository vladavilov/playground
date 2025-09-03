from uuid import uuid4
import os
import json
from unittest.mock import AsyncMock

import pytest

# Reuse common fakes/utilities
from test_helpers import _FakeHTTPClient, stub_valuation_axes, make_fake_llm

@pytest.fixture(autouse=True)
def setup_env():
    os.environ.setdefault("CLARIFICATION_SCORE_TARGET", "0.45")
    os.environ.setdefault("MAX_AGENT_ITERS", "5")
    os.environ.setdefault("OAI_API_KEY", "test-key")
    os.environ.setdefault("OAI_BASE_URL", "http://localhost:9999")
    os.environ.setdefault("OAI_MODEL", "test-model")


@pytest.fixture()
def mock_external(monkeypatch):
    # Stub LLM used by all experts by patching the imported symbol in each module
    fake = make_fake_llm()
    monkeypatch.setattr("orchestrator.experts.prompt_analyst.get_llm", lambda *args, **kwargs: fake, raising=False)
    monkeypatch.setattr("orchestrator.experts.requirements_engineer.get_llm", lambda *args, **kwargs: fake, raising=False)
    monkeypatch.setattr("orchestrator.experts.consistency_auditor.get_llm", lambda *args, **kwargs: fake, raising=False)
    monkeypatch.setattr("orchestrator.experts.question_strategist.get_llm", lambda *args, **kwargs: fake, raising=False)
    # Stub HTTP client for retrieval
    monkeypatch.setattr("httpx.AsyncClient", _FakeHTTPClient, raising=True)

@pytest.mark.asyncio
async def test_loop_terminates_on_threshold_and_publishes_iterations(mock_external, monkeypatch):
    from config import get_ai_workflow_settings
    get_ai_workflow_settings.cache_clear()

    from orchestrator.orchestrator import run_requirements_workflow
    from services.ai_workflow_status_publisher import AiWorkflowStatusPublisher

    # High axes to exceed threshold
    stub_valuation_axes(monkeypatch, 0.9)

    mock_redis = AsyncMock()
    mock_redis.publish.return_value = 1
    publisher = AiWorkflowStatusPublisher(mock_redis)

    project_id = uuid4()
    prompt = "Design a simple file upload feature"

    bundle = await run_requirements_workflow(
        project_id=project_id,
        prompt=prompt,
        publisher=publisher,
    )

    assert 0.0 <= bundle.score <= 1.0
    settings = get_ai_workflow_settings()
    assert bundle.score >= float(settings.CLARIFICATION_SCORE_TARGET)
    # Success path should not request clarifications
    assert bundle.clarification_questions is None

    assert mock_redis.publish.await_count >= 2
    serialized = [call.args[1] for call in mock_redis.publish.await_args_list]
    parsed = [json.loads(s) for s in serialized]
    assert any(isinstance(m.get("iteration"), int) and m["iteration"] >= 1 for m in parsed)
    assert any(isinstance(m.get("score"), (int, float)) for m in parsed)


@pytest.mark.asyncio
async def test_max_iters_caps_iterations_if_threshold_not_met(mock_external, monkeypatch):
    os.environ["CLARIFICATION_SCORE_TARGET"] = "0.99"
    os.environ["MAX_AGENT_ITERS"] = "3"

    from config import get_ai_workflow_settings
    get_ai_workflow_settings.cache_clear()

    from orchestrator.orchestrator import run_requirements_workflow
    from services.ai_workflow_status_publisher import AiWorkflowStatusPublisher

    # Low axes to force max iters
    stub_valuation_axes(monkeypatch, 0.3)

    mock_redis = AsyncMock()
    mock_redis.publish.return_value = 1
    publisher = AiWorkflowStatusPublisher(mock_redis)

    bundle = await run_requirements_workflow(
        project_id=uuid4(),
        prompt="Feature X",
        publisher=publisher,
    )

    settings = get_ai_workflow_settings()
    assert bundle.score < float(settings.CLARIFICATION_SCORE_TARGET)
    # Clarification path should contain questions
    assert bundle.clarification_questions is not None

    serialized = [call.args[1] for call in mock_redis.publish.await_args_list]
    drafted = [json.loads(s) for s in serialized if json.loads(s).get("stage") == "draft_requirements"]
    assert len(drafted) == int(os.environ["MAX_AGENT_ITERS"])
