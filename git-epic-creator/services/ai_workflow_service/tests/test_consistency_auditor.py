"""Tests for ConsistencyAuditor with DeepEval logging and error handling."""
import os
import pytest
from unittest.mock import patch, MagicMock
from test_helpers import make_fake_llm

from workflow_models.agent_models import DraftRequirements, RetrievedContext, Requirement


@pytest.fixture(autouse=True)
def setup_env():
    os.environ.setdefault("OAI_API_KEY", "test-key")
    os.environ.setdefault("OAI_BASE_URL", "http://localhost:9999")
    os.environ.setdefault("OAI_MODEL", "test-model")


@pytest.fixture
def sample_draft():
    return DraftRequirements(
        business_requirements=[
            Requirement(
                id="BR-1",
                title="Upload files",
                description="Users can upload files",
                acceptance_criteria=["Given a user, When they upload, Then file is stored"],
                priority="Must",
            )
        ],
        functional_requirements=[],
        assumptions=["Storage available"],
        risks=["Quota limits"],
    )


@pytest.fixture
def sample_context():
    return RetrievedContext(
        context_answer="File storage is available",
        key_facts=["Storage supports 100MB files"],
        citations=["doc:123"],
    )


@pytest.mark.asyncio
async def test_audit_logs_before_deepeval_execution(sample_draft, sample_context, monkeypatch):
    """Verify that audit logs before attempting DeepEval metric execution."""
    fake_llm = make_fake_llm()
    monkeypatch.setattr("orchestrator.experts.consistency_auditor.get_llm", lambda *args, **kwargs: fake_llm, raising=False)
    
    from orchestrator.experts.consistency_auditor import ConsistencyAuditor
    
    # Mock structlog logger
    mock_logger = MagicMock()
    with patch("orchestrator.experts.consistency_auditor.logger", mock_logger):
        auditor = ConsistencyAuditor()
        findings = await auditor.audit(sample_draft, sample_context, "test prompt")
    
    # Verify logger was called with deepeval-related messages
    log_calls = [str(call) for call in mock_logger.info.call_args_list]
    assert any("deepeval" in str(call).lower() or "evaluating" in str(call).lower() for call in log_calls), \
        "Expected logging before DeepEval execution"


@pytest.mark.asyncio
async def test_audit_logs_metric_scores_on_success(sample_draft, sample_context, monkeypatch):
    """Verify that successful metric execution logs the scores."""
    fake_llm = make_fake_llm()
    monkeypatch.setattr("orchestrator.experts.consistency_auditor.get_llm", lambda *args, **kwargs: fake_llm, raising=False)
    
    from orchestrator.experts.consistency_auditor import ConsistencyAuditor
    
    # Mock successful DeepEval execution
    mock_logger = MagicMock()
    
    async def mock_evaluate_axes(self, draft, user_prompt, context):
        mock_logger.info("deepeval_metrics_executed", faithfulness=0.8, groundedness=0.7)
        return {"faithfulness": 0.8, "groundedness": 0.7, "response_relevancy": 0.9, "completeness": 0.85}
    
    monkeypatch.setattr(ConsistencyAuditor, "_evaluate_axes", mock_evaluate_axes, raising=False)
    
    with patch("orchestrator.experts.consistency_auditor.logger", mock_logger):
        auditor = ConsistencyAuditor()
        findings = await auditor.audit(sample_draft, sample_context, "test prompt")
    
    # Verify scores were logged
    log_calls = [str(call) for call in mock_logger.info.call_args_list]
    assert any("deepeval_metrics_executed" in str(call) for call in log_calls), \
        "Expected logging of metric scores on success"
    
    # Verify findings contain the scores
    assert findings.component_scores["faithfulness"] == 0.8
    assert findings.component_scores["groundedness"] == 0.7


@pytest.mark.asyncio
async def test_audit_raises_exception_when_deepeval_fails(sample_draft, sample_context, monkeypatch):
    """Verify that DeepEval failures raise exceptions instead of being silently swallowed."""
    fake_llm = make_fake_llm()
    monkeypatch.setattr("orchestrator.experts.consistency_auditor.get_llm", lambda *args, **kwargs: fake_llm, raising=False)
    
    from orchestrator.experts.consistency_auditor import ConsistencyAuditor
    
    # Mock DeepEval import failure
    async def mock_evaluate_axes_with_error(self, draft, user_prompt, context):
        raise RuntimeError("DeepEval initialization failed: missing API key")
    
    monkeypatch.setattr(ConsistencyAuditor, "_evaluate_axes", mock_evaluate_axes_with_error, raising=False)
    
    auditor = ConsistencyAuditor()
    
    # Should raise the exception, not return with score=0
    with pytest.raises(RuntimeError, match="DeepEval initialization failed"):
        await auditor.audit(sample_draft, sample_context, "test prompt")


@pytest.mark.asyncio
async def test_audit_logs_exception_details_before_raising(sample_draft, sample_context, monkeypatch):
    """Verify that exceptions are logged with context before being raised."""
    fake_llm = make_fake_llm()
    monkeypatch.setattr("orchestrator.experts.consistency_auditor.get_llm", lambda *args, **kwargs: fake_llm, raising=False)
    
    from orchestrator.experts.consistency_auditor import ConsistencyAuditor
    
    mock_logger = MagicMock()
    
    # Mock DeepEval metric failure
    async def mock_evaluate_axes_with_error(self, draft, user_prompt, context):
        mock_logger.error("deepeval_metric_failed", error="API timeout", metric="FaithfulnessMetric")
        raise RuntimeError("DeepEval metric execution failed")
    
    monkeypatch.setattr(ConsistencyAuditor, "_evaluate_axes", mock_evaluate_axes_with_error, raising=False)
    
    with patch("orchestrator.experts.consistency_auditor.logger", mock_logger):
        auditor = ConsistencyAuditor()
        
        with pytest.raises(RuntimeError, match="DeepEval metric execution failed"):
            await auditor.audit(sample_draft, sample_context, "test prompt")
    
    # Verify error was logged
    log_calls = [str(call) for call in mock_logger.error.call_args_list]
    assert any("deepeval_metric_failed" in str(call) for call in log_calls), \
        "Expected error logging before exception propagation"


@pytest.mark.asyncio
async def test_audit_configures_custom_azure_openai_model_for_deepeval(sample_draft, sample_context, monkeypatch):
    """Verify that ConsistencyAuditor creates LiteLLMModel for Azure OpenAI with correct configuration."""
    fake_llm = make_fake_llm()
    monkeypatch.setattr("orchestrator.experts.consistency_auditor.get_llm", lambda *args, **kwargs: fake_llm, raising=False)
    
    # Set environment variables for Azure OpenAI mock
    os.environ["OAI_KEY"] = "KEY"
    os.environ["OAI_BASE_URL"] = "http://openai-mock-service:8000"
    os.environ["OAI_MODEL"] = "gpt-4.1"
    os.environ["OAI_API_VERSION"] = "2024-02-15-preview"
    
    from orchestrator.experts.consistency_auditor import ConsistencyAuditor
    
    # Mock LiteLLMModel to verify it's instantiated with correct args
    mock_litellm_model = MagicMock()
    mock_model_instance = MagicMock()
    mock_litellm_model.return_value = mock_model_instance
    
    # Mock DeepEval metrics to capture the model parameter
    mock_faithfulness = MagicMock()
    mock_geval = MagicMock()
    mock_relevancy = MagicMock()
    
    with patch("orchestrator.experts.consistency_auditor.LiteLLMModel", mock_litellm_model):
        with patch("orchestrator.experts.consistency_auditor.FaithfulnessMetric", return_value=mock_faithfulness):
            with patch("orchestrator.experts.consistency_auditor.GEval", return_value=mock_geval):
                with patch("orchestrator.experts.consistency_auditor.AnswerRelevancyMetric", return_value=mock_relevancy):
                    auditor = ConsistencyAuditor()
                    await auditor.audit(sample_draft, sample_context, "test prompt")
    
    # Verify LiteLLMModel was instantiated with Azure OpenAI configuration
    mock_litellm_model.assert_called_once()
    call_kwargs = mock_litellm_model.call_args[1]
    
    assert call_kwargs["model"] == "azure/gpt-4.1", "Model should be prefixed with 'azure/'"
    assert call_kwargs["api_key"] == "KEY", "API key should match OAI_KEY"
    assert call_kwargs["api_base"] == "http://openai-mock-service:8000", "API base should match OAI_BASE_URL"
    assert call_kwargs["api_version"] == "2024-02-15-preview", "API version should match OAI_API_VERSION"
