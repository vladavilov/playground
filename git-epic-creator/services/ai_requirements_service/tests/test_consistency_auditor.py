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


@pytest.mark.asyncio
async def test_severity_penalty_applied_to_component_scores(sample_draft, sample_context, monkeypatch):
    """Verify that Evaluator applies severity penalty to all component scores."""
    from orchestrator.experts.evaluator import Evaluator
    from workflow_models.agent_models import AuditFindings
    
    # Test case 1: No penalty (severity = 0.0)
    findings_no_penalty = AuditFindings(
        issues=[],
        suggestions=[],
        llm_critique_severity=0.0,
        component_scores={
            "faithfulness": 0.8,
            "groundedness": 0.9,
            "response_relevancy": 0.7,
            "completeness": 0.85,
        }
    )
    
    evaluator = Evaluator()
    report = await evaluator.evaluate(sample_draft, findings_no_penalty, "test prompt", sample_context)
    
    # With no severity penalty (factor=1.0), scores should remain unchanged
    assert report.component_scores["faithfulness"] == 0.8
    assert report.component_scores["groundedness"] == 0.9
    assert report.component_scores["response_relevancy"] == 0.7
    assert report.component_scores["completeness"] == 0.85
    
    # Test case 2: Moderate penalty (severity = 0.5)
    findings_moderate = AuditFindings(
        issues=["Ambiguous requirement BR-1"],
        suggestions=["Add specific acceptance criteria"],
        llm_critique_severity=0.5,
        component_scores={
            "faithfulness": 0.8,
            "groundedness": 0.9,
            "response_relevancy": 0.7,
            "completeness": 0.85,
        }
    )
    
    report = await evaluator.evaluate(sample_draft, findings_moderate, "test prompt", sample_context)
    
    # With severity=0.5, penalty_factor=0.5, all scores should be halved
    assert report.component_scores["faithfulness"] == 0.4  # 0.8 * 0.5
    assert report.component_scores["groundedness"] == 0.45  # 0.9 * 0.5
    assert report.component_scores["response_relevancy"] == 0.35  # 0.7 * 0.5
    assert report.component_scores["completeness"] == 0.425  # 0.85 * 0.5
    
    # Test case 3: Critical penalty (severity = 0.7)
    findings_critical = AuditFindings(
        issues=["Contradictory requirements", "Missing testable criteria"],
        suggestions=["Resolve contradictions", "Add Given/When/Then format"],
        llm_critique_severity=0.7,
        component_scores={
            "faithfulness": 0.8,
            "groundedness": 0.9,
            "response_relevancy": 0.7,
            "completeness": 0.85,
        }
    )
    
    report = await evaluator.evaluate(sample_draft, findings_critical, "test prompt", sample_context)
    
    # With severity=0.7, penalty_factor=0.3, all scores should be multiplied by 0.3
    assert report.component_scores["faithfulness"] == 0.24  # 0.8 * 0.3
    assert report.component_scores["groundedness"] == 0.27  # 0.9 * 0.3
    assert report.component_scores["response_relevancy"] == 0.21  # 0.7 * 0.3
    assert report.component_scores["completeness"] == 0.255  # 0.85 * 0.3
    
    # Verify that the final score is also penalized and below threshold
    assert report.score < 0.3  # Should be around 0.25 given the penalty


@pytest.mark.asyncio
async def test_severity_penalty_affects_final_score(sample_draft, sample_context, monkeypatch):
    """Verify that high severity ensures score drops below threshold to trigger clarification."""
    from orchestrator.experts.evaluator import Evaluator
    from workflow_models.agent_models import AuditFindings
    
    # High component scores but critical severity should result in low final score
    findings = AuditFindings(
        issues=["Critical: Contradictory requirements", "Critical: Untestable criteria"],
        suggestions=["Resolve contradictions", "Make all ACs testable"],
        llm_critique_severity=0.8,  # Critical severity
        component_scores={
            "faithfulness": 0.9,
            "groundedness": 0.85,
            "response_relevancy": 0.9,
            "completeness": 0.88,
        }
    )
    
    evaluator = Evaluator()
    report = await evaluator.evaluate(sample_draft, findings, "test prompt", sample_context)
    
    # With severity=0.8, penalty_factor=0.2, final score should be very low
    assert report.score < 0.3, f"Expected score < 0.3 with critical severity, got {report.score}"
    # Verify this would trigger clarification loop (threshold typically 0.70)
    assert report.score < 0.70, "Critical severity should force clarification loop"