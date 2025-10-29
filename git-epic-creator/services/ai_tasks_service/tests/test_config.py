"""Tests for AI Tasks Service configuration."""

import pytest
from config import AITasksSettings, get_ai_tasks_settings


def test_config_defaults():
    """Test default configuration values."""
    settings = AITasksSettings()
    
    assert settings.http.GRAPH_RAG_SERVICE_URL == "http://neo4j-retrieval-service:8000"
    assert settings.http.GITLAB_CLIENT_SERVICE_URL == "http://gitlab-client-service:8000"
    assert settings.CLARIFICATION_SCORE_TARGET == 0.75
    assert settings.SIMILARITY_THRESHOLD == 0.83
    assert settings.MAX_AGENT_ITERS == 3
    assert settings.RETRIEVAL_TOP_K == 2


def test_config_eval_weights():
    """Test evaluation weights are properly initialized."""
    settings = AITasksSettings()
    
    assert "coverage" in settings.EVAL_WEIGHTS
    assert "specificity" in settings.EVAL_WEIGHTS
    assert "feasibility" in settings.EVAL_WEIGHTS
    assert "duplication" in settings.EVAL_WEIGHTS
    
    # Check weights sum to 1.0
    total = sum(settings.EVAL_WEIGHTS.values())
    assert abs(total - 1.0) < 0.01


def test_config_composed_settings():
    """Test that shared settings are properly composed."""
    settings = AITasksSettings()
    
    # Should have llm and redis composed settings
    assert hasattr(settings, "llm")
    assert hasattr(settings, "redis")
    assert hasattr(settings.llm, "OAI_MODEL")
    assert hasattr(settings.redis, "REDIS_URL")


def test_get_ai_tasks_settings_cached():
    """Test that settings are cached."""
    settings1 = get_ai_tasks_settings()
    settings2 = get_ai_tasks_settings()
    
    assert settings1 is settings2


