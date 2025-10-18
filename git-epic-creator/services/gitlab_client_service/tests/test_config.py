"""Tests for GitLab Client Service configuration."""

import pytest
from config import GitLabClientSettings, get_gitlab_client_settings


def test_gitlab_client_settings_defaults():
    """Test default configuration values."""
    settings = GitLabClientSettings(
        GITLAB_BASE_URL="http://test-gitlab.com"
    )
    
    # GitLab settings
    assert settings.GITLAB_BASE_URL == "http://test-gitlab.com"
    assert settings.HTTP_TIMEOUT_SEC == 30.0
    assert settings.RETRY_MAX_ATTEMPTS == 3
    assert settings.RETRY_BACKOFF_FACTOR == 2.0
    assert settings.DEFAULT_PAGE_SIZE == 100
    
    # Redis settings (from shared RedisSettings)
    assert settings.redis.REDIS_URL == "redis://localhost:6379"
    assert settings.redis.REDIS_DB == 0
    assert settings.redis.REDIS_PASSWORD is None
    
    # LLM/Embedding settings (from shared LlmConfig)
    assert settings.llm.OAI_EMBED_MODEL_NAME == "text-embedding-3-small"
    assert settings.OAI_EMBED_BATCH == 16
    assert settings.OAI_EMBED_CONCURRENCY == 2
    
    # Pub/sub settings
    assert settings.EMBEDDINGS_PUBSUB_PREFIX == "embeddings:projects:"
    
    # Idempotency settings
    assert settings.IDEMPOTENCY_TTL_SECONDS == 86400  # 24 hours


def test_gitlab_client_settings_custom_values(monkeypatch):
    """Test configuration with custom environment values."""
    monkeypatch.setenv("GITLAB_BASE_URL", "https://custom-gitlab.com")
    monkeypatch.setenv("HTTP_TIMEOUT_SEC", "60")
    monkeypatch.setenv("RETRY_MAX_ATTEMPTS", "5")
    monkeypatch.setenv("DEFAULT_PAGE_SIZE", "200")
    monkeypatch.setenv("REDIS_URL", "redis://custom-redis:6379")
    monkeypatch.setenv("REDIS_DB", "2")
    monkeypatch.setenv("OAI_EMBED_BATCH", "32")
    monkeypatch.setenv("IDEMPOTENCY_TTL_SECONDS", "3600")
    
    settings = GitLabClientSettings()
    
    assert settings.GITLAB_BASE_URL == "https://custom-gitlab.com"
    assert settings.HTTP_TIMEOUT_SEC == 60.0
    assert settings.RETRY_MAX_ATTEMPTS == 5
    assert settings.DEFAULT_PAGE_SIZE == 200
    assert settings.redis.REDIS_URL == "redis://custom-redis:6379"
    assert settings.redis.REDIS_DB == 2
    assert settings.OAI_EMBED_BATCH == 32
    assert settings.IDEMPOTENCY_TTL_SECONDS == 3600


def test_get_gitlab_client_settings_cached():
    """Test that settings are cached."""
    settings1 = get_gitlab_client_settings()
    settings2 = get_gitlab_client_settings()
    
    assert settings1 is settings2


def test_embedding_settings():
    """Test embedding-related configuration."""
    settings = GitLabClientSettings(
        GITLAB_BASE_URL="http://test.com",
        OAI_EMBED_BATCH=32,
        OAI_EMBED_CONCURRENCY=4
    )
    
    # LLM settings from shared config
    assert settings.llm.OAI_EMBED_MODEL_NAME == "text-embedding-3-small"
    # GitLab-specific embedding settings
    assert settings.OAI_EMBED_BATCH == 32
    assert settings.OAI_EMBED_CONCURRENCY == 4


def test_redis_pubsub_settings():
    """Test Redis pub/sub configuration."""
    settings = GitLabClientSettings(
        GITLAB_BASE_URL="http://test.com",
        EMBEDDINGS_PUBSUB_PREFIX="custom:prefix:"
    )
    
    assert settings.EMBEDDINGS_PUBSUB_PREFIX == "custom:prefix:"


