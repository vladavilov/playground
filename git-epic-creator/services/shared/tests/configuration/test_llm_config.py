import os
import pytest

from configuration.llm_config import get_llm_config


def clear_cache():
    try:
        get_llm_config.cache_clear()
    except Exception:
        pass


@pytest.fixture(autouse=True)
def _reset_cache_and_env(monkeypatch):
    # Ensure clean cache and environment for each test
    clear_cache()
    # Remove relevant env vars if present
    for key in [
        "OAI_KEY",
        "OAI_MODEL",
        "OAI_EMBED_MODEL_NAME",
        "OAI_EMBED_DEPLOYMENT_NAME",
        "OAI_BASE_URL",
        "OAI_API_VERSION",
        "oai_key",
        "oai_model",
    ]:
        monkeypatch.delenv(key, raising=False)
    yield
    clear_cache()


def test_defaults_when_no_env_vars_set():
    settings = get_llm_config()
    assert settings.OAI_KEY is None
    assert settings.OAI_MODEL == "gpt-4o-mini"
    assert settings.OAI_EMBED_MODEL_NAME == "text-embedding-3-small"
    assert settings.OAI_EMBED_DEPLOYMENT_NAME is None
    assert settings.embedding_deployment_name == "text-embedding-3-small"  # Falls back to model name
    assert settings.OAI_BASE_URL is None
    assert settings.OAI_API_VERSION is None


def test_env_overrides(monkeypatch):
    monkeypatch.setenv("OAI_KEY", "test-key")
    monkeypatch.setenv("OAI_MODEL", "gpt-4o-mini-2024")
    monkeypatch.setenv("OAI_EMBED_MODEL_NAME", "text-embedding-3-large")
    monkeypatch.setenv("OAI_EMBED_DEPLOYMENT_NAME", "custom-embedding-deployment")
    monkeypatch.setenv("OAI_BASE_URL", "http://localhost:8010/v1")
    monkeypatch.setenv("OAI_API_VERSION", "2024-06-01")

    clear_cache()
    settings = get_llm_config()

    assert settings.OAI_KEY == "test-key"
    assert settings.OAI_MODEL == "gpt-4o-mini-2024"
    assert settings.OAI_EMBED_MODEL_NAME == "text-embedding-3-large"
    assert settings.OAI_EMBED_DEPLOYMENT_NAME == "custom-embedding-deployment"
    assert settings.embedding_deployment_name == "custom-embedding-deployment"
    assert settings.OAI_BASE_URL == "http://localhost:8010/v1"
    assert settings.OAI_API_VERSION == "2024-06-01"


def test_caching_behavior(monkeypatch):
    monkeypatch.setenv("OAI_MODEL", "m1")
    clear_cache()
    first = get_llm_config()
    assert first.OAI_MODEL == "m1"

    # Change env var; cached value should remain until cache_clear
    monkeypatch.setenv("OAI_MODEL", "m2")
    second = get_llm_config()
    assert second.OAI_MODEL == "m1"

    # Clear cache and confirm new value is picked up
    clear_cache()
    third = get_llm_config()
    assert third.OAI_MODEL == "m2"


def test_case_sensitive_env_vars(monkeypatch):
    # Lowercase should not affect due to case_sensitive=True
    monkeypatch.setenv("oai_model", "lowercase-ignored")
    monkeypatch.setenv("OAI_MODEL", "uppercase-used")

    clear_cache()
    settings = get_llm_config()
    assert settings.OAI_MODEL == "uppercase-used"


def test_embedding_deployment_fallback(monkeypatch):
    # When deployment name is not set, should fall back to model name
    monkeypatch.setenv("OAI_EMBED_MODEL_NAME", "text-embedding-ada-002")
    
    clear_cache()
    settings = get_llm_config()
    assert settings.OAI_EMBED_MODEL_NAME == "text-embedding-ada-002"
    assert settings.OAI_EMBED_DEPLOYMENT_NAME is None
    assert settings.embedding_deployment_name == "text-embedding-ada-002"


def test_embedding_deployment_explicit(monkeypatch):
    # When deployment name is explicitly set, should use it
    monkeypatch.setenv("OAI_EMBED_MODEL_NAME", "text-embedding-3-small")
    monkeypatch.setenv("OAI_EMBED_DEPLOYMENT_NAME", "my-custom-deployment")
    
    clear_cache()
    settings = get_llm_config()
    assert settings.OAI_EMBED_MODEL_NAME == "text-embedding-3-small"
    assert settings.OAI_EMBED_DEPLOYMENT_NAME == "my-custom-deployment"
    assert settings.embedding_deployment_name == "my-custom-deployment"
