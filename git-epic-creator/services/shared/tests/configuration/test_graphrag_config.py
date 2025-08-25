import os
import pytest

from configuration.graphrag_config import get_graphrag_settings


def clear_cache():
    try:
        get_graphrag_settings.cache_clear()
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
        "OAI_EMBED_MODEL",
        "OAI_BASE_URL",
        "OAI_API_VERSION",
        "RAG_WORKSPACE_ROOT",
        "oai_key",
        "oai_model",
    ]:
        monkeypatch.delenv(key, raising=False)
    yield
    clear_cache()


def test_defaults_when_no_env_vars_set():
    settings = get_graphrag_settings()
    assert settings.OAI_KEY is None
    assert settings.OAI_MODEL == "gpt-4o-mini"
    assert settings.OAI_EMBED_MODEL == "text-embedding-3-small"
    assert settings.OAI_BASE_URL is None
    assert settings.OAI_API_VERSION is None
    assert settings.RAG_WORKSPACE_ROOT == "./graphrag"


def test_env_overrides(monkeypatch):
    monkeypatch.setenv("OAI_KEY", "test-key")
    monkeypatch.setenv("OAI_MODEL", "gpt-4o-mini-2024")
    monkeypatch.setenv("OAI_EMBED_MODEL", "text-embedding-3-large")
    monkeypatch.setenv("OAI_BASE_URL", "http://localhost:8010/v1")
    monkeypatch.setenv("OAI_API_VERSION", "2024-06-01")
    monkeypatch.setenv("RAG_WORKSPACE_ROOT", "/tmp/graphrag")

    clear_cache()
    settings = get_graphrag_settings()

    assert settings.OAI_KEY == "test-key"
    assert settings.OAI_MODEL == "gpt-4o-mini-2024"
    assert settings.OAI_EMBED_MODEL == "text-embedding-3-large"
    assert settings.OAI_BASE_URL == "http://localhost:8010/v1"
    assert settings.OAI_API_VERSION == "2024-06-01"
    assert settings.RAG_WORKSPACE_ROOT == "/tmp/graphrag"


def test_caching_behavior(monkeypatch):
    monkeypatch.setenv("OAI_MODEL", "m1")
    clear_cache()
    first = get_graphrag_settings()
    assert first.OAI_MODEL == "m1"

    # Change env var; cached value should remain until cache_clear
    monkeypatch.setenv("OAI_MODEL", "m2")
    second = get_graphrag_settings()
    assert second.OAI_MODEL == "m1"

    # Clear cache and confirm new value is picked up
    clear_cache()
    third = get_graphrag_settings()
    assert third.OAI_MODEL == "m2"


def test_case_sensitive_env_vars(monkeypatch):
    # Lowercase should not affect due to case_sensitive=True
    monkeypatch.setenv("oai_model", "lowercase-ignored")
    monkeypatch.setenv("OAI_MODEL", "uppercase-used")

    clear_cache()
    settings = get_graphrag_settings()
    assert settings.OAI_MODEL == "uppercase-used"
