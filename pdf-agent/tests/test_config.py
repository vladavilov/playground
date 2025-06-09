import pytest
from pathlib import Path
from pydantic import ValidationError

from src.config import AppSettings

def test_app_settings_defaults(monkeypatch):
    monkeypatch.delenv("API_PORT", raising=False)
    monkeypatch.delenv("TEMP_FILE_DIR", raising=False)
    monkeypatch.delenv("PROPERTY_GROUPS_CONFIG_PATH", raising=False)
    monkeypatch.delenv("AZURE_OPENAI_LLM_MODEL_NAME", raising=False)
    monkeypatch.delenv("AZURE_OPENAI_EMBEDDING_MODEL_NAME", raising=False)
    
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test_key")
    monkeypatch.setenv("AZURE_OPENAI_CHAT_ENDPOINT", "https://test-chat.openai.azure.com/")
    monkeypatch.setenv("AZURE_OPENAI_EMBEDDING_ENDPOINT", "https://test-embed.openai.azure.com/")
    monkeypatch.setenv("AZURE_OPENAI_EMBEDDING_DEPLOYMENT_NAME", "test-embed-deployment")

    settings = AppSettings()
    assert settings.API_PORT == 8000
    assert settings.TEMP_FILE_DIR == Path("./temp_files")
    assert settings.PROPERTY_GROUPS_CONFIG_PATH == Path("config/property_groups.yaml")
    assert settings.AZURE_OPENAI_API_KEY == "test_key"
    assert settings.AZURE_OPENAI_LLM_MODEL_NAME == "gpt-4o"
    assert settings.AZURE_OPENAI_EMBEDDING_MODEL_NAME == "text-embedding-ada-002"

def test_app_settings_from_env_vars(monkeypatch):
    monkeypatch.setenv("API_PORT", "9000")
    monkeypatch.setenv("TEMP_FILE_DIR", "/var/tmp_test")
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "env_key")
    monkeypatch.setenv("AZURE_OPENAI_API_VERSION", "2024-05-01")
    monkeypatch.setenv("AZURE_OPENAI_CHAT_ENDPOINT", "https://env-chat.openai.azure.com/")
    monkeypatch.setenv("AZURE_OPENAI_LLM_MODEL_NAME", "env_llm")
    monkeypatch.setenv("AZURE_OPENAI_EMBEDDING_ENDPOINT", "https://env-embed.openai.azure.com/")
    monkeypatch.setenv("AZURE_OPENAI_EMBEDDING_DEPLOYMENT_NAME", "env-embed-deployment")
    monkeypatch.setenv("AZURE_OPENAI_EMBEDDING_MODEL_NAME", "env_embed_model")
    monkeypatch.setenv("PROPERTY_GROUPS_CONFIG_PATH", "env_config/props.yaml")

    settings = AppSettings()
    assert settings.API_PORT == 9000
    assert settings.TEMP_FILE_DIR == Path("/var/tmp_test")
    assert settings.AZURE_OPENAI_API_KEY == "env_key"
    assert settings.AZURE_OPENAI_API_VERSION == "2024-05-01"
    assert settings.AZURE_OPENAI_CHAT_ENDPOINT == "https://env-chat.openai.azure.com/"
    assert settings.AZURE_OPENAI_LLM_MODEL_NAME == "env_llm"
    assert settings.AZURE_OPENAI_EMBEDDING_ENDPOINT == "https://env-embed.openai.azure.com/"
    assert settings.AZURE_OPENAI_EMBEDDING_DEPLOYMENT_NAME == "env-embed-deployment"
    assert settings.AZURE_OPENAI_EMBEDDING_MODEL_NAME == "env_embed_model"
    assert settings.PROPERTY_GROUPS_CONFIG_PATH == Path("env_config/props.yaml")

def test_app_settings_missing_required_env_vars(monkeypatch):
    monkeypatch.delenv("AZURE_OPENAI_API_KEY", raising=False)
    monkeypatch.setenv("AZURE_OPENAI_CHAT_ENDPOINT", "https://test.com")
    monkeypatch.setenv("AZURE_OPENAI_EMBEDDING_ENDPOINT", "https://test-embed.com")
    monkeypatch.setenv("AZURE_OPENAI_EMBEDDING_DEPLOYMENT_NAME", "test-embed-deploy")

    with pytest.raises(ValidationError):
        AppSettings()

def test_app_settings_all_required_missing(monkeypatch):
    monkeypatch.delenv("AZURE_OPENAI_API_KEY", raising=False)
    monkeypatch.delenv("AZURE_OPENAI_CHAT_ENDPOINT", raising=False)
    monkeypatch.delenv("AZURE_OPENAI_EMBEDDING_ENDPOINT", raising=False)
    monkeypatch.delenv("AZURE_OPENAI_EMBEDDING_DEPLOYMENT_NAME", raising=False)

    with pytest.raises(ValidationError):
        AppSettings()

    missing_vars = ["AZURE_OPENAI_API_KEY", "AZURE_OPENAI_CHAT_ENDPOINT", "AZURE_OPENAI_EMBEDDING_ENDPOINT", "AZURE_OPENAI_EMBEDDING_DEPLOYMENT_NAME"]
    try:
        AppSettings()
    except ValidationError as excinfo:
        error_locs = [error['loc'] for error in excinfo.errors()]
        for var_name in missing_vars:
            found = any(var_name in loc_tuple for loc_tuple in error_locs)
            assert found, f"ValidationError did not report {var_name} as missing" 