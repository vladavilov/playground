import pytest
from pathlib import Path
from pydantic import ValidationError

from src.config import AppSettings

def test_app_settings_defaults(monkeypatch):
    """
    Tests that AppSettings uses the correct default values when
    no environment variables are set for optional fields.
    """
    # Ensure optional env vars are not set
    monkeypatch.delenv("API_PORT", raising=False)
    monkeypatch.delenv("TEMP_FILE_DIR", raising=False)
    monkeypatch.delenv("PROPERTY_GROUPS_CONFIG_PATH", raising=False)
    monkeypatch.delenv("AZURE_OPENAI_LLM_MODEL_NAME", raising=False)
    monkeypatch.delenv("EMBEDDING_MODEL_PATH", raising=False)
    
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test_key")
    monkeypatch.setenv("AZURE_OPENAI_CHAT_ENDPOINT", "https://test-chat.openai.azure.com/")

    settings = AppSettings()
    
    assert settings.API_PORT == 8000
    assert settings.TEMP_FILE_DIR == Path("./temp_files")
    assert settings.PROPERTY_GROUPS_CONFIG_PATH == Path("config/property_groups.yaml")
    assert settings.AZURE_OPENAI_LLM_MODEL_NAME == "gpt-4o"
    assert settings.EMBEDDING_MODEL_PATH == "./embedding_model"
    
    # Assert required fields
    assert settings.AZURE_OPENAI_API_KEY == "test_key"

def test_app_settings_from_env_vars(monkeypatch):
    """
    Tests that AppSettings correctly loads values from environment variables.
    """
    monkeypatch.setenv("API_PORT", "9000")
    monkeypatch.setenv("TEMP_FILE_DIR", "/var/tmp_test")
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "env_key")
    monkeypatch.setenv("AZURE_OPENAI_CHAT_ENDPOINT", "https://env-chat.openai.azure.com/")
    monkeypatch.setenv("AZURE_OPENAI_LLM_MODEL_NAME", "env_llm")
    monkeypatch.setenv("EMBEDDING_MODEL_PATH", "env_embed_path")
    monkeypatch.setenv("PROPERTY_GROUPS_CONFIG_PATH", "env_config/props.yaml")

    settings = AppSettings()
    
    assert settings.API_PORT == 9000
    assert settings.TEMP_FILE_DIR == Path("/var/tmp_test")
    assert settings.AZURE_OPENAI_API_KEY == "env_key"
    assert settings.AZURE_OPENAI_CHAT_ENDPOINT == "https://env-chat.openai.azure.com/"
    assert settings.AZURE_OPENAI_LLM_MODEL_NAME == "env_llm"
    assert settings.EMBEDDING_MODEL_PATH == "env_embed_path"
    assert settings.PROPERTY_GROUPS_CONFIG_PATH == Path("env_config/props.yaml")

def test_app_settings_missing_required_env_vars(monkeypatch):
    """
    Tests that AppSettings raises a ValidationError if required
    environment variables are not set.
    """
    # Unset one required variable
    monkeypatch.delenv("AZURE_OPENAI_API_KEY", raising=False)
    monkeypatch.setenv("AZURE_OPENAI_CHAT_ENDPOINT", "https://test.com")

    with pytest.raises(ValidationError):
        AppSettings()

    # Unset the other required variable
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test_key")
    monkeypatch.delenv("AZURE_OPENAI_CHAT_ENDPOINT", raising=False)
    
    with pytest.raises(ValidationError):
        AppSettings()

    # Unset all required variables
    monkeypatch.delenv("AZURE_OPENAI_API_KEY", raising=False)
    monkeypatch.delenv("AZURE_OPENAI_CHAT_ENDPOINT", raising=False)

    try:
        AppSettings()
        pytest.fail("ValidationError was not raised when required env vars were missing.")
    except ValidationError as excinfo:
        error_locs = [error['loc'][0] for error in excinfo.errors()]
        assert "AZURE_OPENAI_API_KEY" in error_locs
        assert "AZURE_OPENAI_CHAT_ENDPOINT" in error_locs 