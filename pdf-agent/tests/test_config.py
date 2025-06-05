import pytest
from pathlib import Path # Import Path
from pydantic import ValidationError

from src.config import AppSettings # Import the real AppSettings

def test_app_settings_defaults(monkeypatch):
    # Ensure no relevant env vars are set for this test for default checks
    monkeypatch.delenv("API_PORT", raising=False)
    monkeypatch.delenv("TEMP_FILE_DIR", raising=False)
    monkeypatch.delenv("PROPERTY_GROUPS_CONFIG_PATH", raising=False)
    
    # Required Azure settings must be provided for instantiation
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test_key")
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "test_endpoint")
    monkeypatch.setenv("AZURE_OPENAI_LLM_DEPLOYMENT_NAME", "test_llm")
    monkeypatch.setenv("AZURE_OPENAI_EMBEDDINGS_DEPLOYMENT_NAME", "test_embeddings")

    settings = AppSettings()
    assert settings.API_PORT == 8000
    assert settings.TEMP_FILE_DIR == Path("./temp_files") # Compare with Path
    assert settings.PROPERTY_GROUPS_CONFIG_PATH == Path("config/property_groups.yaml") # Compare with Path
    assert settings.AZURE_OPENAI_API_KEY == "test_key"

def test_app_settings_from_env_vars(monkeypatch):
    monkeypatch.setenv("API_PORT", "9000")
    monkeypatch.setenv("TEMP_FILE_DIR", "/var/tmp_test")
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "env_key")
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "env_endpoint")
    monkeypatch.setenv("AZURE_OPENAI_LLM_DEPLOYMENT_NAME", "env_llm")
    monkeypatch.setenv("AZURE_OPENAI_EMBEDDINGS_DEPLOYMENT_NAME", "env_embeddings")
    monkeypatch.setenv("PROPERTY_GROUPS_CONFIG_PATH", "env_config/props.yaml")

    settings = AppSettings()
    assert settings.API_PORT == 9000
    assert settings.TEMP_FILE_DIR == Path("/var/tmp_test") # Compare with Path
    assert settings.AZURE_OPENAI_API_KEY == "env_key"
    assert settings.AZURE_OPENAI_ENDPOINT == "env_endpoint"
    assert settings.AZURE_OPENAI_LLM_DEPLOYMENT_NAME == "env_llm"
    assert settings.AZURE_OPENAI_EMBEDDINGS_DEPLOYMENT_NAME == "env_embeddings"
    assert settings.PROPERTY_GROUPS_CONFIG_PATH == Path("env_config/props.yaml") # Compare with Path

def test_app_settings_missing_required_env_vars(monkeypatch):
    # Clear only one of the required Azure settings
    monkeypatch.delenv("AZURE_OPENAI_API_KEY", raising=False)
    # Set others to satisfy instantiation if it were to pass this point
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "test_endpoint")
    monkeypatch.setenv("AZURE_OPENAI_LLM_DEPLOYMENT_NAME", "test_llm")
    monkeypatch.setenv("AZURE_OPENAI_EMBEDDINGS_DEPLOYMENT_NAME", "test_embeddings")

    with pytest.raises(ValidationError) as excinfo:
        AppSettings() # This should fail due to missing AZURE_OPENAI_API_KEY
    
    # Check that the error details mention the missing key
    error_found = False
    for error in excinfo.value.errors():
        if 'AZURE_OPENAI_API_KEY' in error['loc']:
            error_found = True
            break
    assert error_found, "ValidationError did not report AZURE_OPENAI_API_KEY as missing"

def test_app_settings_all_required_missing(monkeypatch):
    monkeypatch.delenv("AZURE_OPENAI_API_KEY", raising=False)
    monkeypatch.delenv("AZURE_OPENAI_ENDPOINT", raising=False)
    monkeypatch.delenv("AZURE_OPENAI_LLM_DEPLOYMENT_NAME", raising=False)
    monkeypatch.delenv("AZURE_OPENAI_EMBEDDINGS_DEPLOYMENT_NAME", raising=False)

    with pytest.raises(ValidationError) as excinfo:
        AppSettings()

    missing_vars = ["AZURE_OPENAI_API_KEY", "AZURE_OPENAI_ENDPOINT", "AZURE_OPENAI_LLM_DEPLOYMENT_NAME", "AZURE_OPENAI_EMBEDDINGS_DEPLOYMENT_NAME"]
    error_locs = [error['loc'] for error in excinfo.value.errors()]
    
    for var_name in missing_vars:
        found = any(var_name in loc_tuple for loc_tuple in error_locs)
        assert found, f"ValidationError did not report {var_name} as missing" 