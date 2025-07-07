import pytest
from unittest.mock import patch, MagicMock
import os

from config import Settings, AzureAppConfigurationSource

@pytest.fixture
def mock_env_vars():
    """Fixture to set and unset environment variables for tests."""
    with patch.dict(os.environ, {
        "AZURE_APPCONFIG_ENDPOINT": "https://fake-app-config.azconfig.io",
        "BENZINGA_API_TOKEN": "env_token",
        "API_PORT": "8001"
    }, clear=True) as patched_dict:
        yield patched_dict

@pytest.fixture
def temp_env_file(tmp_path):
    """Fixture to create a temporary .env file."""
    env_content = """
BENZINGA_API_TOKEN=dotenv_token
API_PORT=8002
"""
    env_file = tmp_path / ".env"
    env_file.write_text(env_content)
    return env_file

def test_azure_app_config_source_success(mock_env_vars):
    """
    Tests that AzureAppConfigurationSource successfully loads config
    when endpoint is available.
    """
    mock_azure_config = {
        "BENZINGA_API_TOKEN": "azure_token",
        "API_PORT": "8000"
    }
    with patch('config.load', return_value=mock_azure_config) as mock_load, \
         patch('config.DefaultAzureCredential', MagicMock()):
        
        source = AzureAppConfigurationSource(Settings)
        
        mock_load.assert_called_once()
        assert source() == mock_azure_config

def test_azure_app_config_source_no_endpoint():
    """
    Tests that AzureAppConfigurationSource returns empty dict
    when endpoint is not set.
    """
    with patch.dict(os.environ, {}, clear=True):
        source = AzureAppConfigurationSource(Settings)
        assert source() == {}

def test_azure_app_config_source_exception(mock_env_vars):
    """
    Tests that AzureAppConfigurationSource returns empty dict
    when an exception occurs during loading.
    """
    with patch('config.load', side_effect=Exception("Connection Error")) as mock_load, \
         patch('config.DefaultAzureCredential', MagicMock()):
        
        source = AzureAppConfigurationSource(Settings)
        
        mock_load.assert_called_once()
        assert source() == {}

def test_settings_priority(mock_env_vars, temp_env_file):
    """
    Tests the configuration source priority: .env > env vars > Azure App Config.
    """
    mock_azure_config = {
        "LOG_LEVEL": "DEBUG",
        "BENZINGA_API_TOKEN": "azure_token",
        "API_PORT": "8000"
    }
    with patch('config.load', return_value=mock_azure_config), \
         patch('config.DefaultAzureCredential', MagicMock()):

        # Instantiate the Settings class, providing the temp .env file path
        # to override the default.
        final_settings = Settings(_env_file=str(temp_env_file))

        # Assert that the highest-priority settings were loaded:
        # 1. .env file
        assert final_settings.BENZINGA_API_TOKEN == "dotenv_token"
        assert final_settings.API_PORT == 8002
        # 2. Azure (for values not in .env or env vars)
        assert final_settings.LOG_LEVEL == "DEBUG"
        # 3. Environment variables (for values not in .env file)
        assert final_settings.AZURE_APPCONFIG_ENDPOINT == "https://fake-app-config.azconfig.io" 