import os
from unittest.mock import patch

import pytest
from pydantic import ValidationError
from config import Settings


def test_settings_load_from_env():
    """
    Tests that the Settings model correctly loads and parses env variables.
    """
    expected_urls = "http://localhost:8001, http://localhost:8002"
    mock_env = {
        "ADAPTER_URLS": expected_urls,
        "SERVICE_BUS_NAMESPACE": "test-sb",
        "SERVICE_BUS_QUEUE_NAME": "test-q",
    }

    with patch.dict(os.environ, mock_env):
        settings = Settings()
        assert settings.adapter_urls_list == [
            "http://localhost:8001",
            "http://localhost:8002",
        ]
        assert settings.SERVICE_BUS_NAMESPACE == "test-sb"
        assert settings.SERVICE_BUS_QUEUE_NAME == "test-q"


def test_settings_missing_env_raises_error():
    """
    Tests that the Settings model raises ValidationError for missing env variables.
    """
    with patch.dict(os.environ, {}, clear=True):
        with pytest.raises(ValidationError):
            Settings()


def test_fallback_configuration():
    """Test that the configuration correctly detects fallback mode."""
    mock_env = {
        "USE_LOCAL_FALLBACK": "true",
        "SERVICE_BUS_QUEUE_NAME": "test-queue",
        "ADAPTER_URLS": "http://localhost:8001/news"
    }
    
    with patch.dict(os.environ, mock_env, clear=True):
        settings = Settings()
        
        assert not settings.is_azure_available, "Should be in fallback mode"
        assert "localhost" in settings.service_bus_endpoint
        assert settings.USE_LOCAL_FALLBACK is True


def test_azure_available_detection():
    """Test Azure availability detection with proper credentials."""
    mock_env = {
        "SERVICE_BUS_NAMESPACE": "test-namespace",
        "SERVICE_BUS_QUEUE_NAME": "test-queue",
        "ADAPTER_URLS": "http://localhost:8001/news",
        "USE_LOCAL_FALLBACK": "false"
    }
    
    with patch.dict(os.environ, mock_env, clear=True):
        with patch('config.DefaultAzureCredential') as mock_cred:
            mock_cred.return_value.get_token.return_value = "mock-token"
            
            settings = Settings()
            
            assert settings.is_azure_available is True
            assert settings.service_bus_endpoint == "test-namespace.servicebus.windows.net"


def test_azure_unavailable_fallback():
    """Test fallback when Azure credentials are not available."""
    mock_env = {
        "SERVICE_BUS_NAMESPACE": "test-namespace",
        "SERVICE_BUS_QUEUE_NAME": "test-queue",
        "ADAPTER_URLS": "http://localhost:8001/news",
        "USE_LOCAL_FALLBACK": "false"
    }
    
    with patch.dict(os.environ, mock_env, clear=True):
        with patch('config.DefaultAzureCredential') as mock_cred:
            mock_cred.return_value.get_token.side_effect = Exception("No credentials")
            
            settings = Settings()
            
            assert settings.is_azure_available is False
            assert "localhost" in settings.service_bus_endpoint


def test_adapter_urls_parsing():
    """Test that adapter URLs are correctly parsed from comma-separated string."""
    mock_env = {
        "ADAPTER_URLS": "http://adapter1/news,http://adapter2/news, http://adapter3/news ",
        "SERVICE_BUS_NAMESPACE": "test-sb",
        "SERVICE_BUS_QUEUE_NAME": "test-q",
    }
    
    with patch.dict(os.environ, mock_env, clear=True):
        settings = Settings()
        
        assert len(settings.adapter_urls_list) == 3
        assert "http://adapter1/news" in settings.adapter_urls_list
        assert "http://adapter2/news" in settings.adapter_urls_list
        assert "http://adapter3/news" in settings.adapter_urls_list


def test_empty_adapter_urls():
    """Test that empty adapter URLs are handled correctly."""
    mock_env = {
        "ADAPTER_URLS": "",
        "SERVICE_BUS_NAMESPACE": "test-sb",
        "SERVICE_BUS_QUEUE_NAME": "test-q",
    }
    
    with patch.dict(os.environ, mock_env, clear=True):
        settings = Settings()
        
        assert settings.adapter_urls_list == []
