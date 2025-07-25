"""
Tests for HTTP client configuration settings.
"""
import pytest
from unittest.mock import patch
from pydantic import ValidationError

from configuration.http_client_config import HTTPClientSettings


class TestHTTPClientSettings:
    """Test cases for HTTPClientSettings configuration."""

    def test_default_values(self):
        """Test that HTTPClientSettings has proper default values."""
        settings = HTTPClientSettings()
        
        assert settings.PROJECT_MANAGEMENT_SERVICE_URL == "http://localhost:8001"
        assert settings.CONNECTION_TIMEOUT == 30.0
        assert settings.READ_TIMEOUT == 60.0
        assert settings.MAX_RETRIES == 3
        assert settings.RETRY_BACKOFF_FACTOR == 2.0
        assert settings.MAX_CONNECTIONS == 100
        assert settings.MAX_KEEPALIVE_CONNECTIONS == 20

    @patch.dict('os.environ', {
        'PROJECT_MANAGEMENT_SERVICE_URL': 'http://project-service:8080',
        'HTTP_CONNECTION_TIMEOUT': '45.0',
        'HTTP_READ_TIMEOUT': '90.0',
        'HTTP_MAX_RETRIES': '5',
        'HTTP_RETRY_BACKOFF_FACTOR': '1.5',
        'HTTP_MAX_CONNECTIONS': '200',
        'HTTP_MAX_KEEPALIVE_CONNECTIONS': '50'
    })
    def test_environment_variable_override(self):
        """Test that environment variables properly override default values."""
        settings = HTTPClientSettings()
        
        assert settings.PROJECT_MANAGEMENT_SERVICE_URL == "http://project-service:8080"
        assert settings.CONNECTION_TIMEOUT == 45.0
        assert settings.READ_TIMEOUT == 90.0
        assert settings.MAX_RETRIES == 5
        assert settings.RETRY_BACKOFF_FACTOR == 1.5
        assert settings.MAX_CONNECTIONS == 200
        assert settings.MAX_KEEPALIVE_CONNECTIONS == 50

    def test_invalid_url_validation(self):
        """Test that invalid URLs raise validation errors."""
        with patch.dict('os.environ', {'PROJECT_MANAGEMENT_SERVICE_URL': 'not-a-url'}):
            with pytest.raises(ValidationError):
                HTTPClientSettings()

    def test_negative_timeout_validation(self):
        """Test that negative timeout values raise validation errors."""
        with patch.dict('os.environ', {'HTTP_CONNECTION_TIMEOUT': '-1.0'}):
            with pytest.raises(ValidationError):
                HTTPClientSettings()

    def test_negative_retry_validation(self):
        """Test that negative retry values raise validation errors."""
        with patch.dict('os.environ', {'HTTP_MAX_RETRIES': '-1'}):
            with pytest.raises(ValidationError):
                HTTPClientSettings()

    def test_zero_connections_validation(self):
        """Test that zero or negative connection values raise validation errors."""
        with patch.dict('os.environ', {'HTTP_MAX_CONNECTIONS': '0'}):
            with pytest.raises(ValidationError):
                HTTPClientSettings()