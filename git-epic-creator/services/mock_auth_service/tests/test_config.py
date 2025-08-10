"""
Tests for the configuration module of the mock auth service.
"""
import os
import pytest
from unittest.mock import patch
from pydantic import ValidationError

from config import Settings, settings


class TestSettings:
    """Test cases for Settings configuration class."""

    def test_default_configuration_values(self):
        """Test that default configuration values are set correctly."""
        test_settings = Settings()
        
        assert test_settings.AZURE_AD_TENANT_ID == "e7963c3a-3b3a-43b6-9426-89e433d07e69"
        assert test_settings.AZURE_AD_CLIENT_ID == "a9e304a9-5b6c-4ef7-9b37-23a579a6d7be"
        assert str(test_settings.AZURE_AD_AUTHORITY) == "http://mock-auth-service:8005/"
        assert test_settings.MOCK_AUTH_PRIVATE_KEY == ""
        assert test_settings.MOCK_AUTH_KEY_ID == ""

    def test_environment_variable_override(self):
        """Test that environment variables override default values."""
        test_env = {
            'AZURE_AD_TENANT_ID': 'custom-tenant-id',
            'AZURE_AD_CLIENT_ID': 'custom-client-id',
            'AZURE_AD_AUTHORITY': 'http://custom-authority:9000',
            'MOCK_AUTH_PRIVATE_KEY': 'custom-private-key',
            'MOCK_AUTH_KEY_ID': 'custom-key-id'
        }
        
        with patch.dict(os.environ, test_env):
            test_settings = Settings()
            
            assert test_settings.AZURE_AD_TENANT_ID == 'custom-tenant-id'
            assert test_settings.AZURE_AD_CLIENT_ID == 'custom-client-id'
            assert str(test_settings.AZURE_AD_AUTHORITY) == 'http://custom-authority:9000/'
            assert test_settings.MOCK_AUTH_PRIVATE_KEY == 'custom-private-key'
            assert test_settings.MOCK_AUTH_KEY_ID == 'custom-key-id'

    def test_settings_singleton_instance(self):
        """Test that the settings instance is properly configured."""
        # The settings instance should be importable and have default values
        assert hasattr(settings, 'AZURE_AD_TENANT_ID')
        assert hasattr(settings, 'AZURE_AD_CLIENT_ID')
        assert hasattr(settings, 'AZURE_AD_AUTHORITY')
        assert hasattr(settings, 'MOCK_AUTH_PRIVATE_KEY')
        assert hasattr(settings, 'MOCK_AUTH_KEY_ID')

    def test_configuration_validation_for_empty_tenant_id(self):
        """Test validation fails for empty tenant ID."""
        with patch.dict(os.environ, {'AZURE_AD_TENANT_ID': ''}):
            with pytest.raises(ValidationError) as exc_info:
                Settings()
            
            error_details = str(exc_info.value)
            assert "AZURE_AD_TENANT_ID" in error_details
            assert "String should have at least 1 character" in error_details

    def test_configuration_validation_for_empty_client_id(self):
        """Test validation fails for empty client ID."""
        with patch.dict(os.environ, {'AZURE_AD_CLIENT_ID': ''}):
            with pytest.raises(ValidationError) as exc_info:
                Settings()
            
            error_details = str(exc_info.value)
            assert "AZURE_AD_CLIENT_ID" in error_details
            assert "String should have at least 1 character" in error_details

    def test_configuration_validation_for_invalid_authority_url(self):
        """Test validation fails for invalid authority URL."""
        with patch.dict(os.environ, {'AZURE_AD_AUTHORITY': 'not-a-valid-url'}):
            with pytest.raises(ValidationError) as exc_info:
                Settings()
            
            error_details = str(exc_info.value)
            assert "AZURE_AD_AUTHORITY" in error_details
            assert "Input should be a valid URL" in error_details