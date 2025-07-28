"""
Tests for Azure authentication configuration.
"""

import pytest
from unittest.mock import patch
from pydantic import ValidationError
from configuration.azure_auth_config import AzureAuthSettings


class TestAzureAuthSettings:
    """Test cases for Azure authentication settings validation."""

    def test_valid_tenant_id_validation(self):
        """Test that valid tenant ID passes validation."""
        with patch.dict('os.environ', {
            'AZURE_TENANT_ID': '12345678-1234-1234-1234-123456789abc',
            'AZURE_CLIENT_ID': 'test-client-id',
        }):
            settings = AzureAuthSettings()
            assert settings.AZURE_TENANT_ID == '12345678-1234-1234-1234-123456789abc'

    def test_empty_tenant_id_raises_error(self):
        """Test that empty tenant ID raises validation error in production mode."""
        with patch.dict('os.environ', {
            'AZURE_TENANT_ID': '',
            'AZURE_CLIENT_ID': 'test-client-id'
        }, clear=True):
            with pytest.raises(ValidationError, match="AZURE_TENANT_ID is required"):
                AzureAuthSettings()

    def test_invalid_tenant_id_format_raises_error(self):
        """Test that invalid tenant ID format raises validation error."""
        with patch.dict('os.environ', {
            'AZURE_TENANT_ID': 'invalid-tenant-id',
            'AZURE_CLIENT_ID': 'test-client-id',
        }):
            with pytest.raises(ValidationError, match="not a valid UUID format"):
                AzureAuthSettings()

    def test_empty_client_id_raises_error(self):
        """Test that empty client ID raises validation error."""
        with patch.dict('os.environ', {
            'AZURE_TENANT_ID': '12345678-1234-1234-1234-123456789abc',
            'AZURE_CLIENT_ID': '',
        }, clear=True):
            with pytest.raises(ValidationError, match="AZURE_CLIENT_ID is required"):
                AzureAuthSettings()

    def test_custom_openid_config_url_field_not_exists(self):
        """Test that CUSTOM_OPENID_CONFIG_URL field should not exist after refactoring."""
        with patch.dict('os.environ', {
            'AZURE_TENANT_ID': '12345678-1234-1234-1234-123456789abc',
            'AZURE_CLIENT_ID': 'test-client-id',
        }):
            settings = AzureAuthSettings()
            # Should not have CUSTOM_OPENID_CONFIG_URL field
            assert not hasattr(settings, 'CUSTOM_OPENID_CONFIG_URL')

    def test_openid_config_url_uses_only_authority_and_tenant(self):
        """Test that OPENID_CONFIG_URL only uses AZURE_AD_AUTHORITY and AZURE_TENANT_ID."""
        with patch.dict('os.environ', {
            'AZURE_TENANT_ID': '12345678-1234-1234-1234-123456789abc',
            'AZURE_CLIENT_ID': 'test-client-id',
            'AZURE_AD_AUTHORITY': 'https://login.microsoftonline.com',
        }):
            settings = AzureAuthSettings()
            expected_url = "https://login.microsoftonline.com/12345678-1234-1234-1234-123456789abc/v2.0/.well-known/openid-configuration"
            assert settings.OPENID_CONFIG_URL == expected_url

    def test_openid_config_url_with_custom_authority(self):
        """Test that OPENID_CONFIG_URL works with custom AZURE_AD_AUTHORITY."""
        with patch.dict('os.environ', {
            'AZURE_TENANT_ID': '12345678-1234-1234-1234-123456789abc',
            'AZURE_CLIENT_ID': 'test-client-id',
            'AZURE_AD_AUTHORITY': 'https://login.mock.us',  # US Government cloud
        }):
            settings = AzureAuthSettings()
            expected_url = "https://login.mock.us/12345678-1234-1234-1234-123456789abc/v2.0/.well-known/openid-configuration"
            assert settings.OPENID_CONFIG_URL == expected_url
