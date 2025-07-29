"""
Consolidated tests for blob storage configuration settings and integration.
"""

from unittest.mock import patch, MagicMock

import pytest
from pydantic import ValidationError

from configuration.blob_storage_config import BlobStorageSettings, get_blob_storage_settings
from configuration.common_config import AppSettings, get_app_settings


class TestBlobStorageSettings:
    """Test cases for BlobStorageSettings configuration class."""

    def test_default_values(self):
        """Test that default configuration values are set correctly."""
        settings = BlobStorageSettings()
        
        assert settings.AZURE_STORAGE_CONNECTION_STRING == "DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;BlobEndpoint=http://azurite:10000/devstoreaccount1;"
        assert settings.AZURE_STORAGE_CONTAINER_NAME == "documents"
        assert settings.AZURE_STORAGE_ACCOUNT_NAME == "devstoreaccount1"
        assert settings.AZURE_STORAGE_ACCOUNT_KEY == "Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw=="
        assert settings.AZURE_STORAGE_BLOB_ENDPOINT == "http://azurite:10000/devstoreaccount1"
        assert settings.AZURE_STORAGE_MAX_SINGLE_PUT_SIZE == 64 * 1024 * 1024  # 64MB
        assert settings.AZURE_STORAGE_MAX_BLOCK_SIZE == 4 * 1024 * 1024  # 4MB

    @patch.dict('os.environ', {
        'AZURE_STORAGE_CONNECTION_STRING': 'custom_connection_string',
        'AZURE_STORAGE_CONTAINER_NAME': 'custom-container',
        'AZURE_STORAGE_ACCOUNT_NAME': 'custom_account',
        'AZURE_STORAGE_ACCOUNT_KEY': 'custom_key',
        'AZURE_STORAGE_BLOB_ENDPOINT': 'http://custom:10000/custom',
        'AZURE_STORAGE_MAX_SINGLE_PUT_SIZE': '128000000',
        'AZURE_STORAGE_MAX_BLOCK_SIZE': '8000000'
    })
    def test_environment_variable_override(self):
        """Test that environment variables override default values."""
        settings = BlobStorageSettings()
        
        assert settings.AZURE_STORAGE_CONNECTION_STRING == 'custom_connection_string'
        assert settings.AZURE_STORAGE_CONTAINER_NAME == 'custom-container'
        assert settings.AZURE_STORAGE_ACCOUNT_NAME == 'custom_account'
        assert settings.AZURE_STORAGE_ACCOUNT_KEY == 'custom_key'
        assert settings.AZURE_STORAGE_BLOB_ENDPOINT == 'http://custom:10000/custom'
        assert settings.AZURE_STORAGE_MAX_SINGLE_PUT_SIZE == 128000000
        assert settings.AZURE_STORAGE_MAX_BLOCK_SIZE == 8000000

    def test_container_name_validation_valid(self):
        """Test that valid container names pass validation."""
        valid_names = [
            "documents",
            "my-container",
            "container123",
            "a" * 63,  # Maximum length
            "test-container-name"
        ]
        
        for name in valid_names:
            settings = BlobStorageSettings(AZURE_STORAGE_CONTAINER_NAME=name)
            assert settings.AZURE_STORAGE_CONTAINER_NAME == name

    def test_container_name_validation_invalid(self):
        """Test that invalid container names fail validation."""
        invalid_names = [
            "A",  # Uppercase
            "container_name",  # Underscore
            "container.",  # Ends with dot
            "-container",  # Starts with dash
            "container-",  # Ends with dash
            "a" * 64,  # Too long
            "ab",  # Too short
            "container..name",  # Consecutive dots
            "container--name",  # Consecutive dashes
        ]
        
        for name in invalid_names:
            with pytest.raises(ValidationError):
                BlobStorageSettings(AZURE_STORAGE_CONTAINER_NAME=name)

    def test_max_single_put_size_validation(self):
        """Test validation of max single put size."""
        # Valid values
        valid_sizes = [1024, 64 * 1024 * 1024, 256 * 1024 * 1024]
        for size in valid_sizes:
            settings = BlobStorageSettings(AZURE_STORAGE_MAX_SINGLE_PUT_SIZE=size)
            assert settings.AZURE_STORAGE_MAX_SINGLE_PUT_SIZE == size

        # Invalid values
        invalid_sizes = [0, -1, 257 * 1024 * 1024]  # 0, negative, too large
        for size in invalid_sizes:
            with pytest.raises(ValidationError):
                BlobStorageSettings(AZURE_STORAGE_MAX_SINGLE_PUT_SIZE=size)

    def test_max_block_size_validation(self):
        """Test validation of max block size."""
        # Valid values
        valid_sizes = [1024, 4 * 1024 * 1024, 100 * 1024 * 1024]
        for size in valid_sizes:
            settings = BlobStorageSettings(AZURE_STORAGE_MAX_BLOCK_SIZE=size)
            assert settings.AZURE_STORAGE_MAX_BLOCK_SIZE == size

        # Invalid values
        invalid_sizes = [0, -1, 101 * 1024 * 1024]  # 0, negative, too large
        for size in invalid_sizes:
            with pytest.raises(ValidationError):
                BlobStorageSettings(AZURE_STORAGE_MAX_BLOCK_SIZE=size)

    def test_inherits_from_base_config(self):
        """Test that BlobStorageSettings inherits from BaseConfig."""
        from configuration.base_config import BaseConfig
        settings = BlobStorageSettings()
        assert isinstance(settings, BaseConfig)


class TestGetBlobStorageSettings:
    """Test cases for get_blob_storage_settings function."""

    def test_returns_blob_storage_settings_instance(self):
        """Test that function returns BlobStorageSettings instance."""
        settings = get_blob_storage_settings()
        assert isinstance(settings, BlobStorageSettings)

    def test_caching_behavior(self):
        """Test that function returns the same instance on multiple calls (caching)."""
        settings1 = get_blob_storage_settings()
        settings2 = get_blob_storage_settings()
        assert settings1 is settings2

    @patch('configuration.blob_storage_config.BlobStorageSettings')
    def test_settings_instantiation(self, mock_settings_class):
        """Test that BlobStorageSettings is instantiated correctly."""
        mock_instance = MagicMock()
        mock_settings_class.return_value = mock_instance
        
        # Clear the cache first
        get_blob_storage_settings.cache_clear()
        
        result = get_blob_storage_settings()
        
        mock_settings_class.assert_called_once()
        assert result == mock_instance


class TestAppSettingsWithBlobStorage:
    """Test cases for AppSettings with blob storage integration."""

    def test_app_settings_includes_blob_storage(self):
        """Test that AppSettings includes blob storage configuration."""
        settings = AppSettings()
        
        assert hasattr(settings, 'blob_storage')
        assert isinstance(settings.blob_storage, BlobStorageSettings)

    def test_blob_storage_default_factory(self):
        """Test that blob storage uses default factory."""
        settings = AppSettings()
        
        # Should have default blob storage settings
        assert settings.blob_storage.AZURE_STORAGE_CONTAINER_NAME == "documents"
        assert "devstoreaccount1" in settings.blob_storage.AZURE_STORAGE_CONNECTION_STRING

    @patch.dict('os.environ', {
        'AZURE_STORAGE_CONTAINER_NAME': 'test-container',
        'AZURE_STORAGE_ACCOUNT_NAME': 'testaccount'
    })
    def test_blob_storage_environment_override(self):
        """Test that blob storage respects environment variables."""
        settings = AppSettings()
        
        assert settings.blob_storage.AZURE_STORAGE_CONTAINER_NAME == 'test-container'
        assert settings.blob_storage.AZURE_STORAGE_ACCOUNT_NAME == 'testaccount'

    def test_all_service_configurations_present(self):
        """Test that all service configurations are present including blob storage."""
        settings = AppSettings()
        
        # Existing configurations
        assert hasattr(settings, 'postgres')
        assert hasattr(settings, 'neo4j')
        assert hasattr(settings, 'redis')
        assert hasattr(settings, 'celery')
        assert hasattr(settings, 'http_client')
        
        # New blob storage configuration
        assert hasattr(settings, 'blob_storage')


class TestGetAppSettingsWithBlobStorage:
    """Test cases for get_app_settings function with blob storage."""

    def test_returns_app_settings_with_blob_storage(self):
        """Test that function returns AppSettings with blob storage."""
        settings = get_app_settings()
        
        assert isinstance(settings, AppSettings)
        assert hasattr(settings, 'blob_storage')
        assert isinstance(settings.blob_storage, BlobStorageSettings)

    def test_caching_includes_blob_storage(self):
        """Test that caching works correctly with blob storage included."""
        settings1 = get_app_settings()
        settings2 = get_app_settings()
        
        assert settings1 is settings2
        assert settings1.blob_storage is settings2.blob_storage

    def setup_method(self):
        """Clear all caches before each test."""
        get_app_settings.cache_clear()
        get_blob_storage_settings.cache_clear()

    @patch.dict('os.environ', {
        'AZURE_STORAGE_CONTAINER_NAME': 'integration-test-container',
        'AZURE_STORAGE_CONNECTION_STRING': 'test_connection_string'
    })
    def test_end_to_end_configuration_flow(self):
        """Test complete configuration flow from environment to client usage."""
        from utils.blob_storage import BlobStorageClient
        
        # Create new instances to pick up environment variables
        settings = BlobStorageSettings()
        client = BlobStorageClient(settings=settings)
        
        # Verify environment override worked
        assert settings.AZURE_STORAGE_CONTAINER_NAME == 'integration-test-container'
        assert settings.AZURE_STORAGE_CONNECTION_STRING == 'test_connection_string'
        assert client.container_name == 'integration-test-container'
        assert client.connection_string == 'test_connection_string'