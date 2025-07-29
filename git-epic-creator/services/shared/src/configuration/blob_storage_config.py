"""
Azure Blob Storage configuration settings.
"""

from functools import lru_cache
from pydantic import Field, field_validator
from .base_config import BaseConfig


class BlobStorageSettings(BaseConfig):
    """
    Defines the Azure Blob Storage configuration settings.
    """
    AZURE_STORAGE_CONNECTION_STRING: str = Field(
        default="DefaultEndpointsProtocol=http;AccountName=devstoreaccount1;AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==;BlobEndpoint=http://azurite:10000/devstoreaccount1;",
        description="Azure Storage connection string"
    )
    AZURE_STORAGE_CONTAINER_NAME: str = Field(
        default="documents",
        description="Azure Storage container name"
    )
    AZURE_STORAGE_ACCOUNT_NAME: str = Field(
        default="devstoreaccount1",
        description="Azure Storage account name"
    )
    AZURE_STORAGE_ACCOUNT_KEY: str = Field(
        default="Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw==",
        description="Azure Storage account key"
    )
    AZURE_STORAGE_BLOB_ENDPOINT: str = Field(
        default="http://azurite:10000/devstoreaccount1",
        description="Azure Storage blob endpoint"
    )
    AZURE_STORAGE_MAX_SINGLE_PUT_SIZE: int = Field(
        default=64 * 1024 * 1024,  # 64MB
        description="Maximum size for single put operation in bytes"
    )
    AZURE_STORAGE_MAX_BLOCK_SIZE: int = Field(
        default=4 * 1024 * 1024,  # 4MB
        description="Maximum block size for multipart upload in bytes"
    )

    @field_validator('AZURE_STORAGE_CONTAINER_NAME')
    @classmethod
    def validate_container_name(cls, v):
        """Validate Azure Storage container name according to Azure rules."""
        if not v:
            raise ValueError("Container name cannot be empty")
        
        if len(v) < 3 or len(v) > 63:
            raise ValueError("Container name must be between 3 and 63 characters")
        
        if not v.islower():
            raise ValueError("Container name must be lowercase")
        
        if not v.replace('-', '').replace('.', '').isalnum():
            raise ValueError("Container name can only contain letters, numbers, hyphens, and dots")
        
        if v.startswith('-') or v.endswith('-'):
            raise ValueError("Container name cannot start or end with a hyphen")
        
        if v.endswith('.'):
            raise ValueError("Container name cannot end with a dot")
        
        if '--' in v or '..' in v:
            raise ValueError("Container name cannot contain consecutive hyphens or dots")
        
        return v

    @field_validator('AZURE_STORAGE_MAX_SINGLE_PUT_SIZE')
    @classmethod
    def validate_max_single_put_size(cls, v):
        """Validate max single put size."""
        if v <= 0:
            raise ValueError("Max single put size must be positive")
        
        if v > 256 * 1024 * 1024:  # 256MB limit
            raise ValueError("Max single put size cannot exceed 256MB")
        
        return v

    @field_validator('AZURE_STORAGE_MAX_BLOCK_SIZE')
    @classmethod
    def validate_max_block_size(cls, v):
        """Validate max block size."""
        if v <= 0:
            raise ValueError("Max block size must be positive")
        
        if v > 100 * 1024 * 1024:  # 100MB limit
            raise ValueError("Max block size cannot exceed 100MB")
        
        return v


@lru_cache()
def get_blob_storage_settings() -> BlobStorageSettings:
    """
    Creates a cached instance of BlobStorageSettings.
    This ensures that the settings are loaded only once and reused.
    """
    return BlobStorageSettings()