"""
Tests for PostgreSQL client utilities.
"""

import pytest
from unittest.mock import Mock, patch
from utils.postgres_client import (
    PostgresClient,
    get_postgres_client
)
from configuration.common_config import AppSettings
from configuration.postgres_config import PostgresSettings

@pytest.fixture
def mock_app_settings():
    """Mock application settings."""
    settings = Mock(spec=AppSettings)
    settings.postgres = Mock(spec=PostgresSettings)
    settings.postgres.DATABASE_URL = "postgresql://postgres:postgres123@localhost:5432/requirementsdb"
    settings.postgres.ASYNC_DATABASE_URL = "postgresql+asyncpg://postgres:postgres123@localhost:5432/requirementsdb"
    settings.postgres.POSTGRES_SCHEMA = "public"
    settings.postgres.POSTGRES_POOL_SIZE = 5
    settings.postgres.POSTGRES_MAX_OVERFLOW = 10
    settings.postgres.POSTGRES_POOL_TIMEOUT = 30
    settings.postgres.POSTGRES_POOL_RECYCLE = 1800
    settings.postgres.ECHO = False
    settings.postgres.POSTGRES_HOST = "localhost"
    settings.postgres.POSTGRES_PORT = 5432
    settings.postgres.POSTGRES_USER = "postgres"
    settings.postgres.POSTGRES_PASSWORD = "postgres123"
    settings.postgres.POSTGRES_DB = "requirementsdb"
    return settings

@patch('utils.postgres_client.create_engine')
@patch('utils.postgres_client.create_async_engine')
@patch('utils.postgres_client.sessionmaker')
@patch('utils.postgres_client.async_sessionmaker')
def test_postgres_client_init(
    mock_async_sessionmaker,
    mock_sessionmaker,
    mock_create_async_engine,
    mock_create_engine,
    mock_app_settings
):
    """Test PostgreSQL client initialization."""
    mock_sync_engine = Mock()
    mock_async_engine = Mock()
    mock_sync_session_factory = Mock()
    mock_async_session_factory = Mock()
    
    mock_create_engine.return_value = mock_sync_engine
    mock_create_async_engine.return_value = mock_async_engine
    mock_sessionmaker.return_value = mock_sync_session_factory
    mock_async_sessionmaker.return_value = mock_async_session_factory
    
    client = PostgresClient(mock_app_settings.postgres)
    
    assert client.settings == mock_app_settings.postgres
    assert client.sync_engine == mock_sync_engine
    assert client.async_engine == mock_async_engine
    assert client.sync_session_factory == mock_sync_session_factory
    assert client.metadata.schema == mock_app_settings.postgres.POSTGRES_SCHEMA

def test_get_sync_session():
    """Test get_sync_session method."""
    client = PostgresClient.__new__(PostgresClient)
    client.sync_session_factory = Mock()
    
    client.get_sync_session()
    
    client.sync_session_factory.assert_called_once()

@patch('utils.postgres_client.PostgresClient')
@patch('utils.postgres_client.get_app_settings')
def test_get_postgres_client(mock_get_settings, mock_client_class, mock_app_settings):
    """Test get_postgres_client function."""
    mock_client = Mock()
    
    mock_get_settings.return_value = mock_app_settings
    mock_client_class.return_value = mock_client
    
    get_postgres_client.cache_clear()
    
    client = get_postgres_client()
    
    mock_get_settings.assert_called_once()
    mock_client_class.assert_called_once_with(mock_app_settings.postgres)
    assert client == mock_client

@patch('utils.postgres_client.PostgresClient')
@patch('utils.postgres_client.get_app_settings')
def test_get_postgres_client_cached(mock_get_settings, mock_client_class, mock_app_settings):
    """Test get_postgres_client caching."""
    mock_client = Mock()
    
    mock_get_settings.return_value = mock_app_settings
    mock_client_class.return_value = mock_client
    
    get_postgres_client.cache_clear()
    
    client1 = get_postgres_client()
    client2 = get_postgres_client()
    
    mock_client_class.assert_called_once_with(mock_app_settings.postgres)
    assert client1 == client2
