import pytest
from unittest.mock import Mock, patch
from neo4j import Driver
from utils.neo4j_client import (
    Neo4jClientFactory,
    Neo4jClient,
    get_neo4j_client
)
from configuration.common_config import AppSettings
from configuration.neo4j_config import Neo4jSettings

@pytest.fixture
def mock_app_settings():
    """Mock application settings."""
    settings = Mock(spec=AppSettings)
    settings.neo4j = Mock(spec=Neo4jSettings)
    settings.neo4j.NEO4J_URI = "bolt://localhost:7687"
    settings.neo4j.NEO4J_USERNAME = "neo4j"
    settings.neo4j.NEO4J_PASSWORD = "neo4j123"
    settings.neo4j.NEO4J_DATABASE = "neo4j"
    settings.neo4j.NEO4J_CONNECTION_TIMEOUT = 30.0
    settings.neo4j.NEO4J_MAX_CONNECTION_POOL_SIZE = 50
    settings.neo4j.NEO4J_MAX_TRANSACTION_RETRY_TIME = 30.0
    return settings

@patch('utils.neo4j_client.GraphDatabase.driver')
def test_create_driver(mock_driver, mock_app_settings):
    """Test driver creation."""
    mock_driver_instance = Mock(spec=Driver)
    mock_driver.return_value = mock_driver_instance
    
    driver = Neo4jClientFactory.create_driver(mock_app_settings.neo4j)
    
    mock_driver.assert_called_once_with(
        mock_app_settings.neo4j.NEO4J_URI,
        auth=(mock_app_settings.neo4j.NEO4J_USERNAME, mock_app_settings.neo4j.NEO4J_PASSWORD),
        max_connection_lifetime=mock_app_settings.neo4j.NEO4J_CONNECTION_TIMEOUT,
        max_connection_pool_size=mock_app_settings.neo4j.NEO4J_MAX_CONNECTION_POOL_SIZE,
        max_transaction_retry_time=mock_app_settings.neo4j.NEO4J_MAX_TRANSACTION_RETRY_TIME
    )
    assert driver == mock_driver_instance

@patch('utils.neo4j_client.Neo4jClientFactory.create_driver')
def test_neo4j_client_init(mock_create_driver, mock_app_settings):
    """Test Neo4j client initialization."""
    mock_driver = Mock(spec=Driver)
    mock_create_driver.return_value = mock_driver
    
    client = Neo4jClient(mock_app_settings.neo4j)
    
    mock_create_driver.assert_called_once_with(mock_app_settings.neo4j)
    assert client.settings == mock_app_settings.neo4j
    assert client._driver == mock_driver

def test_close(mock_app_settings):
    """Test close method."""
    with patch('utils.neo4j_client.Neo4jClientFactory.create_driver') as mock_create_driver:
        mock_driver = Mock(spec=Driver)
        mock_create_driver.return_value = mock_driver
        
        with Neo4jClient(mock_app_settings.neo4j) as client:
            client._driver.close.assert_not_called()
        
        mock_driver.close.assert_called_once()

def test_get_session(mock_app_settings):
    """Test get_session method."""
    with patch('utils.neo4j_client.Neo4jClientFactory.create_driver') as mock_create_driver:
        mock_driver = Mock(spec=Driver)
        mock_create_driver.return_value = mock_driver
        
        with Neo4jClient(mock_app_settings.neo4j) as client:
            client.get_session()
            mock_driver.session.assert_called_once_with(database="neo4j")
            
            client.get_session(database="test_db")
            mock_driver.session.assert_called_with(database="test_db")

def test_get_async_session(mock_app_settings):
    """Test get_async_session method."""
    with patch('utils.neo4j_client.Neo4jClientFactory.create_driver') as mock_create_driver:
        mock_driver = Mock(spec=Driver)
        mock_create_driver.return_value = mock_driver
        
        with Neo4jClient(mock_app_settings.neo4j) as client:
            client.get_async_session()
            mock_driver.session.assert_called_once_with(database="neo4j")
            
            client.get_async_session(database="test_db")
            mock_driver.session.assert_called_with(database="test_db")

@patch('utils.neo4j_client.Neo4jClient')
@patch('utils.neo4j_client.get_app_settings')
def test_get_neo4j_client(mock_get_settings, mock_client_class, mock_app_settings):
    """Test get_neo4j_client function."""
    mock_client = Mock()
    
    mock_get_settings.return_value = mock_app_settings
    mock_client_class.return_value = mock_client
    
    get_neo4j_client.cache_clear()
    
    client = get_neo4j_client()
    
    mock_get_settings.assert_called_once()
    mock_client_class.assert_called_once_with(mock_app_settings.neo4j)
    assert client == mock_client

@patch('utils.neo4j_client.Neo4jClient')
@patch('utils.neo4j_client.get_app_settings')
def test_get_neo4j_client_cached(mock_get_settings, mock_client_class, mock_app_settings):
    """Test get_neo4j_client caching."""
    mock_client = Mock()
    
    mock_get_settings.return_value = mock_app_settings
    mock_client_class.return_value = mock_client
    
    get_neo4j_client.cache_clear()
    
    client1 = get_neo4j_client()
    client2 = get_neo4j_client()
    
    mock_client_class.assert_called_once_with(mock_app_settings.neo4j)
    assert client1 == client2
