"""
Tests for Neo4j configuration settings.
"""

import pytest
from unittest.mock import Mock
from configuration.neo4j_config import Neo4jSettings, get_neo4j_settings


@pytest.fixture(autouse=True)
def reset_get_neo4j_settings_cache():
    """Reset the cache for get_neo4j_settings before each test."""
    get_neo4j_settings.cache_clear()

class TestNeo4jSettings:
    """Test Neo4j configuration settings."""
    
    def test_neo4j_settings_default_values(self):
        """Test that Neo4j settings have proper default values."""
        settings = Neo4jSettings()
        
        assert settings.NEO4J_URI == "bolt://localhost:7687"
        assert settings.NEO4J_USERNAME == "neo4j"
        assert settings.NEO4J_PASSWORD == "neo4j123"
        assert settings.NEO4J_DATABASE == "neo4j"
        assert settings.NEO4J_CONNECTION_TIMEOUT == 30.0
        assert settings.RETRY_MAX_ATTEMPTS == 3
        assert settings.RETRY_BACKOFF_BASE_SEC == 2.0
        assert settings.NEO4J_MAX_CONNECTION_POOL_SIZE == 50
        assert settings.NEO4J_MAX_TRANSACTION_RETRY_TIME == 30.0
    
    def test_neo4j_settings_from_env(self, monkeypatch):
        """Test that Neo4j settings load from environment variables."""
        monkeypatch.setenv("NEO4J_URI", "bolt://test-neo4j:7687")
        monkeypatch.setenv("NEO4J_USERNAME", "test-user")
        monkeypatch.setenv("NEO4J_PASSWORD", "test-password")
        monkeypatch.setenv("NEO4J_DATABASE", "test-db")
        monkeypatch.setenv("NEO4J_CONNECTION_TIMEOUT", "60.0")
        
        settings = Neo4jSettings()
        
        assert settings.NEO4J_URI == "bolt://test-neo4j:7687"
        assert settings.NEO4J_USERNAME == "test-user"
        assert settings.NEO4J_PASSWORD == "test-password"
        assert settings.NEO4J_DATABASE == "test-db"
        assert settings.NEO4J_CONNECTION_TIMEOUT == 60.0
    
    def test_get_neo4j_settings_cached(self):
        """Test that get_neo4j_settings returns cached instance."""
        settings1 = get_neo4j_settings()
        settings2 = get_neo4j_settings()
        
        assert settings1 is settings2
    
    def test_neo4j_settings_validation(self):
        """Test Neo4j settings validation."""
        settings = Neo4jSettings()
        
        # Test that all required fields have values
        assert settings.NEO4J_URI is not None
        assert settings.NEO4J_USERNAME is not None
        assert settings.NEO4J_PASSWORD is not None
        assert settings.NEO4J_DATABASE is not None
        assert isinstance(settings.NEO4J_CONNECTION_TIMEOUT, float)
        assert isinstance(settings.RETRY_MAX_ATTEMPTS, int)
        assert isinstance(settings.RETRY_BACKOFF_BASE_SEC, float)
    
    def test_neo4j_settings_timeout_validation(self):
        """Test Neo4j timeout validation."""
        with pytest.raises(ValueError):
            Neo4jSettings(NEO4J_CONNECTION_TIMEOUT=0)
        
        with pytest.raises(ValueError):
            Neo4jSettings(RETRY_BACKOFF_BASE_SEC=-1.0)
        
        with pytest.raises(ValueError):
            Neo4jSettings(NEO4J_MAX_TRANSACTION_RETRY_TIME=0)
    
    def test_neo4j_settings_positive_int_validation(self):
        """Test Neo4j positive integer validation."""
        with pytest.raises(ValueError):
            Neo4jSettings(RETRY_MAX_ATTEMPTS=0)
        
        with pytest.raises(ValueError):
            Neo4jSettings(NEO4J_MAX_CONNECTION_POOL_SIZE=-1)