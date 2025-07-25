"""
Tests for PostgreSQL configuration settings.
"""

import pytest
from unittest.mock import Mock
from configuration.postgres_config import PostgresSettings, get_postgres_settings


@pytest.fixture(autouse=True)
def reset_get_postgres_settings_cache():
    """Reset the cache for get_postgres_settings before each test."""
    get_postgres_settings.cache_clear()

class TestPostgresSettings:
    """Test PostgreSQL configuration settings."""
    
    def test_postgres_settings_default_values(self):
        """Test that PostgreSQL settings have proper default values."""
        settings = PostgresSettings()
        
        assert settings.POSTGRES_HOST == "localhost"
        assert settings.POSTGRES_PORT == 5432
        assert settings.POSTGRES_USER == "postgres"
        assert settings.POSTGRES_PASSWORD == "postgres123"
        assert settings.POSTGRES_DB == "requirementsdb"
        assert settings.POSTGRES_SCHEMA == "public"
        assert settings.POSTGRES_POOL_SIZE == 5
        assert settings.POSTGRES_MAX_OVERFLOW == 10
        assert settings.POSTGRES_POOL_TIMEOUT == 30
        assert settings.POSTGRES_POOL_RECYCLE == 1800
    
    def test_postgres_settings_from_env(self, monkeypatch):
        """Test that PostgreSQL settings load from environment variables."""
        monkeypatch.setenv("POSTGRES_HOST", "test-postgres")
        monkeypatch.setenv("POSTGRES_PORT", "5433")
        monkeypatch.setenv("POSTGRES_USER", "test-user")
        monkeypatch.setenv("POSTGRES_PASSWORD", "test-password")
        monkeypatch.setenv("POSTGRES_DB", "test-db")
        
        settings = PostgresSettings()
        
        assert settings.POSTGRES_HOST == "test-postgres"
        assert settings.POSTGRES_PORT == 5433
        assert settings.POSTGRES_USER == "test-user"
        assert settings.POSTGRES_PASSWORD == "test-password"
        assert settings.POSTGRES_DB == "test-db"
    
    def test_get_postgres_settings_cached(self):
        """Test that get_postgres_settings returns cached instance."""
        settings1 = get_postgres_settings()
        settings2 = get_postgres_settings()
        
        assert settings1 is settings2
    
    def test_postgres_settings_port_validation(self):
        """Test PostgreSQL port validation."""
        with pytest.raises(ValueError):
            PostgresSettings(POSTGRES_PORT=0)
        
        with pytest.raises(ValueError):
            PostgresSettings(POSTGRES_PORT=65536)
    
    def test_database_url_property(self):
        """Test DATABASE_URL property."""
        settings = PostgresSettings(
            POSTGRES_HOST="test-host",
            POSTGRES_PORT=5432,
            POSTGRES_USER="test-user",
            POSTGRES_PASSWORD="test-password",
            POSTGRES_DB="test-db"
        )
        
        expected_url = "postgresql://test-user:test-password@test-host:5432/test-db"
        assert settings.DATABASE_URL == expected_url
    
    def test_async_database_url_property(self):
        """Test ASYNC_DATABASE_URL property."""
        settings = PostgresSettings(
            POSTGRES_HOST="test-host",
            POSTGRES_PORT=5432,
            POSTGRES_USER="test-user",
            POSTGRES_PASSWORD="test-password",
            POSTGRES_DB="test-db"
        )
        
        expected_url = "postgresql+asyncpg://test-user:test-password@test-host:5432/test-db"
        assert settings.ASYNC_DATABASE_URL == expected_url