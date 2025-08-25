"""
Configuration management for end-to-end tests.

This module provides centralized configuration management for e2e tests,
supporting environment variable overrides and providing sensible defaults.
"""

import os
from typing import Dict, Any
from urllib.parse import urlparse
from pathlib import Path


class TestConfig:
    """
    Centralized configuration management for e2e tests.
    
    Supports environment variable configuration with fallback defaults,
    matching the docker-compose.env structure for consistency.
    """

    @classmethod
    def get_service_urls(cls) -> Dict[str, str]:
        """
        Get service URLs from environment variables with fallback defaults.
        
        Returns:
            Dict mapping service names to their URLs
        """
        return {
            "project_management": os.getenv("PROJECT_MANAGEMENT_SERVICE_URL", "http://localhost:8003"),
            "document_processing": os.getenv("DOCUMENT_PROCESSING_URL", "http://localhost:8004"),
            "neo4j_ingestion": os.getenv("NEO4J_INGESTION_URL", "http://localhost:8006"),
            "mock_auth": os.getenv("AZURE_AD_AUTHORITY", "http://localhost:8005"),
            "init_db_service": os.getenv("INIT_DB_SERVICE_URL", "http://localhost:8001"),
            "neo4j_maintenance": os.getenv("NEO4J_MAINTENANCE_URL", "http://localhost:8002"),
        }
    
    @classmethod
    def get_dummy_pdf_path(cls) -> str:
        """
        Get the absolute path to the test dummy PDF file.
        
        Returns:
            Absolute path to dummy.pdf
            
        Raises:
            FileNotFoundError: If dummy.pdf doesn't exist at expected location
        """
        current_dir = Path(__file__).parent
        pdf_path = current_dir / ".." / "resources" / "dummy.pdf"
        pdf_path = pdf_path.resolve()
        
        if not pdf_path.exists():
            raise FileNotFoundError(f"dummy.pdf not found at {pdf_path}")
            
        return str(pdf_path)
    
    @classmethod
    def read_dummy_pdf(cls) -> bytes:
        """
        Read and return the dummy PDF content as bytes.
        
        Returns:
            PDF file content as bytes
            
        Raises:
            FileNotFoundError: If dummy.pdf doesn't exist
            IOError: If file cannot be read
        """
        pdf_path = cls.get_dummy_pdf_path()
        
        try:
            with open(pdf_path, 'rb') as f:
                return f.read()
        except IOError as e:
            raise IOError(f"Failed to read dummy.pdf: {e}") from e
    
    @classmethod
    def get_postgres_config(cls) -> Dict[str, Any]:
        """
        Get PostgreSQL configuration from environment variables.
        
        Returns:
            Dict with PostgreSQL connection parameters
        """
        return {
            "host": os.getenv("POSTGRES_HOST", "localhost"),
            "port": int(os.getenv("POSTGRES_PORT", "5432")),
            "user": os.getenv("POSTGRES_USER", "postgres"),
            "password": os.getenv("POSTGRES_PASSWORD", "postgres123"),
            "database": os.getenv("POSTGRES_DB", "requirementsdb")
        }
    
    @classmethod
    def get_neo4j_config(cls) -> Dict[str, Any]:
        """
        Get Neo4j configuration from environment variables.
        
        Returns:
            Dict with Neo4j connection parameters
        """
        return {
            "uri": os.getenv("NEO4J_URI", "bolt://localhost:7687"),
            "username": os.getenv("NEO4J_USERNAME", "neo4j"),
            "password": os.getenv("NEO4J_PASSWORD", "neo4j123"),
            "database": os.getenv("NEO4J_DATABASE", "neo4j"),
            "connection_timeout": float(os.getenv("NEO4J_CONNECTION_TIMEOUT", "30.0")),
            "max_retry_attempts": int(os.getenv("NEO4J_MAX_RETRY_ATTEMPTS", "3")),
            "retry_delay": float(os.getenv("NEO4J_RETRY_DELAY", "2.0"))
        }
    
    @classmethod
    def get_redis_config(cls) -> Dict[str, Any]:
        """
        Get Redis configuration from environment variables.
        
        Supports both REDIS_URL format and individual parameters.
        
        Returns:
            Dict with Redis connection parameters
        """
        redis_url = os.getenv("REDIS_URL")
        
        if redis_url:
            # Parse REDIS_URL to extract components
            parsed_url = urlparse(redis_url)
            host = parsed_url.hostname or "localhost"
            port = parsed_url.port or 6379
        else:
            # Fallback to individual parameters
            host = os.getenv("REDIS_HOST", "localhost")
            port = int(os.getenv("REDIS_PORT", "6379"))
        
        redis_config = {
            "host": host,
            "port": port,
            "db": int(os.getenv("REDIS_DB", "0")),
            "max_connections": int(os.getenv("REDIS_MAX_CONNECTIONS", "10")),
            "socket_connect_timeout": float(os.getenv("REDIS_SOCKET_CONNECT_TIMEOUT", "5.0")),
            "socket_timeout": float(os.getenv("REDIS_SOCKET_TIMEOUT", "5.0"))
        }
        
        # Add password only if it's provided and non-empty
        redis_password = os.getenv("REDIS_PASSWORD")
        if redis_password:
            redis_config["password"] = redis_password

        return redis_config
    
    @classmethod
    def get_auth_config(cls) -> Dict[str, str]:
        """
        Get authentication configuration from environment variables.
        
        Returns:
            Dict with authentication parameters
        """
        return {
            "tenant_id": os.getenv("AZURE_TENANT_ID", "e7963c3a-3b3a-43b6-9426-89e433d07e69"),
            "client_id": os.getenv("AZURE_CLIENT_ID", "a9e304a9-5b6c-4ef7-9b37-23a579a6d7be"),
            "client_secret": os.getenv("MOCK_CLIENT_SECRET", "mock-secret"),
            "scope": os.getenv("MOCK_SCOPE", "api://a9e304a9-5b6c-4ef7-9b37-23a579a6d7be/.default")
        }
    
    @classmethod
    def get_celery_config(cls) -> Dict[str, str]:
        """
        Get Celery configuration from environment variables.
        
        Returns:
            Dict with Celery broker and result backend URLs
        """
        return {
            "broker_url": os.getenv("CELERY_BROKER_URL", "redis://localhost:6379/0"),
            "result_backend": os.getenv("CELERY_RESULT_BACKEND", "redis://localhost:6379/0")
        }

    @classmethod
    def get_blob_storage_config(cls) -> Dict[str, Any]:
        """
        Get Azure Blob Storage configuration using exact literal values expected in docker-compose.

        Tests should break if these change, by design.
        """
        return {
            "connection_string": (
                "DefaultEndpointsProtocol=http;"
                "AccountName=devstoreaccount1;"
                "AccountKey=Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/"
                "K1SZFPTOtr/KBHBeksoGMGw==;"
                "BlobEndpoint=http://localhost:10000/devstoreaccount1;"
            ),
            "container_name": "documents",
            "account_name": "devstoreaccount1",
            "account_key": (
                "Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/"
                "K1SZFPTOtr/KBHBeksoGMGw=="
            ),
            "blob_endpoint": "http://localhost:10000/devstoreaccount1",
            "max_single_put_size": 67108864,
            "max_block_size": 4194304,
        }
    
class TestConstants:
    """Constants used across e2e tests."""
    
    DEFAULT_TIMEOUT = 30
    SERVICE_HEALTH_TIMEOUT = 20
    DOCUMENT_PROCESSING_TIMEOUT = 120
    CLEANUP_TIMEOUT = 30
    
    # HTTP status codes
    HTTP_OK = 200
    HTTP_CREATED = 201
    HTTP_NOT_FOUND = 404
    HTTP_SERVER_ERROR = 500
    
    # Project statuses
    PROJECT_STATUS_ACTIVE = "active"
    PROJECT_STATUS_PROCESSING = "processing"
    PROJECT_STATUS_RAG_PROCESSING = "rag_processing"
    PROJECT_STATUS_RAG_READY = "rag_ready"
    
    # Service health endpoints
    HEALTH_ENDPOINT = "/health"
    CELERY_HEALTH_ENDPOINT = "/health/celery" 