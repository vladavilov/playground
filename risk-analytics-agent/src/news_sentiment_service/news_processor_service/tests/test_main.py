import pytest
from unittest.mock import Mock, patch
from fastapi.testclient import TestClient
import sys
import os

# Add paths to import from src and common
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', 'common', 'src'))

from main import app


class TestFastAPIApp:
    
    @pytest.fixture
    def client(self):
        """Create a test client for the FastAPI app."""
        return TestClient(app)
    
    @patch('main.settings')
    def test_health_endpoint(self, mock_settings, client):
        """Test the health check endpoint."""
        mock_settings.API_PORT = 8000
        
        response = client.get("/health")
        
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "healthy"
        assert data["service"] == "news-processor-service"
        assert "azure_openai_configured" in data
        assert "message_processor_active" in data
        assert "settings" in data
    
    @patch('main.message_processor')
    @patch('main.settings')
    def test_detailed_health_endpoint(self, mock_settings, mock_message_processor, client):
        """Test the detailed health check endpoint."""
        mock_settings.AZURE_OPENAI_ENDPOINT = "https://test.openai.azure.com/"
        mock_settings.SERVICE_BUS_QUEUE_NAME = "test-queue"
        mock_settings.AZURE_COSMOSDB_ENDPOINT = "https://test.documents.azure.com:443/"
        
        # Mock message processor to appear active
        mock_message_processor.is_processing.return_value = True
        
        response = client.get("/health/detailed")
        
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "healthy"
        assert data["service"] == "news-processor-service"
        assert "timestamp" in data
        assert "dependencies" in data
        assert "azure_openai" in data["dependencies"]
        assert "message_processor" in data["dependencies"]
        assert "cosmos_db" in data["dependencies"]
    
    @patch('main.settings')
    def test_stats_endpoint_no_processor(self, mock_settings, client):
        """Test the stats endpoint when message processor is not initialized."""
        response = client.get("/stats")
        
        assert response.status_code == 200
        data = response.json()
        assert "message" in data
        assert "error" in data
        assert data["message"] == "Message processor not initialized"
    

    
    def test_app_metadata(self, client):
        """Test the FastAPI app metadata."""
        response = client.get("/docs")
        assert response.status_code == 200
        
        # Check that the app title and description are set correctly
        response = client.get("/openapi.json")
        assert response.status_code == 200
        openapi_data = response.json()
        assert openapi_data["info"]["title"] == "News Processor Service"
        assert openapi_data["info"]["description"] == "AI-powered news article enrichment and processing service"
        assert openapi_data["info"]["version"] == "1.0.0" 