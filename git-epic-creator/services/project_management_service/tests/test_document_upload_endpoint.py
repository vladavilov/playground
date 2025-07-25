"""
Simple tests for document upload endpoint.
"""

import pytest
from unittest.mock import Mock, patch
from uuid import uuid4
from fastapi.testclient import TestClient


def test_document_upload_endpoint_exists():
    """Test that document upload endpoint exists in the router."""
    # Import here to avoid initialization issues
    with patch('services.document_upload_service.BlobStorageClient'):
        with patch('utils.postgres_client.get_postgres_client'):
            with patch('utils.redis_client.get_redis_client'):
                from main import app
                
                client = TestClient(app)
                project_id = uuid4()
                
                test_files = [
                    ("files", ("test.pdf", b"PDF content", "application/pdf"))
                ]
                
                response = client.post(f"/projects/{project_id}/documents/upload", files=test_files)
                
                # The endpoint should exist (not 404), even if auth fails
                assert response.status_code != 404


def test_document_upload_endpoint_in_openapi():
    """Test that document upload endpoint appears in OpenAPI spec."""
    with patch('services.document_upload_service.BlobStorageClient'):
        with patch('utils.postgres_client.get_postgres_client'):
            with patch('utils.redis_client.get_redis_client'):
                from main import app
                
                client = TestClient(app)
                
                response = client.get("/openapi.json")
                assert response.status_code == 200
                
                openapi_data = response.json()
                paths = openapi_data.get("paths", {})
                
                # Check that document upload path exists
                upload_path_found = False
                for path in paths.keys():
                    if "/documents/upload" in path:
                        upload_path_found = True
                        break
                
                assert upload_path_found, "Document upload endpoint not found in OpenAPI spec"