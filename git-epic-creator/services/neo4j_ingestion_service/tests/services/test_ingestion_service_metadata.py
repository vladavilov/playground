"""
Tests for metadata handling in the Neo4j ingestion service.

These tests verify that document metadata is properly extracted and passed
to the GraphRAG pipeline for knowledge graph creation.
"""

import asyncio
import json
import tempfile
from pathlib import Path
from unittest.mock import Mock, patch

from services.ingestion_service import Neo4jIngestionService


class MockBlobStorageClient:
    def __init__(self, file_list=None):
        self.file_list = file_list or []
        self.downloaded_files = {}
        self.deleted_files = []

    def list_files(self, project_id=None, prefix=None):
        result = Mock()
        result.success = True
        result.file_list = self.file_list
        return result

    def download_file(self, blob_name, local_path, project_id=None):
        # Create a mock JSON file with metadata
        if blob_name in self.downloaded_files:
            with open(local_path, 'w', encoding='utf-8') as f:
                json.dump(self.downloaded_files[blob_name], f)
        return Mock(success=True)

    def delete_file(self, blob_name, project_id=None):
        self.deleted_files.append(blob_name)
        return Mock(success=True)


class MockNeo4jClient:
    def __init__(self):
        self.driver = Mock()


def test_ingestion_service_passes_metadata_to_graphrag():
    """Test that the ingestion service extracts and passes metadata to GraphRAG."""
    
    # Create sample JSON content with metadata
    sample_json_content = {
        "title": "test_document.pdf",
        "text": "Sample extracted text content",
        "metadata": {
            "file_name": "test_document.pdf",
            "file_type": "pdf",
            "content_type": "application/pdf",
            "creation_date": "2020-10-14T15:08:10Z",
            "modification_date": "2020-10-14T15:08:10Z"
        }
    }

    # Mock blob storage with our sample content
    mock_blob_client = MockBlobStorageClient()
    mock_blob_client.file_list = ["output/test_document.json"]
    mock_blob_client.downloaded_files = {"output/test_document.json": sample_json_content}

    # Mock the blob storage client factory
    with patch('services.ingestion_service.get_blob_storage_client', return_value=mock_blob_client):
        # Mock the GraphRAG pipeline function
        with patch('services.ingestion_service.run_documents') as mock_run_documents:
            # Create the service
            service = Neo4jIngestionService(client=MockNeo4jClient())
            
            # Run the pipeline
            result = asyncio.run(service.run_graphrag_pipeline("123e4567-e89b-12d3-a456-426614174000"))
            
            # Verify the result
            assert result["success"] is True
            assert result["counts"]["documents"] == 1
            
            # Verify that run_documents was called
            mock_run_documents.assert_called_once()
            
            # Get the call arguments
            call_args = mock_run_documents.call_args
            driver = call_args[0][0]
            documents = call_args[0][1]
            
            # Verify the arguments
            assert len(documents) == 1
            assert documents[0]["project_id"] == "123e4567-e89b-12d3-a456-426614174000"
            assert "test_document.json" in documents[0]["path"]


def test_ingestion_service_handles_missing_metadata_gracefully():
    """Test that the ingestion service handles JSON files without metadata gracefully."""
    
    # Create JSON content without metadata
    sample_json_content = {
        "title": "test_document.pdf",
        "text": "Sample extracted text content"
        # No metadata field
    }

    mock_blob_client = MockBlobStorageClient()
    mock_blob_client.file_list = ["output/test_document.json"]
    mock_blob_client.downloaded_files = {"output/test_document.json": sample_json_content}

    with patch('services.ingestion_service.get_blob_storage_client', return_value=mock_blob_client):
        with patch('services.ingestion_service.run_documents') as mock_run_documents:
            service = Neo4jIngestionService(client=MockNeo4jClient())
            
            # This should not raise an error
            result = asyncio.run(service.run_graphrag_pipeline("123e4567-e89b-12d3-a456-426614174000"))
            
            assert result["success"] is True
            mock_run_documents.assert_called_once()


def test_ingestion_service_handles_invalid_json():
    """Test that the ingestion service handles invalid JSON gracefully."""
    
    mock_blob_client = MockBlobStorageClient()
    mock_blob_client.file_list = ["output/test_document.json"]
    
    # Create a temporary file with invalid JSON
    with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
        f.write("invalid json content")
        temp_file_path = f.name
    
    mock_blob_client.downloaded_files = {"output/test_document.json": {"path": temp_file_path}}

    with patch('services.ingestion_service.get_blob_storage_client', return_value=mock_blob_client):
        with patch('services.ingestion_service.run_documents') as mock_run_documents:
            service = Neo4jIngestionService(client=MockNeo4jClient())
            
            # This should not raise an error
            result = asyncio.run(service.run_graphrag_pipeline("123e4567-e89b-12d3-a456-426614174000"))
            
            assert result["success"] is True
            mock_run_documents.assert_called_once()
    
    # Clean up
    Path(temp_file_path).unlink(missing_ok=True)


def test_ingestion_service_processes_multiple_documents():
    """Test that the ingestion service processes multiple documents correctly."""
    
    # Create multiple JSON files with metadata
    sample_contents = {
        "output/doc1.pdf.json": {
            "title": "doc1.pdf",
            "text": "Content 1",
            "metadata": {
                "file_name": "doc1.pdf",
                "file_type": "pdf",
                "content_type": "application/pdf"
            }
        },
        "output/doc2.docx.json": {
            "title": "doc2.docx",
            "text": "Content 2",
            "metadata": {
                "file_name": "doc2.docx",
                "file_type": "docx",
                "content_type": "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
            }
        }
    }

    mock_blob_client = MockBlobStorageClient()
    mock_blob_client.file_list = list(sample_contents.keys())
    mock_blob_client.downloaded_files = sample_contents

    with patch('services.ingestion_service.get_blob_storage_client', return_value=mock_blob_client):
        with patch('services.ingestion_service.run_documents') as mock_run_documents:
            service = Neo4jIngestionService(client=MockNeo4jClient())
            
            result = asyncio.run(service.run_graphrag_pipeline("123e4567-e89b-12d3-a456-426614174000"))
            
            assert result["success"] is True
            assert result["counts"]["documents"] == 2
            
            # Verify that run_documents was called with all documents
            mock_run_documents.assert_called_once()
            call_args = mock_run_documents.call_args
            documents = call_args[0][1]
            assert len(documents) == 2
