"""
Tests for document schemas in shared models.
"""

import pytest
from datetime import datetime
from uuid import UUID, uuid4
from pydantic import ValidationError

from models.document_schemas import (
    DocumentStatus,
    DocumentUploadResponse,
    DocumentProcessingStatus,
    DocumentMetadata,
    BulkUploadResponse
)


class TestDocumentStatus:
    """Test cases for DocumentStatus enum."""

    def test_document_status_values(self):
        """Test that DocumentStatus has correct values."""
        assert DocumentStatus.UPLOADED == "uploaded"
        assert DocumentStatus.PROCESSING == "processing"
        assert DocumentStatus.COMPLETED == "completed"
        assert DocumentStatus.FAILED == "failed"


class TestDocumentUploadResponse:
    """Test cases for DocumentUploadResponse model."""

    def test_document_upload_response_creation(self):
        """Test successful creation of DocumentUploadResponse."""
        document_id = uuid4()
        project_id = uuid4()
        upload_time = datetime.utcnow()
        
        response = DocumentUploadResponse(
            document_id=document_id,
            filename="test.pdf",
            file_size=1024,
            status=DocumentStatus.UPLOADED,
            upload_time=upload_time,
            project_id=project_id
        )
        
        assert response.document_id == document_id
        assert response.filename == "test.pdf"
        assert response.file_size == 1024
        assert response.status == DocumentStatus.UPLOADED
        assert response.upload_time == upload_time
        assert response.project_id == project_id

    def test_document_upload_response_serialization(self):
        """Test UUID and datetime serialization."""
        document_id = uuid4()
        project_id = uuid4()
        upload_time = datetime.utcnow()
        
        response = DocumentUploadResponse(
            document_id=document_id,
            filename="test.pdf",
            file_size=1024,
            status=DocumentStatus.UPLOADED,
            upload_time=upload_time,
            project_id=project_id
        )
        
        # Test model_dump to verify serialization
        data = response.model_dump()
        assert isinstance(data['document_id'], str)
        assert isinstance(data['project_id'], str)
        assert isinstance(data['upload_time'], str)


class TestDocumentProcessingStatus:
    """Test cases for DocumentProcessingStatus model."""

    def test_document_processing_status_creation(self):
        """Test successful creation of DocumentProcessingStatus."""
        project_id = uuid4()
        
        status = DocumentProcessingStatus(
            project_id=project_id,
            total_documents=10,
            processed_documents=7,
            processing_documents=2,
            failed_documents=1,
            processed_percentage=70.0,
            status="processing"
        )
        
        assert status.project_id == project_id
        assert status.total_documents == 10
        assert status.processed_documents == 7
        assert status.processing_documents == 2
        assert status.failed_documents == 1
        assert status.processed_percentage == 70.0
        assert status.status == "processing"


class TestDocumentMetadata:
    """Test cases for DocumentMetadata model."""

    def test_document_metadata_creation(self):
        """Test successful creation of DocumentMetadata."""
        document_id = uuid4()
        
        metadata = DocumentMetadata(
            document_id=document_id,
            filename="test.pdf",
            file_type="pdf",
            file_size=2048,
            page_count=5,
            extracted_text_length=1500,
            processing_time=2.5,
            status=DocumentStatus.COMPLETED
        )
        
        assert metadata.document_id == document_id
        assert metadata.filename == "test.pdf"
        assert metadata.file_type == "pdf"
        assert metadata.file_size == 2048
        assert metadata.page_count == 5
        assert metadata.extracted_text_length == 1500
        assert metadata.processing_time == 2.5
        assert metadata.status == DocumentStatus.COMPLETED
        assert metadata.error_message is None

    def test_document_metadata_with_error(self):
        """Test DocumentMetadata with error message."""
        document_id = uuid4()
        
        metadata = DocumentMetadata(
            document_id=document_id,
            filename="corrupted.pdf",
            file_type="pdf",
            file_size=1024,
            status=DocumentStatus.FAILED,
            error_message="File is corrupted"
        )
        
        assert metadata.status == DocumentStatus.FAILED
        assert metadata.error_message == "File is corrupted"


class TestBulkUploadResponse:
    """Test cases for BulkUploadResponse model."""

    def test_bulk_upload_response_creation(self):
        """Test successful creation of BulkUploadResponse."""
        project_id = uuid4()
        upload_time = datetime.utcnow()
        
        response = BulkUploadResponse(
            project_id=project_id,
            total_files=5,
            successful_uploads=4,
            failed_uploads=1,
            upload_time=upload_time,
            processing_initiated=True,
            uploaded_files=["file1.pdf", "file2.docx", "file3.txt", "file4.xlsx"],
            failed_files=["corrupted.pdf"]
        )
        
        assert response.project_id == project_id
        assert response.total_files == 5
        assert response.successful_uploads == 4
        assert response.failed_uploads == 1
        assert response.upload_time == upload_time
        assert response.processing_initiated is True
        assert len(response.uploaded_files) == 4
        assert len(response.failed_files) == 1
        assert "corrupted.pdf" in response.failed_files

    def test_bulk_upload_response_default_failed_files(self):
        """Test BulkUploadResponse with default empty failed_files."""
        project_id = uuid4()
        upload_time = datetime.utcnow()
        
        response = BulkUploadResponse(
            project_id=project_id,
            total_files=2,
            successful_uploads=2,
            failed_uploads=0,
            upload_time=upload_time,
            processing_initiated=True,
            uploaded_files=["file1.pdf", "file2.docx"]
        )
        
        assert response.failed_files == []

    def test_bulk_upload_response_serialization(self):
        """Test UUID and datetime serialization in BulkUploadResponse."""
        project_id = uuid4()
        upload_time = datetime.utcnow()
        
        response = BulkUploadResponse(
            project_id=project_id,
            total_files=1,
            successful_uploads=1,
            failed_uploads=0,
            upload_time=upload_time,
            processing_initiated=True,
            uploaded_files=["test.pdf"]
        )
        
        # Test model_dump to verify serialization
        data = response.model_dump()
        assert isinstance(data['project_id'], str)
        assert isinstance(data['upload_time'], str)