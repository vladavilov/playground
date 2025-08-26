"""
Tests for document schemas in shared models.
"""

import pytest
from datetime import datetime, timezone
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

    def test_document_status_enum_values(self):
        """Test that DocumentStatus has correct enum values."""
        # Assert
        assert DocumentStatus.UPLOADED.value == "uploaded"
        assert DocumentStatus.PROCESSING.value == "processing"
        assert DocumentStatus.COMPLETED.value == "completed"
        assert DocumentStatus.FAILED.value == "failed"

    def test_document_status_enum_membership(self):
        """Test DocumentStatus enum membership."""
        # Assert
        assert "uploaded" in [status.value for status in DocumentStatus]
        assert "processing" in [status.value for status in DocumentStatus]
        assert "completed" in [status.value for status in DocumentStatus]
        assert "failed" in [status.value for status in DocumentStatus]
        assert "invalid_status" not in [status.value for status in DocumentStatus]

    def test_document_status_string_comparison(self):
        """Test DocumentStatus string comparison functionality."""
        # Assert
        assert DocumentStatus.UPLOADED == "uploaded"
        assert DocumentStatus.PROCESSING == "processing"
        assert DocumentStatus.COMPLETED == "completed"
        assert DocumentStatus.FAILED == "failed"


class TestDocumentUploadResponse:
    """Test cases for DocumentUploadResponse model."""

    @pytest.fixture
    def sample_upload_response_data(self):
        """Sample data for DocumentUploadResponse."""
        return {
            "document_id": uuid4(),
            "filename": "test.pdf",
            "file_size": 1024,
            "status": DocumentStatus.UPLOADED,
            "upload_time": datetime.now(timezone.utc),
            "project_id": uuid4()
        }

    def test_document_upload_response_creation(self, sample_upload_response_data):
        """Test successful creation of DocumentUploadResponse."""
        # Act
        response = DocumentUploadResponse(**sample_upload_response_data)
        
        # Assert
        assert response.document_id == sample_upload_response_data["document_id"]
        assert response.filename == sample_upload_response_data["filename"]
        assert response.file_size == sample_upload_response_data["file_size"]
        assert response.status == sample_upload_response_data["status"]
        assert response.upload_time == sample_upload_response_data["upload_time"]
        assert response.project_id == sample_upload_response_data["project_id"]

    def test_document_upload_response_serialization(self, sample_upload_response_data):
        """Test UUID and datetime serialization."""
        # Arrange
        response = DocumentUploadResponse(**sample_upload_response_data)
        
        # Act
        data = response.model_dump()
        
        # Assert
        assert isinstance(data['document_id'], str)
        assert isinstance(data['project_id'], str)
        assert isinstance(data['upload_time'], str)
        assert data['document_id'] == str(sample_upload_response_data["document_id"])
        assert data['project_id'] == str(sample_upload_response_data["project_id"])

    def test_document_upload_response_validation_errors(self):
        """Test validation errors for DocumentUploadResponse."""
        # Test missing required fields
        with pytest.raises(ValidationError) as exc_info:
            DocumentUploadResponse()
        
        errors = exc_info.value.errors()
        required_fields = {"document_id", "filename", "file_size", "status", "upload_time", "project_id"}
        error_fields = {error["loc"][0] for error in errors if error["type"] == "missing"}
        assert required_fields.issubset(error_fields)

    def test_document_upload_response_invalid_uuid(self):
        """Test DocumentUploadResponse with invalid UUID."""
        # Arrange
        invalid_data = {
            "document_id": "not-a-uuid",
            "filename": "test.pdf",
            "file_size": 1024,
            "status": DocumentStatus.UPLOADED,
            "upload_time": datetime.now(timezone.utc),
            "project_id": uuid4()
        }
        
        # Act & Assert
        with pytest.raises(ValidationError) as exc_info:
            DocumentUploadResponse(**invalid_data)
        
        errors = exc_info.value.errors()
        uuid_errors = [e for e in errors if "uuid" in e["type"]]
        assert len(uuid_errors) > 0

    @pytest.mark.parametrize("file_size", [-1, 0])
    def test_document_upload_response_invalid_file_size(self, file_size, sample_upload_response_data):
        """Test DocumentUploadResponse with invalid file sizes."""
        # Arrange
        sample_upload_response_data["file_size"] = file_size
        
        # Act & Assert
        if file_size < 0:
            with pytest.raises(ValidationError):
                DocumentUploadResponse(**sample_upload_response_data)
        else:
            # file_size = 0 should be valid (empty file)
            response = DocumentUploadResponse(**sample_upload_response_data)
            assert response.file_size == 0


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
        upload_time = datetime.now(timezone.utc)
        
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
        upload_time = datetime.now(timezone.utc)
        
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
        upload_time = datetime.now(timezone.utc)
        
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