"""
Pydantic schemas for document processing operations.
"""

from typing import Optional, List
from uuid import UUID
from datetime import datetime
from enum import Enum
from pydantic import BaseModel, Field, field_serializer


class DocumentStatus(str, Enum):
    """Document processing status enumeration."""
    UPLOADED = "uploaded"
    PROCESSING = "processing"
    COMPLETED = "completed"
    FAILED = "failed"


class DocumentUploadResponse(BaseModel):
    """Response model for document upload."""
    document_id: UUID = Field(..., description="Unique document identifier")
    filename: str = Field(..., description="Original filename")
    file_size: int = Field(..., ge=0, description="File size in bytes")
    status: DocumentStatus = Field(..., description="Processing status")
    upload_time: datetime = Field(..., description="Upload timestamp")
    project_id: UUID = Field(..., description="Associated project ID")

    @field_serializer('document_id', 'project_id')
    def serialize_uuid(self, value: UUID) -> str:
        """Serialize UUID fields to string."""
        return str(value)

    @field_serializer('upload_time')
    def serialize_datetime(self, value: datetime) -> str:
        """Serialize datetime fields to ISO format string."""
        return value.isoformat()


class DocumentProcessingStatus(BaseModel):
    """Response model for document processing status."""
    project_id: UUID = Field(..., description="Project ID")
    total_documents: int = Field(..., description="Total number of documents")
    processed_documents: int = Field(..., description="Number of processed documents")
    processing_documents: int = Field(..., description="Number of documents currently processing")
    failed_documents: int = Field(..., description="Number of failed documents")
    processed_percentage: float = Field(..., description="Processing completion percentage")
    status: str = Field(..., description="Overall processing status")

    @field_serializer('project_id')
    def serialize_uuid(self, value: UUID) -> str:
        """Serialize UUID fields to string."""
        return str(value)


class DocumentMetadata(BaseModel):
    """Document metadata model."""
    document_id: UUID = Field(..., description="Document ID")
    filename: str = Field(..., description="Original filename")
    file_type: str = Field(..., description="File type/extension")
    file_size: int = Field(..., description="File size in bytes")
    page_count: Optional[int] = Field(None, description="Number of pages (for supported formats)")
    extracted_text_length: Optional[int] = Field(None, description="Length of extracted text")
    processing_time: Optional[float] = Field(None, description="Processing time in seconds")
    status: DocumentStatus = Field(..., description="Processing status")
    error_message: Optional[str] = Field(None, description="Error message if processing failed")

    @field_serializer('document_id')
    def serialize_uuid(self, value: UUID) -> str:
        """Serialize UUID fields to string."""
        return str(value)


class BulkUploadResponse(BaseModel):
    """Response model for bulk document upload."""
    project_id: UUID = Field(..., description="Project ID")
    total_files: int = Field(..., description="Total number of files uploaded")
    successful_uploads: int = Field(..., description="Number of successful uploads")
    failed_uploads: int = Field(..., description="Number of failed uploads")
    upload_time: datetime = Field(..., description="Upload timestamp")
    processing_initiated: bool = Field(..., description="Whether background processing was initiated")
    uploaded_files: List[str] = Field(..., description="List of successfully uploaded filenames")
    failed_files: List[str] = Field(default_factory=list, description="List of failed filenames")

    @field_serializer('project_id')
    def serialize_uuid(self, value: UUID) -> str:
        """Serialize UUID fields to string."""
        return str(value)

    @field_serializer('upload_time')
    def serialize_datetime(self, value: datetime) -> str:
        """Serialize datetime fields to ISO format string."""
        return value.isoformat()