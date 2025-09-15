"""
Pydantic models for API operations.
"""

from enum import Enum
from typing import Optional
from uuid import UUID
from datetime import datetime
from pydantic import BaseModel, ConfigDict, Field, AnyUrl, field_validator, model_validator


class ProjectStatus(str, Enum):
    """Valid project status values."""
    ACTIVE = "active"
    INACTIVE = "inactive"
    ARCHIVED = "archived"
    PROCESSING = "processing"
    RAG_PROCESSING = "rag_processing"
    RAG_READY = "rag_ready"
    RAG_FAILED = "rag_failed"


class ProjectSet(BaseModel):
    """
    Pydantic model for project creation and update operations.
    Used for both create and update requests with optional fields for updates.
    """

    name: str = Field(
        ...,
        min_length=1,
        max_length=255,
        description="Project name (1-255 characters)"
    )

    @field_validator('name')
    @classmethod
    def validate_name_not_whitespace_only(cls, v: str) -> str:
        """Ensure name is not whitespace-only."""
        if v.strip() == "":
            raise ValueError("Name cannot be whitespace only")
        return v
    description: Optional[str] = Field(
        None,
        description="Optional project description"
    )
    gitlab_url: Optional[AnyUrl] = Field(
        None,
        description="GitLab project URL"
    )
    gitlab_repository_url: Optional[AnyUrl] = Field(
        None,
        description="GitLab repository URL"
    )
    status: ProjectStatus = Field(
        default=ProjectStatus.ACTIVE,
        description="Project status"
    )


class ProjectResponse(BaseModel):
    """
    Pydantic model for project API responses.
    Configured for SQLAlchemy integration with from_attributes=True.
    """

    model_config = ConfigDict(from_attributes=True)

    id: UUID = Field(description="Unique project identifier")
    name: str = Field(description="Project name")
    description: Optional[str] = Field(description="Project description")
    gitlab_url: Optional[str] = Field(description="GitLab project URL")
    gitlab_repository_url: Optional[str] = Field(description="GitLab repository URL")
    status: str = Field(description="Project status")
    created_by: str = Field(description="User ID who created the project")
    created_at: datetime = Field(description="Project creation timestamp")
    updated_at: datetime = Field(description="Project last update timestamp")


class ProjectMemberRole(str, Enum):
    """Valid project member role values."""
    ADMIN = "Admin"
    PROJECT_MANAGER = "Project Manager"
    CONTRIBUTOR = "Contributor"


class ProjectMemberSet(BaseModel):
    """
    Pydantic model for project member creation operations.
    Used for adding members to projects.
    """

    user_id: str = Field(
        ...,
        min_length=1,
        max_length=255,
        description="Azure AD user ID"
    )
    role: ProjectMemberRole = Field(
        ...,
        description="Project member role"
    )


class ProjectMemberResponse(BaseModel):
    """
    Pydantic model for project member API responses.
    Configured for SQLAlchemy integration with from_attributes=True.
    """

    model_config = ConfigDict(from_attributes=True)

    id: UUID = Field(description="Unique project member identifier")
    project_id: UUID = Field(description="Project identifier")
    user_id: str = Field(description="Azure AD user ID")
    role: str = Field(description="Project member role")
    created_by: str = Field(description="User ID who added this member")
    created_at: datetime = Field(description="Member addition timestamp")
    updated_at: datetime = Field(description="Member last update timestamp")


class ProjectProgressUpdateRequest(BaseModel):
    """
    Pydantic model for project status update requests.
    Handles both progress updates (with counts) and status resets (without counts).
    Implements Requirements 2.1 and 2.2.
    """

    status: ProjectStatus = Field(
        default=ProjectStatus.ACTIVE,
        description="Project status"
    )
    process_step: Optional[str] = Field(
        None,
        description="Optional human-readable pipeline step for UI progress"
    )
    processed_count: Optional[int] = Field(
        None,
        ge=0,
        description="Deprecated: prefer processed_pct; kept for backward compatibility"
    )
    total_count: Optional[int] = Field(
        None,
        gt=0,
        description="Deprecated: prefer processed_pct; kept for backward compatibility"
    )
    processed_pct: Optional[float] = Field(
        None,
        ge=0,
        le=100,
        description="Overall normalized progress percent (0-100)."
    )
    error_message: Optional[str] = Field(
        None,
        min_length=1,
        description="Error message for failed processing"
    )

    @field_validator('error_message')
    @classmethod
    def validate_error_message_not_empty(cls, v: Optional[str]) -> Optional[str]:
        """Ensure error_message is not whitespace-only when provided."""
        if v is not None and v.strip() == "":
            raise ValueError("error_message cannot be empty")
        return v.strip() if v is not None else None

    @model_validator(mode='after')
    def validate_processing_requirements(self) -> 'ProjectProgressUpdateRequest':
        """Validate requirements for PROCESSING status and count relationships."""
        errors = []

        # Check PROCESSING status requirements (but allow RAG_PROCESSING without counts).
        # Accept processed_pct instead of counts for PROCESSING.
        if self.status == ProjectStatus.PROCESSING:
            if self.processed_pct is None and (self.processed_count is None or self.total_count is None):
                errors.append("Either processed_pct or both processed_count and total_count are required when status is PROCESSING")
        
        # Check that processed_count doesn't exceed total_count
        if (self.processed_count is not None and
            self.total_count is not None and
            self.processed_count > self.total_count):
            errors.append("processed_count cannot exceed total_count")

        if errors:
            # Raise the first error (Pydantic will only show one anyway)
            raise ValueError(errors[0])

        return self
