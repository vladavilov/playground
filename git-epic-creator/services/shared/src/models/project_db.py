"""
Base models for SQLAlchemy ORM.
"""

import uuid
from sqlalchemy import Column, String, DateTime, Text, Float, func
from sqlalchemy.dialects.postgresql import UUID
from sqlalchemy.orm import declarative_base

Base = declarative_base()

class BaseModel(Base):
    """
    Base model for all SQLAlchemy models.
    Provides common fields and functionality.
    """
    __abstract__ = True

    id = Column(UUID(as_uuid=True), primary_key=True, default=uuid.uuid4)
    created_at = Column(DateTime, server_default=func.now(), nullable=False)
    updated_at = Column(DateTime, server_default=func.now(), onupdate=func.now(), nullable=False)
    created_by = Column(String(255), nullable=False)

    def to_dict(self):
        """
        Convert model instance to dictionary.
        
        Returns:
            dict: Model as dictionary
        """
        return {c.name: getattr(self, c.name) for c in self.__table__.columns}

    @classmethod
    def from_dict(cls, data):
        """
        Create model instance from dictionary.
        
        Args:
            data: Dictionary with model data
            
        Returns:
            BaseModel: Model instance
        """
        return cls(**{k: v for k, v in data.items() if k in cls.__table__.columns.keys()})

class Project(BaseModel):
    """
    SQLAlchemy model for the 'projects' table.
    This schema is based on the system_architecture.md document.
    Extended to support document processing with PROCESSING status and processed_pct field.
    """
    __tablename__ = "projects"

    # Override id from BaseModel to use UUID type
    id = Column(UUID(as_uuid=True), primary_key=True, default=uuid.uuid4)
    name = Column(String(255), nullable=False)
    description = Column(Text, nullable=True)
    gitlab_url = Column(Text, nullable=True)
    gitlab_repository_url = Column(Text, nullable=True)
    status = Column(String(50), default='active')  # Supports: active, processing, inactive
    processed_pct = Column(Float, default=0.0, nullable=False)  # Percentage (0.0-100.0)
    created_by = Column(String(255), nullable=False)  # Stores Azure AD user ID

    # These override the BaseModel columns to use default instead of server_default
    created_at = Column(DateTime, default=func.now())
    updated_at = Column(DateTime, default=func.now(), onupdate=func.now())

    def __repr__(self):
        """String representation of Project instance."""
        return f"<Project(id={self.id}, name='{self.name}')>"


class ProjectMember(BaseModel):
    """
    SQLAlchemy model for the 'project_members' table.
    Implements project member management for Requirements 6.7 and 8.2.
    """
    __tablename__ = "project_members"

    # Override id from BaseModel to use UUID type
    id = Column(UUID(as_uuid=True), primary_key=True, default=uuid.uuid4)
    project_id = Column(UUID(as_uuid=True), nullable=False)
    user_id = Column(String(255), nullable=False)  # Azure AD user ID
    role = Column(String(50), nullable=False)  # Admin, Project Manager, Contributor
    created_by = Column(String(255), nullable=False)  # Who added this member

    # These override the BaseModel columns to use default instead of server_default
    created_at = Column(DateTime, default=func.now())
    updated_at = Column(DateTime, default=func.now(), onupdate=func.now())

    def __repr__(self):
        """String representation of ProjectMember instance."""
        return f"<ProjectMember(project_id={self.project_id}, user_id='{self.user_id}', role='{self.role}')>"
