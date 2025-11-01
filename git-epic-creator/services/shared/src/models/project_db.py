"""SQLAlchemy ORM base models."""

import uuid
from sqlalchemy import Column, String, DateTime, Text, Float, func
from sqlalchemy.dialects.postgresql import UUID, ARRAY
from sqlalchemy.orm import declarative_base

Base = declarative_base()

class BaseModel(Base):
    """Common fields for ORM models."""
    __abstract__ = True

    id = Column(UUID(as_uuid=True), primary_key=True, default=uuid.uuid4)
    created_at = Column(DateTime, server_default=func.now(), nullable=False)
    updated_at = Column(DateTime, server_default=func.now(), onupdate=func.now(), nullable=False)
    created_by = Column(String(255), nullable=False)

class Project(BaseModel):
    """`projects` table."""
    __tablename__ = "projects"

    name = Column(String(255), nullable=False)
    description = Column(Text, nullable=True)
    
    # GitLab Repository (Source Code) - single URL, no resolution needed
    gitlab_repository_url = Column(Text, nullable=True)  # Git clone URL (SSH/HTTPS)
    
    # GitLab Backlog Projects (Issues/Epics) - multiple projects, requires resolution
    gitlab_backlog_project_ids = Column(ARRAY(String(255)), nullable=True, default=list)  # Resolved project IDs
    gitlab_backlog_project_urls = Column(ARRAY(Text), nullable=True, default=list)  # Project URLs for backlog
    
    status = Column(String(50), default='active')  # Supports: active, processing, inactive
    processed_pct = Column(Float, default=0.0, nullable=False)  # Percentage (0.0-100.0)
    created_by = Column(String(255), nullable=False)  # Stores Azure AD user ID

    # These override the BaseModel columns to use default instead of server_default
    created_at = Column(DateTime, default=func.now())
    updated_at = Column(DateTime, default=func.now(), onupdate=func.now())

    def __repr__(self):
        return f"<Project(id={self.id}, name='{self.name}')>"


class ProjectMember(BaseModel):
    """`project_members` table."""
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
        return f"<ProjectMember(project_id={self.project_id}, user_id='{self.user_id}', role='{self.role}')>"
