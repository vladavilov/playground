"""SQLAlchemy ORM base models."""

import uuid
from sqlalchemy import Column, String, DateTime, Text, Float, func, UniqueConstraint, Index
from sqlalchemy.dialects.postgresql import UUID, ARRAY, JSONB
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


class ProjectRepoIndex(BaseModel):
    """`project_repo_indexes` table.

    Stores a deterministic repo manifest (`repo_index_json`) keyed by (project_id, repo_fingerprint).
    This is NOT a portable graph export and NOT the graph golden source; Neo4j remains authoritative
    for parsed units and relationships.
    """

    __tablename__ = "project_repo_indexes"

    project_id = Column(UUID(as_uuid=True), nullable=False)
    repo_fingerprint = Column(Text, nullable=False)
    repo_index_json = Column(JSONB, nullable=False)
    content_sha256 = Column(Text, nullable=False)

    __table_args__ = (
        UniqueConstraint("project_id", "repo_fingerprint", name="uq_project_repo_indexes_project_fingerprint"),
        Index("ix_project_repo_indexes_project_fingerprint", "project_id", "repo_fingerprint"),
    )

    # Override timestamps to match existing patterns in this module
    created_at = Column(DateTime, default=func.now())
    updated_at = Column(DateTime, default=func.now(), onupdate=func.now())

    def __repr__(self):
        return f"<ProjectRepoIndex(project_id={self.project_id}, repo_fingerprint='{self.repo_fingerprint}')>"
