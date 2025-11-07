"""
Unified progress message models for Redis Pub/Sub updates.

Contains both backlog and workflow progress messages used by
`ai_tasks_service` and `ai_requirements_service` to avoid duplication.
"""

from datetime import datetime
from enum import Enum
from typing import Literal, Optional
from uuid import UUID, uuid4

from pydantic import BaseModel, Field, field_validator, field_serializer


class BacklogStatus(str, Enum):
    """Allowed status values for backlog generation workflow."""

    ANALYZING_REQUIREMENTS = "analyzing_requirements"
    ANALYZING_ITEM = "analyzing_item"  # Single-item enhancement
    RETRIEVING_CONTEXT = "retrieving_context"
    FETCHING_BACKLOG = "fetching_backlog"
    DRAFTING_BACKLOG = "drafting_backlog"
    ENHANCING_ITEM = "enhancing_item"  # Single-item enhancement
    MAPPING_DUPLICATES = "mapping_duplicates"
    EVALUATING = "evaluating"
    NEEDS_CLARIFICATION = "needs_clarification"
    COMPLETED = "completed"
    WARNING = "warning"
    ERROR = "error"


class BacklogProgressMessage(BaseModel):
    """User-visible step updates for AI backlog generation progress."""

    message_type: Literal["ai_tasks_progress"] = Field(
        default="ai_tasks_progress",
        description="Fixed message type identifier",
    )
    project_id: UUID = Field(..., description="Project ID", json_schema_extra={"format": "uuid"})
    prompt_id: Optional[UUID] = Field(
        default_factory=uuid4,
        description="Prompt identifier (optional for single-item enhancement)",
        json_schema_extra={"format": "uuid"},
    )
    iteration: Optional[int] = Field(
        None, ge=1, description="Iteration number (>=1) for iterative loops"
    )
    status: BacklogStatus = Field(..., description="Backlog generation status")
    score: Optional[float] = Field(
        None, ge=0.0, le=1.0, description="Optional score in [0,1]"
    )
    thought_summary: str = Field(
        ..., description="Concise progress summary; do not include raw chain-of-thought"
    )
    details_md: Optional[str] = Field(
        None, description="Markdown-formatted step outcomes"
    )
    message_id: UUID = Field(
        default_factory=uuid4, description="Unique message id", json_schema_extra={"format": "uuid"}
    )
    timestamp: datetime = Field(
        default_factory=datetime.now,
        description="Event timestamp (ISO8601)",
    )
    # Enhancement mode fields (optional, for single-item operations)
    item_id: Optional[str] = Field(
        None, description="Item identifier for single-item enhancement operations"
    )
    item_type: Optional[str] = Field(
        None, description="Item type: 'epic', 'task' for single-item operations"
    )
    enhancement_mode: Optional[bool] = Field(
        None, description="True if this is a single-item enhancement operation"
    )

    @field_validator("score")
    @classmethod
    def _validate_score_bounds(cls, v: Optional[float]) -> Optional[float]:
        if v is None:
            return v
        if not (0.0 <= v <= 1.0):
            raise ValueError("score must be between 0 and 1")
        return v

    @field_serializer("project_id", "prompt_id", "message_id", when_used="always")
    def _serialize_uuid(self, v: Optional[UUID]) -> str | None:  # noqa: D401
        return str(v) if v is not None else None

    @field_serializer("timestamp", when_used="always")
    def _serialize_timestamp(self, v: datetime) -> str:  # noqa: D401
        return v.isoformat()


class WorkflowStatus(str, Enum):
    ANALYZING_PROMPT = "analyzing_prompt"
    ANALYZING_ITEM = "analyzing_item"  # Single-item enhancement
    RETRIEVING_CONTEXT = "retrieving_context"
    DRAFTING_REQUIREMENTS = "drafting_requirements"
    ENHANCING_ITEM = "enhancing_item"  # Single-item enhancement
    EVALUATING = "evaluating"
    NEEDS_CLARIFICATION = "needs_clarification"
    COMPLETED = "completed"
    ERROR = "error"


class WorkflowProgressMessage(BaseModel):
    """User-visible step updates for AI workflow progress."""

    message_type: Literal["ai_requirements_progress"] = Field(
        default="ai_requirements_progress",
        description="Fixed message type identifier",
    )
    project_id: UUID = Field(..., description="Project ID", json_schema_extra={"format": "uuid"})
    prompt_id: Optional[UUID] = Field(
        default_factory=uuid4,
        description="Prompt identifier (optional for single-item enhancement)",
        json_schema_extra={"format": "uuid"}
    )
    iteration: Optional[int] = Field(
        None, ge=1, description="Iteration number (>=1) for iterative loops"
    )
    status: WorkflowStatus = Field(..., description="Workflow status")
    score: Optional[float] = Field(
        None, ge=0.0, le=1.0, description="Optional score in [0,1]"
    )
    thought_summary: str = Field(
        ..., description="Concise progress summary; do not include raw chain-of-thought"
    )
    details_md: Optional[str] = Field(
        None, description="Markdown-formatted step outcomes; do not include raw chain-of-thought"
    )
    message_id: UUID = Field(
        default_factory=uuid4, description="Unique message id", json_schema_extra={"format": "uuid"}
    )
    timestamp: datetime = Field(
        default_factory=datetime.now,
        description="Event timestamp (ISO8601)",
    )
    # Enhancement mode fields (optional, for single-item operations)
    item_id: Optional[str] = Field(
        None, description="Item identifier for single-item enhancement operations"
    )
    item_type: Optional[str] = Field(
        None, description="Item type: 'requirement' for single-item operations"
    )
    enhancement_mode: Optional[bool] = Field(
        None, description="True if this is a single-item enhancement operation"
    )

    @field_validator("score")
    @classmethod
    def _validate_score_bounds(cls, v: Optional[float]) -> Optional[float]:
        if v is None:
            return v
        if not (0.0 <= v <= 1.0):
            raise ValueError("score must be between 0 and 1")
        return v

    @field_serializer("project_id", "prompt_id", "message_id", when_used="always")
    def _serialize_uuid(self, v: Optional[UUID]) -> str | None:  # noqa: D401
        return str(v) if v is not None else None

    @field_serializer("timestamp", when_used="always")
    def _serialize_timestamp(self, v: datetime) -> str:  # noqa: D401
        return v.isoformat()


class RetrievalStatus(str, Enum):
    """Allowed status values for retrieval workflow."""
    
    INITIALIZING = "initializing"
    EXPANDING_QUERY = "expanding_query"
    RETRIEVING_COMMUNITIES = "retrieving_communities"
    EXECUTING_FOLLOWUP = "executing_followup"
    AGGREGATING_RESULTS = "aggregating_results"
    COMPLETED = "completed"
    ERROR = "error"


class RetrievalProgressMessage(BaseModel):
    """User-visible step updates for retrieval progress (DRIFT search).
    
    Published during neo4j_retrieval_service operations to show:
    - Query expansion (HyDE)
    - Community retrieval from graph
    - Follow-up question execution
    - Result aggregation
    """

    message_type: Literal["retrieval_progress"] = Field(
        default="retrieval_progress",
        description="Fixed message type identifier",
    )
    project_id: UUID = Field(
        ..., 
        description="Project ID", 
        json_schema_extra={"format": "uuid"}
    )
    prompt_id: Optional[UUID] = Field(
        None,
        description="Parent workflow prompt_id for UI box tracking",
        json_schema_extra={"format": "uuid"},
    )
    retrieval_id: UUID = Field(
        default_factory=uuid4,
        description="Unique retrieval session identifier",
        json_schema_extra={"format": "uuid"},
    )
    phase: RetrievalStatus = Field(
        ..., 
        description="Current retrieval phase"
    )
    progress_pct: Optional[float] = Field(
        None, 
        ge=0.0, 
        le=100.0, 
        description="Progress percentage (0-100)"
    )
    thought_summary: str = Field(
        ..., 
        description="Concise progress summary for UI display"
    )
    details_md: Optional[str] = Field(
        None, 
        description="Markdown-formatted step details"
    )
    message_id: UUID = Field(
        default_factory=uuid4, 
        description="Unique message id", 
        json_schema_extra={"format": "uuid"}
    )
    timestamp: datetime = Field(
        default_factory=datetime.now,
        description="Event timestamp (ISO8601)",
    )

    @field_validator("progress_pct")
    @classmethod
    def _validate_progress_bounds(cls, v: Optional[float]) -> Optional[float]:
        if v is None:
            return v
        if not (0.0 <= v <= 100.0):
            raise ValueError("progress_pct must be between 0 and 100")
        return v

    @field_serializer("project_id", "prompt_id", "retrieval_id", "message_id", when_used="always")
    def _serialize_uuid(self, v: Optional[UUID]) -> str | None:  # noqa: D401
        return str(v) if v is not None else None

    @field_serializer("timestamp", when_used="always")
    def _serialize_timestamp(self, v: datetime) -> str:  # noqa: D401
        return v.isoformat()


