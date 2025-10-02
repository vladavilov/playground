"""Progress message models for Redis Pub/Sub updates.

Conforms to README Message Schemas for backlog generation progress.
"""

from datetime import datetime
from enum import Enum
from typing import Literal, Optional
from uuid import UUID, uuid4

from pydantic import BaseModel, Field, field_validator, field_serializer


class BacklogStatus(str, Enum):
    """Allowed status values for backlog generation workflow."""

    ANALYZING_REQUIREMENTS = "analyzing_requirements"
    RETRIEVING_CONTEXT = "retrieving_context"
    FETCHING_BACKLOG = "fetching_backlog"
    DRAFTING_BACKLOG = "drafting_backlog"
    MAPPING_DUPLICATES = "mapping_duplicates"
    EVALUATING = "evaluating"
    NEEDS_CLARIFICATION = "needs_clarification"
    COMPLETED = "completed"
    ERROR = "error"


class BacklogProgressMessage(BaseModel):
    """User-visible step updates for AI backlog generation progress."""

    message_type: Literal["ai_tasks_progress"] = Field(
        default="ai_tasks_progress",
        description="Fixed message type identifier",
    )
    project_id: UUID = Field(..., description="Project ID", json_schema_extra={"format": "uuid"})
    prompt_id: UUID = Field(
        default_factory=uuid4,
        description="Prompt identifier",
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

    @field_validator("score")
    @classmethod
    def _validate_score_bounds(cls, v: Optional[float]) -> Optional[float]:
        if v is None:
            return v
        if not (0.0 <= v <= 1.0):
            raise ValueError("score must be between 0 and 1")
        return v

    @field_serializer("project_id", "prompt_id", "message_id", when_used="always")
    def _serialize_uuid(self, v: UUID) -> str:  # noqa: D401
        return str(v)

    @field_serializer("timestamp", when_used="always")
    def _serialize_timestamp(self, v: datetime) -> str:  # noqa: D401
        return v.isoformat()


