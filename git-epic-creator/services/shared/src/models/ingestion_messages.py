"""
Pydantic models for ingestion job messages shared across services.
"""

from __future__ import annotations

from uuid import UUID

from pydantic import BaseModel, Field, field_validator, field_serializer


class IngestionTriggerMessage(BaseModel):
    """
    New trigger message shape for `ingestion.trigger` stream: no documents included.
    """

    job_id: str = Field(..., description="Unique job identifier")
    project_id: UUID = Field(..., description="Project ID (UUID)")
    attempts: int = Field(default=0, ge=0, description="Delivery attempts for backoff/DLQ handling")

    @field_validator("job_id")
    @classmethod
    def validate_job_id_non_empty(cls, value: str) -> str:
        if value is None or not isinstance(value, str) or not value.strip():
            raise ValueError("job_id must be a non-empty string")
        return value

    @field_serializer("project_id", when_used="always")
    def _serialize_uuid(self, value: UUID) -> str:  # noqa: D401
        return str(value)


