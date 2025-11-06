"""Pydantic models for AI Workflow Service domain entities.

Implements Requirement, ClarificationQuestion, QuestionAnswer, and RequirementsBundle
as specified in the service README (Data Models section).
"""

from typing import List, Optional
from uuid import UUID
from pydantic import BaseModel, Field, field_serializer, field_validator


class Requirement(BaseModel):
    """Single requirement item with acceptance criteria and priority."""

    id: str = Field(..., description="Requirement identifier")
    title: str = Field(..., description="Short title")
    description: str = Field(..., description="Detailed requirement description")
    rationale: Optional[str] = Field(
        None, description="Optional rationale explaining the requirement"
    )
    acceptance_criteria: List[str] = Field(
        ..., description="List of testable acceptance criteria"
    )
    priority: str = Field(
        ..., description="Priority label (e.g., Must/Should/Could/Won’t)"
    )


class ClarificationQuestion(BaseModel):
    """Question to clarify missing details to improve evaluation score."""

    id: str = Field(..., description="Question identifier")
    text: str = Field(..., description="The question text presented to the user")
    options: Optional[List[str]] = Field(
        None, description="Optional multiple-choice options to accelerate responses"
    )
    expected_impact: str = Field(
        ..., description="Short reason how this answer will raise the score"
    )
    axis: Optional[str] = Field(
        None, description='One of "precision" | "grounding" | "completeness" | "feasibility"'
    )
    priority: Optional[int] = Field(
        None, ge=1, description="Priority (1 = highest)"
    )
    expected_score_gain: Optional[float] = Field(
        None, ge=0.0, le=1.0, description="Expected score gain in [0,1]"
    )
    targets: Optional[List[str]] = Field(
        None, description="Ids of affected requirements/intents"
    )


class QuestionAnswer(BaseModel):
    """User answer to a clarification question."""

    id: str = Field(..., description="Matches ClarificationQuestion.id")
    answer: str = Field(..., description="Answer text")


class RequirementsBundle(BaseModel):
    """Aggregated requirements bundle returned by the workflow endpoints."""

    prompt_id: UUID = Field(..., description="Prompt identifier", json_schema_extra={"format": "uuid"})
    project_id: UUID = Field(..., description="Project identifier", json_schema_extra={"format": "uuid"})
    business_requirements: List[Requirement] = Field(
        ..., description="List of business requirements"
    )
    functional_requirements: List[Requirement] = Field(
        ..., description="List of functional requirements"
    )
    assumptions: List[str] = Field(..., description="List of assumptions")
    risks: List[str] = Field(..., description="List of risks")
    score: float = Field(..., ge=0.0, le=1.0, description="Score in [0,1]")
    clarification_questions: Optional[List[ClarificationQuestion]] = Field(
        None, description="Present if score < target"
    )

    @field_serializer("prompt_id", "project_id", when_used="always")
    def _serialize_uuid(self, v: UUID) -> str:  # noqa: D401
        return str(v)

    @field_validator("score")
    @classmethod
    def _validate_score_bounds(cls, v: float) -> float:
        """Ensure score strictly fits within [0,1]. Field ge/le handle, but keep explicit."""
        if not (0.0 <= v <= 1.0):
            raise ValueError("score must be between 0 and 1")
        return v


class EnhanceRequirementRequest(BaseModel):
    """Request for enhancing a single requirement with AI."""

    project_id: UUID = Field(..., description="Project identifier")
    requirement_id: str = Field(..., description="Requirement identifier")
    requirement_type: str = Field(
        ..., description='Requirement type: "business" or "functional"'
    )
    current_content: dict = Field(
        ..., description="Current requirement content (title, description, acceptance_criteria, etc.)"
    )

    @field_validator("requirement_type")
    @classmethod
    def _validate_requirement_type(cls, v: str) -> str:
        """Ensure requirement_type is either 'business' or 'functional'."""
        if v not in ("business", "functional"):
            raise ValueError("requirement_type must be 'business' or 'functional'")
        return v



