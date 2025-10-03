"""Internal agent/expert data models for backlog generation workflow."""

from typing import List, Optional, Dict, Any
from pydantic import BaseModel, Field

from .backlog_models import Epic, Task


class RequirementsAnalysis(BaseModel):
    """Output from RequirementsAnalyst expert."""

    requirements_text: str = Field(..., description="Normalized requirements text")
    intents: List[str] = Field(default_factory=list, description="Extracted intents")
    entities: List[str] = Field(default_factory=list, description="Domain entities")
    constraints: List[str] = Field(default_factory=list, description="Constraints and SLAs")


class RetrievedContext(BaseModel):
    """Output from ContextRetriever expert."""

    context_answer: str = Field(..., description="Context summary from GraphRAG")
    key_facts: List[str] = Field(default_factory=list, description="Key technical facts")
    citations: List[str] = Field(default_factory=list, description="Citation references")


class BacklogDraft(BaseModel):
    """Output from BacklogEngineer expert."""

    epics: List[Epic] = Field(default_factory=list, description="Generated epics")
    assumptions: List[str] = Field(default_factory=list, description="Assumptions made")
    risks: List[str] = Field(default_factory=list, description="Identified risks")


class DuplicateMappings(BaseModel):
    """Output from DuplicateMapper expert."""

    enriched_epics: List[Epic] = Field(
        default_factory=list,
        description="Epics enriched with similar matches",
    )
    stats: Dict[str, Any] = Field(
        default_factory=dict,
        description="Statistics: total_matches, avg_similarity, etc.",
    )


class AuditFindings(BaseModel):
    """Output from ConsistencyAuditor expert."""

    issues: List[str] = Field(default_factory=list, description="Identified issues")
    suggestions: List[str] = Field(default_factory=list, description="Improvement suggestions")
    overlaps: List[str] = Field(default_factory=list, description="Overlapping tasks/epics")


class EvaluationReport(BaseModel):
    """Output from Evaluator expert."""

    score: float = Field(..., ge=0.0, le=1.0, description="Overall quality score")
    component_scores: Dict[str, float] = Field(
        default_factory=dict,
        description="Scores per axis: coverage, specificity, feasibility, duplication",
    )
    rationale: str = Field(..., description="Brief explanation of scoring")
    gaps: List[str] = Field(default_factory=list, description="Top identified gaps")


class ClarificationPlan(BaseModel):
    """Output from ClarificationStrategist expert."""

    questions: List[Dict[str, str]] = Field(
        default_factory=list,
        description="Questions with id and text",
    )
    focus_areas: List[str] = Field(
        default_factory=list,
        description="Areas needing clarification: coverage/specificity/feasibility/duplication",
    )


