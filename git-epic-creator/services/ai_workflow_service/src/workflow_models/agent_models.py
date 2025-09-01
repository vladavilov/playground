from typing import List, Optional
from pydantic import BaseModel, Field

from workflow_models.workflow_models import Requirement, ClarificationQuestion


class PromptAnalysis(BaseModel):
    prompt: str
    intents: List[str] = Field(default_factory=list)


class RetrievedContext(BaseModel):
    citations: List[str] = Field(default_factory=list)
    notes: Optional[str] = None


class DraftRequirements(BaseModel):
    business_requirements: List[Requirement] = Field(default_factory=list)
    functional_requirements: List[Requirement] = Field(default_factory=list)
    assumptions: List[str] = Field(default_factory=list)
    risks: List[str] = Field(default_factory=list)


class AuditFindings(BaseModel):
    issues: List[str] = Field(default_factory=list)
    suggestions: List[str] = Field(default_factory=list)
    llm_critique_severity: float = 0.0
    component_scores: dict[str, float] = Field(default_factory=dict)


class ScoreReport(BaseModel):
    score: float = 0.0
    component_scores: dict[str, float] = Field(default_factory=dict)


class ClarificationPlan(BaseModel):
    questions: List[ClarificationQuestion] = Field(default_factory=list)


