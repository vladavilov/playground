"""Backlog data models for AI Tasks Service (moved from local models)."""

from typing import List, Optional
from pydantic import BaseModel, Field


class SimilarMatch(BaseModel):
    """Represents a similar work item from GitLab."""

    kind: str = Field(..., description="Type of work item: 'epic' or 'issue'")
    id: str = Field(..., description="GitLab work item ID")
    status: Optional[str] = Field(None, description="Work item status (e.g., open, closed)")
    similarity: Optional[float] = Field(None, ge=0.0, le=1.0, description="Cosine similarity score")
    url: Optional[str] = Field(None, description="GitLab web URL for the work item")


class Task(BaseModel):
    """Represents a task within an epic."""

    id: str = Field(..., description="Task identifier")
    title: str = Field(..., description="Task title")
    description: str = Field(..., description="Task description")
    acceptance_criteria: List[str] = Field(
        default_factory=list,
        description="List of acceptance criteria (Given/When/Then format)",
    )
    dependencies: List[str] = Field(
        default_factory=list,
        description="List of task IDs this task depends on",
    )
    similar: Optional[List[SimilarMatch]] = Field(
        None,
        description="Similar work items found in GitLab",
    )


class Epic(BaseModel):
    """Represents an epic containing multiple tasks."""

    id: str = Field(..., description="Epic identifier")
    title: str = Field(..., description="Epic title")
    description: str = Field(..., description="Epic description")
    tasks: List[Task] = Field(default_factory=list, description="Tasks within this epic")
    similar: Optional[List[SimilarMatch]] = Field(
        None,
        description="Similar epics found in GitLab",
    )



