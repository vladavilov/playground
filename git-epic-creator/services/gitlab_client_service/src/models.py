from typing import List, Optional, Literal
from pydantic import BaseModel, Field


Kind = Literal["epic", "issue"]

class GitLabWorkItem(BaseModel):
    kind: Kind
    id: str
    title: str
    title_embedding: List[float] = Field(default_factory=list)
    description: str = ""
    state: Literal["opened", "closed"] = "opened"
    labels: List[str] = Field(default_factory=list)
    web_url: str


class Pagination(BaseModel):
    page: int
    per_page: int
    next_page: Optional[int] = None
    prev_page: Optional[int] = None
    total: Optional[int] = None


class ListResponse(BaseModel):
    items: List[GitLabWorkItem]
    pagination: Pagination


class ApplyBacklogWorkItem(BaseModel):
    title: str
    description: str = ""
    labels: List[str] = Field(default_factory=list)
    related_to_iid: Optional[str] = Field(
        None,
        description="IID of similar/related item to link to (creates related link)"
    )

class ApplyBacklogIssue(ApplyBacklogWorkItem):
    parent_epic_index: Optional[int] = Field(
        None,
        description="Index of parent epic in epics array (for epic-issue hierarchy)"
    )


class ApplyBacklogRequest(BaseModel):
    project_id: str = Field(description="GitLab project ID (numeric or path)")
    prompt_id: str = Field(description="Unique prompt/generation ID for idempotency")
    internal_project_id: Optional[str] = Field(
        default=None,
        description="Internal project management service ID (UUID) for tracking"
    )
    epics: List[ApplyBacklogWorkItem] = Field(default_factory=list)
    issues: List[ApplyBacklogIssue] = Field(default_factory=list)


class ApplyBacklogItemResult(BaseModel):
    input_index: int
    action: Literal["created", "updated", "unchanged"]
    id: str
    web_url: str


class ApplyBacklogResults(BaseModel):
    epics: List[ApplyBacklogItemResult] = Field(default_factory=list)
    issues: List[ApplyBacklogItemResult] = Field(default_factory=list)


class ApplyBacklogError(BaseModel):
    scope: Literal["epic", "issue"]
    input_index: int
    message: str
    gitlab_status: Optional[int] = None


class ApplyBacklogResponse(BaseModel):
    results: ApplyBacklogResults
    errors: List[ApplyBacklogError] = Field(default_factory=list)


class ResolveProjectRequest(BaseModel):
    """Request to resolve GitLab project path to project ID."""
    gitlab_path: str = Field(
        description="GitLab project path in format: namespace/project"
    )


class ResolveProjectResponse(BaseModel):
    """Response with resolved GitLab project details."""
    project_id: str = Field(description="Numeric GitLab project ID")
    path: str = Field(description="Full project path: namespace/project")
    name: str = Field(description="Project name")
    web_url: str = Field(description="Project web URL")


