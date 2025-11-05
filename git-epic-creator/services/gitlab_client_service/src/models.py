from typing import List, Optional, Literal
from pydantic import BaseModel, Field


Kind = Literal["epic", "issue"]

class GitLabWorkItem(BaseModel):
    kind: Kind
    id: str
    iid: str = Field(description="Internal ID (IID) - used for linking within project/group")
    title: str
    title_embedding: List[float] = Field(default_factory=list)
    description: str = ""
    state: Literal["opened", "closed"] = "opened"
    labels: List[str] = Field(default_factory=list)
    web_url: str
    source_gitlab_project_id: Optional[str] = Field(
        default=None,
        description="Source GitLab project ID (for multi-project backlogs)"
    )


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
    target_project_id: Optional[str] = Field(
        None,
        description="User-selected target GitLab project ID (overrides payload-level project_id)"
    )
    related_to_iids: List[str] = Field(
        default_factory=list,
        description="List of IIDs of similar/related items to link to (creates multiple related links for many-to-many relationships)"
    )

class ApplyBacklogIssue(ApplyBacklogWorkItem):
    parent_epic_index: Optional[int] = Field(
        None,
        description="Index of parent epic in epics array (for epic-issue hierarchy)"
    )


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


class BatchApplyBacklogProjectPayload(BaseModel):
    """Payload for applying backlog to a single project within batch operation."""
    project_id: str = Field(description="Target GitLab project ID")
    epics: List[ApplyBacklogWorkItem] = Field(default_factory=list)
    issues: List[ApplyBacklogIssue] = Field(default_factory=list)


class BatchApplyBacklogRequest(BaseModel):
    """Request for apply-backlog endpoint (supports 1+ projects)."""
    prompt_id: str = Field(description="Unique prompt/generation ID for idempotency")
    internal_project_id: Optional[str] = Field(
        default=None,
        description="Internal project management service ID (UUID) for tracking"
    )
    projects: List[BatchApplyBacklogProjectPayload] = Field(
        description="List of project payloads, each targeting a specific GitLab project"
    )


class BatchApplyBacklogProjectResult(BaseModel):
    """Result for one project in apply-backlog operation."""
    project_id: str
    success: bool
    results: Optional[ApplyBacklogResults] = None
    errors: List[ApplyBacklogError] = Field(default_factory=list)
    error_message: Optional[str] = None


class BatchApplyBacklogResponse(BaseModel):
    """Aggregated response from apply-backlog endpoint."""
    project_results: List[BatchApplyBacklogProjectResult]
    total_epics_created: int = 0
    total_issues_created: int = 0
    total_errors: int = 0
    projects_succeeded: int = 0
    projects_failed: int = 0


