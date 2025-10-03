"""HTTP client for GitLab client service."""

from typing import Any, Dict, List
import httpx
from tenacity import AsyncRetrying, stop_after_attempt, wait_exponential
import structlog

logger = structlog.get_logger(__name__)


class GitLabClient:
    """Client for communicating with gitlab_client_service."""

    def __init__(
        self,
        base_url: str,
        timeout_sec: float = 30.0,
        max_attempts: int = 2,
        backoff_base_sec: float = 0.2,
    ):
        self.base_url = base_url.rstrip("/")
        self.timeout = timeout_sec
        self.max_attempts = max_attempts
        self.backoff_base = backoff_base_sec

    def _build_url(self, project_id: str) -> str:
        return f"{self.base_url}/gitlab/projects/{project_id}/backlog"

    async def fetch_backlog(
        self,
        project_id: str,
        auth_header: str | None = None,
        gitlab_token: str | None = None,
    ) -> Dict[str, Any]:
        """Fetch existing epics and issues from GitLab.
        
        Args:
            project_id: GitLab project identifier
            auth_header: Optional authorization header value (JWT)
            gitlab_token: Optional GitLab access token
            
        Returns:
            Dict with keys: epics (list), issues (list)
            Each item includes title_embedding if available from cache
        """
        async for attempt in AsyncRetrying(
            reraise=True,
            stop=stop_after_attempt(self.max_attempts),
            wait=wait_exponential(multiplier=self.backoff_base),
        ):
            with attempt:
                async with httpx.AsyncClient(timeout=self.timeout) as client:
                    headers = {}
                    if auth_header:
                        headers["Authorization"] = auth_header
                    if gitlab_token:
                        headers["GitLab-Access-Token"] = gitlab_token
                    
                    logger.info(
                        "Fetching backlog from GitLab client",
                        url=self._build_url(project_id),
                        project_id=project_id,
                    )
                    
                    resp = await client.get(self._build_url(project_id), headers=headers)
                    
                try:
                    resp.raise_for_status()
                except Exception as e:
                    logger.warning(
                        "GitLab client request failed",
                        status_code=resp.status_code,
                        error=str(e),
                    )
                    # Return empty backlog on error rather than failing
                    return {"epics": [], "issues": []}
                
                data = resp.json() if hasattr(resp, "json") else {}
                
                # Parse ListResponse structure (items with 'kind' field)
                items = data.get("items", [])
                epics = [item for item in items if item.get("kind") == "epic"]
                issues = [item for item in items if item.get("kind") == "issue"]
                
                logger.info(
                    "Parsed GitLab backlog",
                    epics_count=len(epics),
                    issues_count=len(issues),
                    total_items=len(items),
                )
                
                return {
                    "epics": epics,
                    "issues": issues,
                }


