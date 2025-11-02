"""HTTP client for GitLab client service."""

from typing import Any, Dict, List
import asyncio
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

    async def fetch_backlog_from_multiple_projects(
        self,
        gitlab_project_ids: List[str],
        auth_header: str | None = None,
        gitlab_token: str | None = None,
    ) -> Dict[str, List[Dict[str, Any]]]:
        """Fetch and aggregate backlog from multiple GitLab projects.
        
        Args:
            gitlab_project_ids: List of GitLab project IDs to fetch from
            auth_header: Optional authorization header value (JWT)
            gitlab_token: Optional GitLab access token
            
        Returns:
            Aggregated dict with keys: epics (list), issues (list)
            Each item includes 'project_id' field indicating source project
        """
        if not gitlab_project_ids:
            logger.warning("No GitLab project IDs provided for backlog fetch")
            return {"epics": [], "issues": []}
        
        logger.info(
            "Fetching backlog from multiple GitLab projects",
            project_count=len(gitlab_project_ids),
            project_ids=gitlab_project_ids,
        )
        
        # Fetch all projects in parallel
        tasks = [
            self._fetch_single_project_backlog(
                project_id, auth_header, gitlab_token
            )
            for project_id in gitlab_project_ids
        ]
        
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # Aggregate results
        aggregated_epics = []
        aggregated_issues = []
        
        for idx, result in enumerate(results):
            project_id = gitlab_project_ids[idx]
            
            if isinstance(result, Exception):
                logger.warning(
                    "Failed to fetch backlog from project",
                    project_id=project_id,
                    error=str(result),
                )
                continue
            
            if not isinstance(result, dict):
                logger.warning(
                    "Invalid backlog result type",
                    project_id=project_id,
                    result_type=type(result).__name__,
                )
                continue
            
            # Add project_id to each item and aggregate
            epics = result.get("epics", [])
            issues = result.get("issues", [])
            
            for epic in epics:
                epic["project_id"] = project_id
                aggregated_epics.append(epic)
            
            for issue in issues:
                issue["project_id"] = project_id
                aggregated_issues.append(issue)
            
            logger.info(
                "Fetched backlog from project",
                project_id=project_id,
                epics_count=len(epics),
                issues_count=len(issues),
            )
        
        logger.info(
            "Aggregated backlog from multiple projects",
            total_epics=len(aggregated_epics),
            total_issues=len(aggregated_issues),
            successful_projects=len([r for r in results if not isinstance(r, Exception)]),
        )
        
        return {
            "epics": aggregated_epics,
            "issues": aggregated_issues,
        }
    
    async def _fetch_single_project_backlog(
        self,
        gitlab_project_id: str,
        auth_header: str | None = None,
        gitlab_token: str | None = None,
    ) -> Dict[str, Any]:
        """Fetch backlog from a single project (internal helper for parallel fetching)."""
        url = f"{self.base_url}/gitlab/projects/{gitlab_project_id}/backlog"
        
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
                    
                    resp = await client.get(url, headers=headers)
                    
                    try:
                        resp.raise_for_status()
                    except Exception:
                        # Return empty backlog on error rather than failing
                        return {"epics": [], "issues": []}
                    
                    data = resp.json()
                    
                    # Parse ListResponse structure (items with 'kind' field)
                    items = data.get("items", [])
                    epics = [item for item in items if item.get("kind") == "epic"]
                    issues = [item for item in items if item.get("kind") == "issue"]
                    
                    return {"epics": epics, "issues": issues}


