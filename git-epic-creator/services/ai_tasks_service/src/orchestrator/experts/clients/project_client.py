"""HTTP client for Project Management service."""

from typing import Dict, Any
from uuid import UUID
import httpx
from tenacity import AsyncRetrying, stop_after_attempt, wait_exponential
import structlog

logger = structlog.get_logger(__name__)


class ProjectClient:
    """Client for communicating with project_management_service."""

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
        """Build project details URL using internal project UUID."""
        return f"{self.base_url}/projects/{project_id}"

    async def get_project(
        self,
        project_id: UUID,
        auth_header: str | None = None,
    ) -> Dict[str, Any]:
        """Fetch project details including gitlab_backlog_project_ids.
        
        Args:
            project_id: Internal project UUID from PostgreSQL
            auth_header: Authorization header value (JWT)
            
        Returns:
            Dict with project fields including:
            - id (UUID)
            - name (str)
            - gitlab_backlog_project_ids (Optional[List[str]]) - array of numeric GitLab project IDs
            - gitlab_backlog_project_urls (Optional[List[str]])
            - gitlab_repository_url (Optional[str])
            - status (str)
            
        Raises:
            httpx.HTTPStatusError: If project not found (404) or auth fails (401)
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
                    
                    url = self._build_url(str(project_id))
                    
                    logger.info(
                        "Fetching project details from project management service",
                        url=url,
                        project_id=str(project_id),
                    )
                    
                    resp = await client.get(url, headers=headers)
                    
                    try:
                        resp.raise_for_status()
                    except httpx.HTTPStatusError as e:
                        logger.warning(
                            "Project management service request failed",
                            status_code=resp.status_code,
                            error=str(e),
                            project_id=str(project_id),
                        )
                        raise
                    
                    data = resp.json()
                    
                    # Extract first GitLab project ID for logging (if array exists)
                    backlog_ids = data.get("gitlab_backlog_project_ids", [])
                    
                    logger.info(
                        "Retrieved project details",
                        project_id=str(project_id),
                        gitlab_backlog_project_count=len(backlog_ids),
                    )
                    
                    return data

    async def get_gitlab_project_ids(
        self,
        project_id: UUID,
        auth_header: str | None = None,
    ) -> list[str]:
        """Fetch all gitlab_backlog_project_ids for a project.
        
        Projects support multiple GitLab backlog projects for comprehensive
        backlog analysis across multiple repositories/projects.
        
        Args:
            project_id: Internal project UUID from PostgreSQL
            auth_header: Authorization header value (JWT)
            
        Returns:
            List of GitLab numeric project IDs (e.g., ["123", "456"]) or empty list if not linked
        """
        try:
            project_data = await self.get_project(project_id, auth_header)
            backlog_project_ids = project_data.get("gitlab_backlog_project_ids", [])
            
            if not backlog_project_ids or not isinstance(backlog_project_ids, list) or len(backlog_project_ids) == 0:
                logger.warning(
                    "Project has no GitLab backlog projects configured",
                    project_id=str(project_id),
                )
                return []
            
            logger.info(
                "Retrieved GitLab backlog project IDs",
                project_id=str(project_id),
                project_count=len(backlog_project_ids),
                gitlab_project_ids=backlog_project_ids,
            )
            
            return backlog_project_ids
            
        except httpx.HTTPStatusError as e:
            if e.response.status_code == 404:
                logger.error(
                    "Project not found in project management service",
                    project_id=str(project_id),
                )
            raise
        except Exception as e:
            logger.error(
                "Failed to fetch gitlab_backlog_project_ids",
                project_id=str(project_id),
                error=str(e),
                error_type=type(e).__name__,
            )
            raise


