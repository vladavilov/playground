"""HTTP client for Project Management service."""

from typing import Optional, Dict, Any
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
        """Fetch project details including gitlab_project_id.
        
        Args:
            project_id: Internal project UUID from PostgreSQL
            auth_header: Authorization header value (JWT)
            
        Returns:
            Dict with project fields including:
            - id (UUID)
            - name (str)
            - gitlab_project_id (Optional[str]) - numeric GitLab project ID
            - gitlab_path (Optional[str])
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
                    
                    logger.info(
                        "Retrieved project details",
                        project_id=str(project_id),
                        gitlab_project_id=data.get("gitlab_project_id"),
                        gitlab_path=data.get("gitlab_path"),
                    )
                    
                    return data

    async def get_gitlab_project_id(
        self,
        project_id: UUID,
        auth_header: str | None = None,
    ) -> Optional[str]:
        """Fetch only the gitlab_project_id for a project.
        
        Args:
            project_id: Internal project UUID from PostgreSQL
            auth_header: Authorization header value (JWT)
            
        Returns:
            GitLab numeric project ID (e.g., "123") or None if not linked to GitLab
        """
        try:
            project_data = await self.get_project(project_id, auth_header)
            gitlab_project_id = project_data.get("gitlab_project_id")
            
            if not gitlab_project_id:
                logger.warning(
                    "Project has no GitLab integration",
                    project_id=str(project_id),
                )
                return None
            
            return gitlab_project_id
            
        except httpx.HTTPStatusError as e:
            if e.response.status_code == 404:
                logger.error(
                    "Project not found in project management service",
                    project_id=str(project_id),
                )
            raise
        except Exception as e:
            logger.error(
                "Failed to fetch gitlab_project_id",
                project_id=str(project_id),
                error=str(e),
                error_type=type(e).__name__,
            )
            raise

