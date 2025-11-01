"""
GitLab Backlog Project URL Resolver.

Resolves GitLab project URLs to project IDs for backlog (issues/epics) management.
Only HTTPS URLs are supported for custom GitLab instances with nested group structures.
"""

import asyncio
from typing import List, Dict, Optional
from urllib.parse import urlparse, unquote
import structlog

from services.gitlab_client_adapter import GitLabClientAdapter

logger = structlog.get_logger(__name__)


class GitLabBacklogProjectResolver:
    """
    Resolve GitLab HTTPS URLs to project IDs for backlog management.
    
    Supports custom GitLab instances with deeply nested group structures.
    Example: https://host.name/team_1/sub_team_1/sub_team_n/project_name
    
    Only HTTPS URLs are accepted.
    """
    
    def __init__(self, gitlab_client_adapter: GitLabClientAdapter):
        """Initialize resolver with GitLab client adapter."""
        self.gitlab_client = gitlab_client_adapter
    
    @staticmethod
    def extract_project_path(url: str) -> str:
        """
        Extract project path from HTTPS URL.
        
        Args:
            url: HTTPS GitLab URL (e.g., https://host.name/team_1/sub_team_1/project)
            
        Returns:
            Project path (e.g., team_1/sub_team_1/project)
            
        Raises:
            ValueError: If URL is not a valid HTTPS URL
        """
        if not url or not url.strip():
            raise ValueError("URL cannot be empty")
        
        url = url.strip()
        
        if not (url.startswith('http://') or url.startswith('https://')):
            raise ValueError(f"Only HTTPS URLs supported: {url}")
        
        parsed = urlparse(url)
        path = unquote(parsed.path.lstrip('/'))
        
        if not path or '/' not in path:
            raise ValueError(f"No valid project path found in URL: {url}")
        
        logger.debug("Extracted project path", url=url, path=path)
        return path
    
    async def resolve_url(
        self,
        url: str,
        s2s_token: str
    ) -> Optional[Dict[str, str]]:
        """
        Resolve a single GitLab HTTPS URL to project details.
        
        Args:
            url: GitLab HTTPS URL
            s2s_token: Service-to-service authentication token
            
        Returns:
            Dict with keys: 'url', 'path', 'project_id', 'name'
            Returns None if resolution fails
        """
        try:
            path = self.extract_project_path(url)
            
            project_id = await self.gitlab_client.resolve_project_id(
                gitlab_path=path,
                s2s_token=s2s_token
            )
            
            if not project_id:
                logger.warning("Failed to resolve GitLab project", url=url, path=path)
                return None
            
            result = {
                'url': url,
                'path': path,
                'project_id': str(project_id),
                'name': path.split('/')[-1]
            }
            
            logger.info("Resolved GitLab URL", **result)
            return result
            
        except ValueError as e:
            logger.error("Invalid GitLab URL", url=url, error=str(e))
            return None
        except Exception as e:
            logger.error("Error resolving GitLab URL", url=url, error=str(e), error_type=type(e).__name__)
            return None
    
    async def resolve_multiple(
        self,
        urls: List[str],
        s2s_token: str
    ) -> List[Dict[str, str]]:
        """
        Resolve multiple GitLab HTTPS URLs to project IDs in parallel.
        
        Args:
            urls: List of GitLab HTTPS URLs
            s2s_token: Service-to-service authentication token
            
        Returns:
            List of dicts with 'url', 'path', 'project_id', 'name'
            Only includes successfully resolved projects
        """
        if not urls:
            return []
        
        logger.info("Resolving multiple GitLab URLs", url_count=len(urls))
        
        tasks = [self.resolve_url(url, s2s_token) for url in urls]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        resolved = [r for r in results if isinstance(r, dict)]
        failed = len(urls) - len(resolved)
        
        if failed > 0:
            logger.warning("Some URLs failed to resolve", failed_count=failed)
        
        logger.info("Resolved GitLab URLs", successful=len(resolved), failed=failed)
        return resolved
    
    @staticmethod
    def extract_project_ids(resolved_projects: List[Dict[str, str]]) -> List[str]:
        """Extract project IDs from resolved project data."""
        return [p['project_id'] for p in resolved_projects]
    
    @staticmethod
    def extract_project_urls(resolved_projects: List[Dict[str, str]]) -> List[str]:
        """Extract URLs from resolved project data."""
        return [p['url'] for p in resolved_projects]

