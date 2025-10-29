"""HTTP adapter for gitlab-client-service API calls."""

from typing import Optional
import structlog
import httpx
from tenacity import (
    retry,
    stop_after_attempt,
    wait_exponential,
    retry_if_exception_type,
    retry_if_result,
)

from configuration.http_client_config import HTTPClientSettings

logger = structlog.get_logger(__name__)


class GitLabClientAdapter:
    """
    HTTP client for gitlab-client-service.
    
    Replaces direct python-gitlab usage with HTTP calls to gitlab-client-service.
    All GitLab interactions are delegated to the dedicated service.
    
    Follows shared library HTTP retry patterns with smart retry predicates.
    Only retries transient failures (connection errors, timeouts, 5xx responses).
    Fails fast on client errors (4xx) for better error handling.
    """
    
    def __init__(self, http_client_config: HTTPClientSettings):
        """
        Initialize adapter with shared HTTP client configuration.
        
        Args:
            http_client_config: HTTP client settings from shared library
        """
        self.config = http_client_config
        self.base_url = self.config.GITLAB_CLIENT_SERVICE_URL.rstrip("/")
        
        timeout = httpx.Timeout(
            connect=self.config.CONNECTION_TIMEOUT,
            read=self.config.READ_TIMEOUT,
            write=self.config.CONNECTION_TIMEOUT,
            pool=self.config.CONNECTION_TIMEOUT,
        )
        self.client = httpx.AsyncClient(timeout=timeout, base_url=self.base_url)
    
    async def close(self):
        """Close HTTP client."""
        await self.client.aclose()
    
    def _should_retry_on_result(self, result: httpx.Response) -> bool:
        """
        Determine if HTTP response should trigger retry.
        Only retry server errors (5xx), not client errors (4xx).
        """
        return result.status_code >= 500
    
    async def _make_request_with_retry(
        self,
        method: str,
        endpoint: str,
        **kwargs
    ) -> httpx.Response:
        """
        Make HTTP request with smart retry logic.
        
        Retries only transient failures:
        - Connection errors (network issues)
        - Timeout exceptions
        - Server errors (5xx)
        
        Does NOT retry client errors (4xx) for fail-fast behavior.
        """
        @retry(
            stop=stop_after_attempt(self.config.MAX_RETRIES + 1),
            wait=wait_exponential(
                multiplier=1,
                max=60,
                exp_base=self.config.RETRY_BACKOFF_FACTOR
            ),
            retry=(
                retry_if_exception_type((httpx.ConnectError, httpx.TimeoutException))
                | retry_if_result(self._should_retry_on_result)
            ),
            reraise=True,
        )
        async def _execute_request():
            logger.info(
                "Making HTTP request to gitlab-client-service",
                method=method,
                endpoint=endpoint,
                base_url=self.base_url,
            )
            
            response = await self.client.request(method, endpoint, **kwargs)
            
            logger.info(
                "HTTP request completed",
                method=method,
                endpoint=endpoint,
                status_code=response.status_code
            )
            return response
        
        return await _execute_request()
    
    async def resolve_project_id(
        self,
        gitlab_path: str,
        s2s_token: str
    ) -> Optional[str]:
        """
        Resolve GitLab project path to numeric project ID.
        
        Args:
            gitlab_path: GitLab project path (namespace/project format) or URL
            s2s_token: Service-to-service JWT token (contains session_id)
        
        Returns:
            Numeric GitLab project ID as string, or None if resolution fails
            
        Raises:
            httpx.HTTPStatusError: If API call fails with non-retryable error
        """
        if not gitlab_path or not gitlab_path.strip():
            logger.warning("Empty GitLab path provided for resolution")
            return None
        
        if not s2s_token or not s2s_token.strip():
            logger.warning("No S2S token provided for GitLab resolution")
            return None
        
        try:
            endpoint = "/gitlab/projects/resolve"
            headers = {
                "Authorization": f"Bearer {s2s_token}",
                "Content-Type": "application/json"
            }
            payload = {"gitlab_path": gitlab_path}
            
            logger.info(
                "Resolving GitLab project via gitlab-client-service",
                gitlab_path=gitlab_path,
                endpoint=endpoint
            )
            
            response = await self._make_request_with_retry(
                "POST",
                endpoint,
                json=payload,
                headers=headers
            )
            response.raise_for_status()
            
            data = response.json()
            project_id = data.get("project_id")
            
            logger.info(
                "Successfully resolved GitLab project ID",
                gitlab_path=gitlab_path,
                project_id=project_id
            )
            
            return project_id
            
        except httpx.HTTPStatusError as e:
            if e.response.status_code == 404:
                logger.warning("GitLab project not found", gitlab_path=gitlab_path)
            elif e.response.status_code == 401:
                logger.error("GitLab authentication failed", gitlab_path=gitlab_path)
            else:
                logger.error(
                    "GitLab client service returned error",
                    gitlab_path=gitlab_path,
                    status_code=e.response.status_code,
                    error=str(e)
                )
            raise
        except Exception as e:
            logger.error(
                "Unexpected error resolving GitLab project ID",
                gitlab_path=gitlab_path,
                error=str(e),
                error_type=type(e).__name__
            )
            raise

