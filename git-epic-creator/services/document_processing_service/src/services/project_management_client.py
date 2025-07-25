"""
HTTP client for communicating with the Project Management Service.
"""
from typing import Optional, Dict, Any
from uuid import UUID
from dataclasses import dataclass
import httpx
import structlog
from tenacity import (
    retry,
    stop_after_attempt,
    wait_exponential,
    retry_if_exception_type,
    retry_if_result
)

from configuration.http_client_config import HTTPClientSettings
from configuration.common_config import get_app_settings

logger = structlog.get_logger(__name__)


@dataclass
class UpdateProjectStatusResult:
    """Result of project status update operation."""
    success: bool
    status_code: Optional[int] = None
    error_message: Optional[str] = None
    response_data: Optional[Dict[str, Any]] = None


class ProjectManagementClient:
    """
    Async HTTP client for Project Management Service communication.

    Provides retry logic with exponential backoff and proper error handling
    for communicating with the project management service.
    """

    def __init__(self):
        """
        Initialize the Project Management Client.

        Args:
            config: HTTP client configuration settings
        """
        settings = get_app_settings()
        self.config = settings.http_client
        self._client: Optional[httpx.AsyncClient] = None

    async def __aenter__(self) -> 'ProjectManagementClient':
        """Async context manager entry."""
        await self._ensure_client()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Async context manager exit."""
        await self.close()

    async def _ensure_client(self) -> None:
        """Ensure HTTP client is initialized."""
        if self._client is None:
            timeout = httpx.Timeout(
                connect=self.config.CONNECTION_TIMEOUT,
                read=self.config.READ_TIMEOUT,
                write=self.config.CONNECTION_TIMEOUT,
                pool=self.config.CONNECTION_TIMEOUT
            )

            limits = httpx.Limits(
                max_connections=self.config.MAX_CONNECTIONS,
                max_keepalive_connections=self.config.MAX_KEEPALIVE_CONNECTIONS
            )

            self._client = httpx.AsyncClient(
                timeout=timeout,
                limits=limits,
                base_url=self.config.PROJECT_MANAGEMENT_SERVICE_URL
            )

    async def close(self) -> None:
        """Close the HTTP client and cleanup resources."""
        if self._client:
            await self._client.aclose()
            self._client = None

    def _should_retry_on_result(self, result: httpx.Response) -> bool:
        """
        Determine if we should retry based on response status.

        Args:
            result: HTTP response

        Returns:
            True if we should retry, False otherwise
        """
        # Retry on 5xx server errors, but not on 4xx client errors
        return result.status_code >= 500

    async def _make_request_with_retry(
        self,
        method: str,
        endpoint: str,
        **kwargs
    ) -> httpx.Response:
        """
        Make HTTP request with retry logic.

        Args:
            method: HTTP method (GET, POST, PUT, etc.)
            endpoint: API endpoint path
            **kwargs: Additional arguments for the HTTP request

        Returns:
            HTTP response

        Raises:
            RetryError: When all retry attempts are exhausted
            httpx.HTTPError: For other HTTP-related errors
        """
        await self._ensure_client()

        @retry(
            stop=stop_after_attempt(self.config.MAX_RETRIES + 1),  # +1 for initial attempt
            wait=wait_exponential(
                multiplier=1,
                max=60,
                exp_base=self.config.RETRY_BACKOFF_FACTOR
            ),
            retry=(
                retry_if_exception_type((httpx.ConnectError, httpx.TimeoutException)) |
                retry_if_result(self._should_retry_on_result)
            ),
            reraise=True
        )
        async def _make_request():
            logger.info(
                "Making HTTP request",
                method=method,
                endpoint=endpoint,
                base_url=self.config.PROJECT_MANAGEMENT_SERVICE_URL
            )

            response = await self._client.request(method, endpoint, **kwargs)

            logger.info(
                "HTTP request completed",
                method=method,
                endpoint=endpoint,
                status_code=response.status_code
            )

            return response

        return await _make_request()

    async def update_project_status(
        self,
        project_id: str,
        processed_count: Optional[int] = None,
        total_count: Optional[int] = None,
        status: Optional[str] = None
    ) -> UpdateProjectStatusResult:
        """
        Update project processing status and progress.

        This method handles both progress updates and status resets in a single method.

        Args:
            project_id: UUID string of the project to update
            processed_count: Number of processed documents (for progress updates)
            total_count: Total number of documents (for progress updates)
            status: Project status to set (for status resets)

        Returns:
            UpdateProjectStatusResult with operation details

        Raises:
            ValueError: For invalid input parameters
        """
        # Validate project_id format
        try:
            UUID(project_id)
        except ValueError as e:
            raise ValueError("Invalid project_id format. Must be a valid UUID string.") from e

        # Validate input parameters
        if processed_count is None and total_count is None and status is None:
            raise ValueError("Either progress counts or status must be provided")

        # Validate progress counts if provided
        if processed_count is not None or total_count is not None:
            if processed_count is None or total_count is None:
                raise ValueError("Both processed_count and total_count must be provided together")

            if processed_count < 0 or total_count < 0:
                raise ValueError("Counts must be non-negative")

            if processed_count > total_count:
                raise ValueError("processed_count cannot be greater than total_count")

        # Build request payload
        payload = {}

        if processed_count is not None and total_count is not None:
            # Progress update
            processed_pct = (processed_count / total_count * 100.0) if total_count > 0 else 0.0

            payload.update({
                "processed_count": processed_count,
                "total_count": total_count,
                "processed_pct": processed_pct,
                "status": "active" if processed_count >= total_count else "processing"
            })
        elif status is not None:
            # Status reset
            payload["status"] = status

        endpoint = f"/projects/{project_id}/status"

        logger.info(
            "Updating project status",
            project_id=project_id,
            payload=payload,
            endpoint=endpoint
        )

        try:
            response = await self._make_request_with_retry(
                "PUT",
                endpoint,
                json=payload
            )

            if response.status_code >= 200 and response.status_code < 300:
                logger.info(
                    "Project status updated successfully",
                    project_id=project_id,
                    status_code=response.status_code
                )

                return UpdateProjectStatusResult(
                    success=True,
                    status_code=response.status_code,
                    response_data=response.json() if response.content else None
                )

            error_msg = f"HTTP {response.status_code}"
            try:
                error_data = response.json()
                if "error" in error_data:
                    error_msg += f": {error_data['error']}"
            except (ValueError, TypeError):
                error_msg += f": {response.text}"

            logger.error(
                "Project status update failed",
                project_id=project_id,
                status_code=response.status_code,
                error=error_msg
            )

            return UpdateProjectStatusResult(
                success=False,
                status_code=response.status_code,
                error_message=error_msg
            )

        except Exception as e:
            error_msg = f"Request failed: {str(e)}"
            logger.error(
                "Project status update request failed",
                project_id=project_id,
                error=error_msg
            )

            return UpdateProjectStatusResult(
                success=False,
                error_message=error_msg
            )
