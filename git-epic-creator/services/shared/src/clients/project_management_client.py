"""
Shared HTTP client for communicating with the Project Management Service.
Moved from document_processing_service to shared library for reuse.
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
    retry_if_result,
)

from configuration.common_config import get_app_settings

logger = structlog.get_logger(__name__)


@dataclass
class UpdateProjectStatusResult:
    success: bool
    status_code: Optional[int] = None
    error_message: Optional[str] = None
    response_data: Optional[Dict[str, Any]] = None


class ProjectManagementClient:
    """Async HTTP client for Project Management Service communication."""

    def __init__(self):
        settings = get_app_settings()
        self.config = settings.http_client
        self._client: Optional[httpx.AsyncClient] = None

    async def __aenter__(self) -> "ProjectManagementClient":
        await self._ensure_client()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        await self.close()

    async def _ensure_client(self) -> None:
        if self._client is None:
            timeout = httpx.Timeout(
                connect=self.config.CONNECTION_TIMEOUT,
                read=self.config.READ_TIMEOUT,
                write=self.config.CONNECTION_TIMEOUT,
                pool=self.config.CONNECTION_TIMEOUT,
            )
            limits = httpx.Limits(
                max_connections=self.config.MAX_CONNECTIONS,
                max_keepalive_connections=self.config.MAX_KEEPALIVE_CONNECTIONS,
            )
            self._client = httpx.AsyncClient(
                timeout=timeout,
                limits=limits,
                base_url=self.config.PROJECT_MANAGEMENT_SERVICE_URL,
            )

    async def close(self) -> None:
        if self._client:
            await self._client.aclose()
            self._client = None

    def _should_retry_on_result(self, result: httpx.Response) -> bool:
        return result.status_code >= 500

    async def _make_request_with_retry(self, method: str, endpoint: str, **kwargs) -> httpx.Response:
        await self._ensure_client()

        @retry(
            stop=stop_after_attempt(self.config.MAX_RETRIES + 1),
            wait=wait_exponential(multiplier=1, max=60, exp_base=self.config.RETRY_BACKOFF_FACTOR),
            retry=(
                retry_if_exception_type((httpx.ConnectError, httpx.TimeoutException))
                | retry_if_result(self._should_retry_on_result)
            ),
            reraise=True,
        )
        async def _make_request():
            request_kwargs = kwargs.copy()

            logger.info(
                "Making HTTP request",
                method=method,
                endpoint=endpoint,
                base_url=self.config.PROJECT_MANAGEMENT_SERVICE_URL,
            )

            response = await self._client.request(method, endpoint, **request_kwargs)

            logger.info("HTTP request completed", method=method, endpoint=endpoint, status_code=response.status_code)
            return response

        return await _make_request()

    async def update_project_status(
        self,
        project_id: str,
        processed_count: Optional[int] = None,
        total_count: Optional[int] = None,
        status: Optional[str] = None,
        error_message: Optional[str] = None,
        process_step: Optional[str] = None,
        authorization_header: Optional[str] = None,
    ) -> UpdateProjectStatusResult:
        # Validate project_id
        try:
            parsed_uuid = UUID(project_id)
            logger.info(
                "ProjectManagementClient received project_id",
                project_id=project_id,
                parsed_uuid=str(parsed_uuid),
            )
        except ValueError as e:
            raise ValueError("Invalid project_id format. Must be a valid UUID string.") from e

        if processed_count is None and total_count is None and status is None and error_message is None:
            raise ValueError("Either progress counts, status, or error_message must be provided")

        if (processed_count is None) ^ (total_count is None):
            raise ValueError("Both processed_count and total_count must be provided together")

        if processed_count is not None and total_count is not None:
            if processed_count < 0 or total_count <= 0:
                raise ValueError("Counts must be non-negative and total_count > 0")
            if processed_count > total_count:
                raise ValueError("processed_count cannot be greater than total_count")

        payload: Dict[str, Any] = {}

        if processed_count is not None and total_count is not None:
            processed_pct = (processed_count / total_count * 100.0)
            payload.update({
                "processed_count": processed_count,
                "total_count": total_count,
                "processed_pct": processed_pct,
                "status": "active" if processed_count >= total_count else "processing",
            })
        if status is not None:
            payload["status"] = status
        if error_message:
            payload["error_message"] = error_message
        if process_step:
            payload["process_step"] = process_step

        endpoint = f"/projects/{project_id}/status"
        headers = {}
        if authorization_header:
            # Ensure Authorization header has Bearer prefix
            if authorization_header.startswith("Bearer "):
                headers["Authorization"] = authorization_header
            else:
                headers["Authorization"] = f"Bearer {authorization_header}"
        try:
            response = await self._make_request_with_retry("PUT", endpoint, json=payload, headers=headers or None)
            if 200 <= response.status_code < 300:
                return UpdateProjectStatusResult(
                    success=True, status_code=response.status_code,
                    response_data=response.json() if response.content else None,
                )

            # Treat 404 as non-fatal during best-effort status updates
            if response.status_code == 404:
                logger.warning("Project status update target not found; continuing", endpoint=endpoint)
                return UpdateProjectStatusResult(success=False, status_code=response.status_code)

            error_msg = f"HTTP {response.status_code}"
            try:
                error_data = response.json()
                detail = error_data.get("detail") or error_data.get("error")
                if detail:
                    error_msg += f": {detail}"
            except (ValueError, TypeError):
                error_msg += f": {response.text}"
            return UpdateProjectStatusResult(success=False, status_code=response.status_code, error_message=error_msg)
        except Exception as e:
            return UpdateProjectStatusResult(success=False, error_message=f"Request failed: {str(e)}")

