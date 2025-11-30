"""HTTP adapters for upstream service communication.

This module provides HTTP clients for:
- Project Management Service (project resolution)
- Neo4j Retrieval Service (DRIFT search)

Reuses shared library HTTP client patterns and retry configuration.
"""

from typing import Any
import httpx
import structlog
from tenacity import (
    retry,
    stop_after_attempt,
    wait_exponential,
    retry_if_exception_type,
    retry_if_result,
)

from configuration.http_client_config import HTTPClientSettings
from config import get_http_client_settings, get_project_management_url, get_retrieval_service_url

logger = structlog.get_logger(__name__)


class UpstreamServiceAdapter:
    """
    Base HTTP adapter for upstream service communication.
    
    Reuses shared library retry patterns with exponential backoff.
    Fails fast on client errors (4xx).
    """

    def __init__(self, base_url: str, http_settings: HTTPClientSettings):
        self.base_url = base_url.rstrip("/")
        self.settings = http_settings
        
        timeout = httpx.Timeout(
            connect=http_settings.CONNECTION_TIMEOUT,
            read=http_settings.READ_TIMEOUT,
            write=http_settings.CONNECTION_TIMEOUT,
            pool=http_settings.CONNECTION_TIMEOUT,
        )
        limits = httpx.Limits(
            max_connections=http_settings.MAX_CONNECTIONS,
            max_keepalive_connections=http_settings.MAX_KEEPALIVE_CONNECTIONS,
        )
        self._client = httpx.AsyncClient(
            timeout=timeout,
            limits=limits,
            base_url=self.base_url
        )

    async def close(self) -> None:
        """Close the HTTP client."""
        await self._client.aclose()

    def _should_retry_on_result(self, result: httpx.Response) -> bool:
        """Retry only on server errors (5xx)."""
        return result.status_code >= 500

    async def _make_request(
        self,
        method: str,
        endpoint: str,
        headers: dict[str, str] | None = None,
        **kwargs
    ) -> httpx.Response:
        """Make HTTP request with retry logic."""

        @retry(
            stop=stop_after_attempt(self.settings.MAX_RETRIES + 1),
            wait=wait_exponential(
                multiplier=1,
                max=60,
                exp_base=self.settings.RETRY_BACKOFF_FACTOR
            ),
            retry=(
                retry_if_exception_type((httpx.ConnectError, httpx.TimeoutException))
                | retry_if_result(self._should_retry_on_result)
            ),
            reraise=True,
        )
        async def _execute():
            logger.debug(
                "Making HTTP request",
                method=method,
                endpoint=endpoint,
                base_url=self.base_url,
            )
            
            response = await self._client.request(
                method, 
                endpoint, 
                headers=headers,
                **kwargs
            )
            
            logger.debug(
                "HTTP response received",
                method=method,
                endpoint=endpoint,
                status_code=response.status_code
            )
            return response

        return await _execute()


class ProjectManagementAdapter(UpstreamServiceAdapter):
    """
    HTTP adapter for Project Management Service.
    
    Used by resolve_project tool to resolve project names to UUIDs.
    """

    def __init__(self):
        http_settings = get_http_client_settings()
        base_url = get_project_management_url()
        super().__init__(base_url, http_settings)

    async def search_projects(
        self,
        project_name: str,
        auth_token: str | None = None
    ) -> dict[str, Any]:
        """
        Search projects by name.
        
        Args:
            project_name: Natural language project name to search for
            auth_token: Optional Bearer token for authentication
            
        Returns:
            Dict with:
            - success: bool
            - project_id: str (if exactly one match)
            - project_name: str (if exactly one match)
            - error: str (if failure - 'ambiguous_results' or 'not_found')
            - message: str (user-friendly message)
            - matches: list (if ambiguous)
        """
        headers = {}
        if auth_token:
            if auth_token.startswith("Bearer "):
                headers["Authorization"] = auth_token
            else:
                headers["Authorization"] = f"Bearer {auth_token}"

        try:
            response = await self._make_request(
                "GET",
                "/projects",
                headers=headers or None,
                params={"search": project_name}
            )

            if response.status_code == 401:
                logger.warning("Authentication failed for project search")
                return {
                    "success": False,
                    "error": "authentication_failed",
                    "message": "Authentication required. Please ensure you are logged in."
                }

            response.raise_for_status()
            projects = response.json()

            if len(projects) == 0:
                logger.info("No projects found", search_term=project_name)
                return {
                    "success": False,
                    "error": "not_found",
                    "message": f"No project found matching '{project_name}'. Please check the project name and try again."
                }

            if len(projects) == 1:
                project = projects[0]
                logger.info(
                    "Single project match found",
                    project_id=project["id"],
                    project_name=project["name"]
                )
                return {
                    "success": True,
                    "project_id": project["id"],
                    "project_name": project["name"]
                }

            # Multiple matches - ambiguous
            logger.info(
                "Multiple projects found",
                search_term=project_name,
                match_count=len(projects)
            )
            matches = [
                {"project_id": p["id"], "project_name": p["name"]}
                for p in projects
            ]
            project_names = ", ".join(p["name"] for p in projects)
            return {
                "success": False,
                "error": "ambiguous_results",
                "message": f"Found {len(projects)} projects matching '{project_name}': {project_names}. Please be more specific.",
                "matches": matches
            }

        except httpx.HTTPStatusError as e:
            logger.error(
                "Project search failed",
                status_code=e.response.status_code,
                error=str(e)
            )
            return {
                "success": False,
                "error": "service_error",
                "message": f"Failed to search projects: HTTP {e.response.status_code}"
            }
        except Exception as e:
            logger.error(
                "Unexpected error during project search",
                error=str(e),
                error_type=type(e).__name__
            )
            return {
                "success": False,
                "error": "service_error",
                "message": f"Failed to search projects: {type(e).__name__}"
            }


class RetrievalServiceAdapter(UpstreamServiceAdapter):
    """
    HTTP adapter for Neo4j Retrieval Service.
    
    Used by retrieve_context tool for DRIFT search.
    """

    def __init__(self):
        http_settings = get_http_client_settings()
        base_url = get_retrieval_service_url()
        super().__init__(base_url, http_settings)

    async def retrieve(
        self,
        query: str,
        project_id: str,
        top_k: int = 5,
        prompt_id: str | None = None,
        auth_token: str | None = None
    ) -> dict[str, Any]:
        """
        Retrieve context from Knowledge Graph using DRIFT search.
        
        Args:
            query: Technical question to answer
            project_id: UUID of the target project
            top_k: Number of results to return
            prompt_id: Optional trace ID for progress tracking
            auth_token: Optional Bearer token for authentication
            
        Returns:
            Dict with:
            - final_answer: str
            - key_facts: list of facts with citations
            - residual_uncertainty: str
            - no_data_found: bool
        """
        headers = {"Content-Type": "application/json"}
        if auth_token:
            if auth_token.startswith("Bearer "):
                headers["Authorization"] = auth_token
            else:
                headers["Authorization"] = f"Bearer {auth_token}"

        payload = {
            "query": query,
            "project_id": project_id,
            "top_k": top_k,
        }
        if prompt_id:
            payload["prompt_id"] = prompt_id

        try:
            response = await self._make_request(
                "POST",
                "/retrieve",
                headers=headers,
                json=payload
            )

            if response.status_code == 401:
                logger.warning("Authentication failed for retrieval")
                return {
                    "final_answer": "",
                    "key_facts": [],
                    "residual_uncertainty": "Authentication failed",
                    "no_data_found": True
                }

            response.raise_for_status()
            result = response.json()

            logger.info(
                "Retrieval completed",
                project_id=project_id,
                has_answer=bool(result.get("final_answer")),
                fact_count=len(result.get("key_facts", []))
            )
            return result

        except httpx.HTTPStatusError as e:
            logger.error(
                "Retrieval request failed",
                project_id=project_id,
                status_code=e.response.status_code,
                error=str(e)
            )
            return {
                "final_answer": "",
                "key_facts": [],
                "residual_uncertainty": f"Retrieval failed: HTTP {e.response.status_code}",
                "no_data_found": True
            }
        except Exception as e:
            logger.error(
                "Unexpected error during retrieval",
                project_id=project_id,
                error=str(e),
                error_type=type(e).__name__
            )
            return {
                "final_answer": "",
                "key_facts": [],
                "residual_uncertainty": f"Retrieval failed: {type(e).__name__}",
                "no_data_found": True
            }
