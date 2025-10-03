"""HTTP client for GraphRAG retrieval service."""

from typing import Any, Dict, List
import httpx
from tenacity import AsyncRetrying, stop_after_attempt, wait_exponential
import structlog

logger = structlog.get_logger(__name__)


class GraphRAGClient:
    """Client for communicating with neo4j_retrieval_service."""

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

    def _build_url(self) -> str:
        return f"{self.base_url}/retrieve"

    async def retrieve(
        self,
        query: str,
        project_id: str,
        top_k: int = 2,
        auth_header: str | None = None,
    ) -> Dict[str, Any]:
        """Retrieve context from GraphRAG service.
        
        Args:
            query: Search query
            project_id: Project identifier
            top_k: Number of top results to retrieve
            auth_header: Optional authorization header value
            
        Returns:
            Dict with keys: final_answer, key_facts, citations
        """
        payload = {"query": query, "top_k": top_k, "project_id": project_id}

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
                    
                    logger.info(
                        "Retrieving context from GraphRAG",
                        url=self._build_url(),
                        project_id=project_id,
                        top_k=top_k,
                    )
                    
                    resp = await client.post(self._build_url(), json=payload, headers=headers)
                    
                try:
                    resp.raise_for_status()
                except Exception as e:
                    logger.warning(
                        "GraphRAG request failed",
                        status_code=resp.status_code,
                        error=str(e),
                    )
                    try:
                        return resp.json()
                    except Exception:
                        return {"error": str(resp.text)}
                
                return resp.json() if hasattr(resp, "json") else {}


