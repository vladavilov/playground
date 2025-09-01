from typing import Any

import httpx
from tenacity import AsyncRetrying, stop_after_attempt, wait_exponential
import config
from models.retrieval import RetrievalPlan, ContextPack


class GraphRAGClient:
    def __init__(self) -> None:
        self._settings = config.get_ai_workflow_settings()

    def _build_url(self) -> str:
        base = self._settings.GRAPH_RAG_BASE_URL.rstrip("/")
        path = getattr(self._settings, "GRAPH_RAG_RETRIEVE_PATH", "/retrieve")
        return f"{base}{path}"

    async def _call(self, payload: Any) -> Any:
        settings = config.get_ai_workflow_settings()
        timeout = float(getattr(settings, "GRAPH_RAG_TIMEOUT_SEC", 5.0))
        attempts = int(getattr(settings, "RETRIEVAL_MAX_ATTEMPTS", 3))
        backoff = float(getattr(settings, "RETRIEVAL_BACKOFF_BASE_SEC", 0.2))
        async for attempt in AsyncRetrying(reraise=True, stop=stop_after_attempt(attempts), wait=wait_exponential(multiplier=backoff)):
            with attempt:
                async with httpx.AsyncClient(timeout=timeout) as client:
                    resp = await client.post(self._build_url(), json=payload)
                return resp.json() if hasattr(resp, "json") else {}

    async def retrieve(self, plan: RetrievalPlan) -> ContextPack:
        payload = plan.model_dump()
        try:
            data = await self._call(payload)
            return ContextPack.from_upstream(data)
        except Exception:
            # Partial aggregation placeholder: return empty pack on failure
            return ContextPack(citations=[], snippets=[], provenance=[])



