from typing import List, Any
import httpx
from tenacity import AsyncRetrying, stop_after_attempt, wait_exponential
import structlog
import config
from workflow_models.agent_models import PromptAnalysis, RetrievedContext, Citation
from utils.citation_parser import parse_citations_from_response, parse_key_facts

logger = structlog.get_logger(__name__)


class ContextRetriever:
    async def retrieve(self, analysis: PromptAnalysis, project_id: Any, auth_header: str, prompt_id: Any = None) -> RetrievedContext:
        data = await self._retrieve_from_provider(analysis.prompt, analysis.intents, project_id, auth_header=auth_header, prompt_id=prompt_id)
        
        # Parse response using shared utility
        key_facts = parse_key_facts(data)
        citations = parse_citations_from_response(data, Citation)
        
        # Deduplicate citations by chunk_id while preserving order
        seen = set()
        dedup_citations = []
        for c in citations:
            if c.chunk_id not in seen:
                seen.add(c.chunk_id)
                dedup_citations.append(c)
        
        # Log retrieval results to distinguish genuine absence vs failure
        context_answer = str(data.get("final_answer", ""))
        logger.info(
            "retrieval_context_parsed",
            context_answer_length=len(context_answer),
            key_facts_count=len(key_facts),
            citations_count=len(dedup_citations),
            project_id=str(project_id),
        )
        
        if not key_facts:
            logger.warning(
                "retrieval_key_facts_empty",
                has_context_answer=bool(context_answer),
                raw_response_keys=list(data.keys()),
                message="No key facts returned from retrieval service. This may indicate sparse graph data or retrieval failure."
            )
        
        return RetrievedContext(
            context_answer=context_answer,
            key_facts=key_facts,
            citations=dedup_citations,
        )

    def _build_url(self) -> str:
        settings = config.get_ai_requirements_settings()
        base = settings.http.GRAPH_RAG_SERVICE_URL.rstrip("/")
        return f"{base}/retrieve"

    def _merge_query_with_intents(self, query: str, intents: List[str]) -> str:
        lines: List[str] = ["### Question", str(query).strip(), "", "### Intents"]
        intents = [i for i in intents if isinstance(i, str) and i.strip()]
        if intents:
            lines.extend([f"- {i.strip()}" for i in intents])
        else:
            lines.append("- (none)")
        return "\n".join(lines)

    async def _retrieve_from_provider(self, query: str, intents: List[str], project_id: Any, auth_header: str, prompt_id: Any = None):
        # Strict validation: auth_header is required for authentication to retrieval service
        if not auth_header or not isinstance(auth_header, str):
            raise ValueError(
                "Authentication header is required for retrieval service. "
                "Ensure auth_header is properly passed through the workflow state."
            )
        
        settings = config.get_ai_requirements_settings()
        merged = self._merge_query_with_intents(query, intents)
        payload = {
            "query": merged, 
            "top_k": settings.RETRIEVAL_TOP_K, 
            "project_id": str(project_id),
            "prompt_id": str(prompt_id) if prompt_id else None
        }

        timeout = settings.http.READ_TIMEOUT
        attempts = settings.RETRY_MAX_ATTEMPTS
        backoff = settings.RETRIEVAL_BACKOFF_BASE_SEC
        async for attempt in AsyncRetrying(
            reraise=True,
            stop=stop_after_attempt(attempts),
            wait=wait_exponential(multiplier=backoff),
        ):
            with attempt:
                async with httpx.AsyncClient(timeout=timeout) as client:
                    headers = {"Authorization": auth_header}
                    resp = await client.post(self._build_url(), json=payload, headers=headers)
                try:
                    resp.raise_for_status()
                except Exception:
                    try:
                        return resp.json()  # Let caller handle missing fields gracefully
                    except Exception:
                        return {"error": str(resp.text)}
                return resp.json() if hasattr(resp, "json") else {}
