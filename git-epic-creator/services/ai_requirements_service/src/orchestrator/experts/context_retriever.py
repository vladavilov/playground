from typing import List, Any
import httpx
from tenacity import AsyncRetrying, stop_after_attempt, wait_exponential
import structlog
import config
from workflow_models.agent_models import PromptAnalysis, RetrievedContext, Citation

logger = structlog.get_logger(__name__)


class ContextRetriever:
    async def retrieve(self, analysis: PromptAnalysis, project_id: Any, auth_header: str) -> RetrievedContext:
        data = await self._retrieve_from_provider(analysis.prompt, analysis.intents, project_id, auth_header=auth_header)
        key_facts = []
        citations: list[Citation] = []
        try:
            for kf in data.get("key_facts", []) or []:
                fact = kf.get("fact")
                if isinstance(fact, str) and fact:
                    key_facts.append(fact)
                # Parse citations from key_facts (may be strings or dicts)
                for cid in kf.get("citations", []) or []:
                    try:
                        if isinstance(cid, str):
                            # Legacy format: just chunk_id
                            citations.append(Citation(
                                chunk_id=cid,
                                text_preview="",
                                document_name="unknown"
                            ))
                        elif isinstance(cid, dict):
                            # New format: full citation object
                            chunk_id = str(cid.get("chunk_id", ""))
                            span = str(cid.get("span", ""))
                            doc_name = str(cid.get("document_name", "unknown"))
                            text_preview = span[:150] + "..." if len(span) > 150 else span
                            citations.append(Citation(
                                chunk_id=chunk_id,
                                text_preview=text_preview,
                                document_name=doc_name
                            ))
                    except Exception as e:
                        logger.debug("citation_parse_error", error=str(e))
                        continue
            # Also include top-level citations (objects with chunk_id/span/document_name)
            for c in data.get("citations", []) or []:
                try:
                    if isinstance(c, dict) and "chunk_id" in c:
                        chunk_id = str(c.get("chunk_id", ""))
                        span = str(c.get("span", ""))
                        doc_name = str(c.get("document_name", "unknown"))
                        text_preview = span[:150] + "..." if len(span) > 150 else span
                        citations.append(Citation(
                            chunk_id=chunk_id,
                            text_preview=text_preview,
                            document_name=doc_name
                        ))
                except Exception as e:
                    logger.debug("citation_parse_error", error=str(e))
                    continue
        except Exception:
            pass
        
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
        base = settings.GRAPH_RAG_BASE_URL.rstrip("/")
        return f"{base}/retrieve"

    def _merge_query_with_intents(self, query: str, intents: List[str]) -> str:
        lines: List[str] = ["### Question", str(query).strip(), "", "### Intents"]
        intents = [i for i in intents if isinstance(i, str) and i.strip()]
        if intents:
            lines.extend([f"- {i.strip()}" for i in intents])
        else:
            lines.append("- (none)")
        return "\n".join(lines)

    async def _retrieve_from_provider(self, query: str, intents: List[str], project_id: Any, auth_header: str):
        # Strict validation: auth_header is required for authentication to retrieval service
        if not auth_header or not isinstance(auth_header, str):
            raise ValueError(
                "Authentication header is required for retrieval service. "
                "Ensure auth_header is properly passed through the workflow state."
            )
        
        settings = config.get_ai_requirements_settings()
        merged = self._merge_query_with_intents(query, intents)
        payload = {"query": merged, "top_k": settings.RETRIEVAL_TOP_K, "project_id": str(project_id)}

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
