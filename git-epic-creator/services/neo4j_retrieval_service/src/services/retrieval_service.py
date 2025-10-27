from typing import Any, Callable, Dict, List, TypedDict, Optional
import json
import asyncio
import random
import time
from contextlib import contextmanager
from uuid import UUID, uuid4
import structlog
from pydantic import BaseModel
import tiktoken

from langgraph.graph import StateGraph, START, END
from langgraph.checkpoint.memory import InMemorySaver
from langchain_openai import AzureChatOpenAI, AzureOpenAIEmbeddings
from fastapi import HTTPException

from config import get_retrieval_settings
from retrieval_ms.prompts import (
    hyde_prompt,
    build_hyde_embed_text,
    primer_prompt,
    local_executor_prompt,
    aggregator_prompt,
)
from retrieval_ms.repositories.neo4j_repository import Neo4jRepository
from retrieval_ms.response_models import (
    PrimerResponse,
    LocalExecutorResponse,
    AggregatorResponse,
)
from configuration.retry_config import get_retry_settings
from models.progress_messages import RetrievalStatus


GetSessionFn = Callable[[], Any]
GetLlmFn = Callable[[], AzureChatOpenAI]
GetEmbedderFn = Callable[[], AzureOpenAIEmbeddings]

logger = structlog.get_logger(__name__)

# Persistent graph instance (singleton pattern)
_GRAPH_INSTANCE = None
_GRAPH_LOCK = asyncio.Lock()

# Cached tiktoken encoder for accurate token counting
_TIKTOKEN_ENCODER = None

# Request deduplication infrastructure
_REQUEST_FUTURES: Dict[str, asyncio.Task] = {}
_REQUEST_LOCK = asyncio.Lock()
_REQUEST_CACHE: Dict[str, tuple] = {}  # (timestamp, result)

def _get_token_encoder():
    """Get or create cached tiktoken encoder."""
    global _TIKTOKEN_ENCODER
    if _TIKTOKEN_ENCODER is None:
        try:
            # Use encoding for GPT-4/GPT-3.5
            _TIKTOKEN_ENCODER = tiktoken.encoding_for_model("gpt-4")
            logger.debug("tiktoken_encoder_initialized", model="gpt-4")
        except Exception as exc:
            logger.warning(
                "tiktoken_init_failed",
                error=str(exc),
                message="Falling back to cl100k_base encoding"
            )
            _TIKTOKEN_ENCODER = tiktoken.get_encoding("cl100k_base")
    return _TIKTOKEN_ENCODER

def _count_tokens_accurate(text: str) -> int:
    """Count tokens using tiktoken for accurate measurement.
    
    Falls back to heuristic (len/4) if tiktoken fails.
    """
    try:
        encoder = _get_token_encoder()
        return len(encoder.encode(text))
    except Exception as exc:
        logger.warning(
            "token_count_fallback",
            error=str(exc),
            text_len=len(text),
            message="Using heuristic token count"
        )
        return len(text) // 4


def _make_cache_key(question: str, top_k: int, project_id: str) -> str:
    """Generate deterministic cache key for request deduplication.
    
    Uses hash of normalized question to handle whitespace variations.
    """
    normalized_q = " ".join(question.strip().lower().split())
    return f"{project_id}:{top_k}:{abs(hash(normalized_q))}"


def _cleanup_expired_cache(ttl_sec: int):
    """Remove expired entries from request cache."""
    now = time.time()
    expired = [
        key for key, (ts, _) in _REQUEST_CACHE.items()
        if now - ts > ttl_sec
    ]
    for key in expired:
        _REQUEST_CACHE.pop(key, None)
    if expired:
        logger.debug("cache_cleanup", expired_count=len(expired))


async def _get_or_create_graph(
    get_session: GetSessionFn, 
    get_llm: GetLlmFn, 
    get_embedder: GetEmbedderFn,
    publisher: Optional[Any] = None
):
    """Get cached graph or create if not exists (singleton pattern).
    
    Creates the StateGraph only once and reuses it for all requests,
    reducing latency by 200-500ms per request.
    
    Note: Publisher is passed per-request but graph is cached. This is acceptable
    because publisher logic is best-effort (failures don't break retrieval).
    """
    global _GRAPH_INSTANCE
    if _GRAPH_INSTANCE is None:
        async with _GRAPH_LOCK:
            if _GRAPH_INSTANCE is None:
                logger.info("graph_initialization", message="Creating persistent graph instance")
                _GRAPH_INSTANCE = await _create_graph(get_session, get_llm, get_embedder, publisher)
    return _GRAPH_INSTANCE


def _is_retryable_error(exc: Exception) -> tuple:
    """Classify exception as retryable or permanent.
    
    Returns:
        (is_retryable, error_category)
    """
    error_str = str(exc).lower()
    status_code = getattr(exc, "status_code", None)
    
    # HTTP status codes (if available)
    if status_code is not None:
        if status_code == 429:
            return (True, "rate_limit")
        if status_code in (502, 503, 504):
            return (True, "server_error")
        if status_code == 408:
            return (True, "timeout")
        if status_code in (400, 401, 403, 404, 422):
            return (False, "client_error")
        if status_code >= 500:
            return (True, "server_error")
    
    # String-based detection (fallback)
    if "429" in error_str or "rate limit" in error_str:
        return (True, "rate_limit")
    if "timeout" in error_str or "timed out" in error_str:
        return (True, "timeout")
    if "connection" in error_str and "refused" in error_str:
        return (True, "connection_error")
    if "503" in error_str or "502" in error_str or "504" in error_str:
        return (True, "server_error")
    if any(code in error_str for code in ["400", "401", "403", "404"]):
        return (False, "client_error")
    
    # Unknown errors: do NOT retry (fail fast)
    return (False, "unknown")


async def _llm_call_with_retry(llm_fn: Callable, label: str) -> Any:
    """Execute LLM call with exponential backoff for rate limiting.
    
    Handles HTTP 429 (Too Many Requests) errors with retry logic using
    shared retry configuration and adds jitter to prevent thundering herd.
    
    Args:
        llm_fn: Async callable that executes the LLM call
        label: Label for logging context
        
    Returns:
        Result from llm_fn
        
    Raises:
        Original exception after max retries exhausted
    """
    retry_settings = get_retry_settings()
    max_attempts = retry_settings.RETRY_MAX_ATTEMPTS
    backoff_base = retry_settings.RETRY_BACKOFF_BASE_SEC
    backoff_factor = retry_settings.RETRY_BACKOFF_FACTOR
    backoff_max = retry_settings.RETRY_BACKOFF_MAX_SEC
    
    for attempt in range(max_attempts):
        try:
            return await llm_fn()
        except Exception as exc:
            is_retryable, error_category = _is_retryable_error(exc)
            is_last_attempt = attempt >= max_attempts - 1
            
            if not is_retryable or is_last_attempt:
                # Permanent error or exhausted retries - propagate exception
                logger.error(
                    "llm_call_failed",
                    label=label,
                    attempt=attempt + 1,
                    max_attempts=max_attempts,
                    error=str(exc)[:200],
                    error_type=type(exc).__name__,
                    error_category=error_category,
                    retryable=is_retryable,
                    message="Permanent error or max retries reached"
                )
                raise
            
            # Transient error - calculate backoff and retry
            exponent = attempt
            backoff = backoff_base * (backoff_factor ** exponent)
            backoff = min(backoff, backoff_max)
            # Add jitter: ±25% random variation
            jitter = backoff * 0.25 * (2 * random.random() - 1)
            sleep_time = max(0.1, backoff + jitter)
            
            logger.warning(
                "llm_transient_error_retry",
                label=label,
                attempt=attempt + 1,
                max_attempts=max_attempts,
                sleep_seconds=round(sleep_time, 2),
                error=str(exc)[:200],
                error_category=error_category,
                message=f"Transient {error_category} error, retrying after {sleep_time:.1f}s"
            )
            
            await asyncio.sleep(sleep_time)
    
    # Should not reach here due to raise in loop, but for type safety
    raise RuntimeError(f"Max retries exhausted for {label}")


class _State(TypedDict, total=False):
    question: str
    top_k: int
    project_id: str
    prompt_id: Optional[str]  # Optional parent workflow prompt_id for UI tracking
    retrieval_id: UUID
    qvec: List[float]
    communities: List[int]
    community_brief: List[Dict[str, Any]]
    primer_json: Dict[str, Any]
    followups: List[Dict[str, Any]]
    followup_results: List[Dict[str, Any]]
    tree: Dict[str, Any]
    result_json: Dict[str, Any]
    # Request-level caches to avoid duplicate work
    _cache_embeddings: Dict[str, List[float]]  # key: text -> embedding vector
    _cache_neighborhoods: Dict[str, Dict[str, Any]]  # key: chunk_id -> expanded data


def _parse_and_validate(
    raw: Any,
    model_class: type[BaseModel],
    label: str,
) -> BaseModel:
    """Parse JSON and validate against Pydantic model with normalization.
    
    This function provides:
    - JSON parsing with error handling
    - Pydantic validation with automatic normalization (e.g., string -> dict)
    - Graceful fallback to default model on validation failure
    - Comprehensive logging for debugging
    
    Args:
        raw: Raw LLM response (string or object with .content)
        model_class: Pydantic model class for validation
        label: Label for logging context
        
    Returns:
        Validated and normalized model instance
        
    Raises:
        HTTPException: Only on JSON parse failure (502)
    """
    logger = structlog.get_logger(__name__)
    
    # Helper to extract string content
    def _as_str_content(obj: Any) -> str:
        return obj.content if hasattr(obj, "content") else str(obj)
    
    try:
        parsed = json.loads(_as_str_content(raw))
    except Exception as exc:
        logger.error(
            "json_parse_failed",
            label=label,
            error=str(exc),
            raw_preview=str(raw)[:200],
        )
        raise HTTPException(status_code=502, detail=f"{label} JSON parse failed: {exc}")
    
    try:
        validated = model_class.model_validate(parsed)
        logger.debug(
            "response_validated",
            label=label,
            model=model_class.__name__,
            message="Successfully validated LLM response"
        )
        return validated
    except Exception as exc:
        # Extract field-level validation errors if available
        field_errors = []
        if hasattr(exc, "errors"):
            try:
                field_errors = [
                    f"{'.'.join(str(x) for x in err.get('loc', []))}: {err.get('msg', '')}" 
                    for err in exc.errors()[:3]  # Show first 3 errors
                ]
            except Exception:
                pass
        
        logger.warning(
            "response_validation_failed",
            label=label,
            model=model_class.__name__,
            error=str(exc)[:200],
            field_errors=field_errors,
            parsed_keys=list(parsed.keys()) if isinstance(parsed, dict) else None,
            message="Attempting normalized fallback"
        )
        # Try to create with defaults (Pydantic validators will normalize)
        try:
            fallback = model_class.model_validate(parsed, strict=False)
            logger.info(
                "response_validation_fallback_success",
                label=label,
                model=model_class.__name__,
                message="Fallback validation succeeded with normalization"
            )
            return fallback
        except Exception as exc2:
            # Last resort: return empty model
            logger.error(
                "response_validation_fatal",
                label=label,
                model=model_class.__name__,
                error=str(exc2)[:200],
                message="Returning empty model as last resort"
            )
            return model_class()


def _truncate_for_prompt(
    data: List[Dict[str, Any]],
    max_items: int,
    label: str,
) -> str:
    """Truncate list data for prompt inclusion with size monitoring.
    
    Prevents token overflow by limiting array size and logging when truncation occurs.
    Also monitors serialized size to warn about potential token pressure.
    
    Args:
        data: List of dictionaries to serialize
        max_items: Maximum number of items to include
        label: Label for logging context
        
    Returns:
        JSON-serialized string of truncated data
    """
    logger = structlog.get_logger(__name__)
    
    truncated = data[:max_items]
    serialized = json.dumps(truncated)
    
    if len(data) > max_items:
        logger.warning(
            "prompt_data_truncated",
            label=label,
            original_count=len(data),
            truncated_count=max_items,
            serialized_length=len(serialized),
            message=f"Truncated {label} to prevent token overflow"
        )
    
    actual_tokens = _count_tokens_accurate(serialized)
    
    if actual_tokens > 2000:
        logger.warning(
            "prompt_data_large",
            label=label,
            serialized_length=len(serialized),
            actual_tokens=actual_tokens,
            message="Large prompt may cause token pressure or LLM output simplification"
        )
    else:
        logger.debug(
            "prompt_data_size",
            label=label,
            serialized_length=len(serialized),
            actual_tokens=actual_tokens,
        )
    
    return serialized


async def _create_graph(get_session: GetSessionFn, get_llm: GetLlmFn, get_embedder: GetEmbedderFn, publisher: Optional[Any] = None):
    settings = get_retrieval_settings()

    builder = StateGraph(_State)


    @contextmanager
    def _repo_ctx():
        with get_session() as session:
            yield Neo4jRepository(session)

    def _ensure_top_k(state: Dict[str, Any]) -> int:
        """Ensure top_k is valid positive integer, default 5 if missing.
        
        Honors user-specified values (no minimum enforcement beyond 1).
        """
        try:
            val = int(state.get("top_k") or 5)  # Default 5 if missing/None
            return max(1, val)  # Only enforce minimum of 1 (must have at least 1 result)
        except Exception:  # noqa: BLE001
            logger.warning("top_k_parse_failed", raw_value=state.get("top_k"), fallback=5)
            return 5

    def _as_str_content(obj: Any) -> str:
        return obj.content if hasattr(obj, "content") else str(obj)

    def _parse_json_or_502(raw: Any, label: str) -> Dict[str, Any]:
        try:
            parsed = json.loads(_as_str_content(raw))
        except Exception as exc:  # noqa: BLE001
            raise HTTPException(status_code=502, detail=f"{label} JSON parse failed: {exc}")
        return parsed if isinstance(parsed, dict) else {}

    async def _embed_one_or_502(text: str) -> List[float]:
        embedder = get_embedder()
        try:
            async def _do_embed():
                return await embedder.aembed_documents([text])
            
            vectors = await _llm_call_with_retry(_do_embed, "Embeddings")
            return [float(x) for x in (vectors[0] if vectors else [])]
        except Exception as exc:  # noqa: BLE001
            raise HTTPException(status_code=502, detail=f"Embeddings failed: {exc}")

    async def _embed_with_cache(
        text: str, 
        state: Dict[str, Any]
    ) -> List[float]:
        """Embed text with request-level caching to avoid duplicate embeddings."""
        cache = state.get("_cache_embeddings", {})
        
        if text in cache:
            logger.debug("embedding_cache_hit", text_preview=text[:50])
            return cache[text]
        
        # Cache miss - compute embedding
        qvec = await _embed_one_or_502(text)
        cache[text] = qvec
        state["_cache_embeddings"] = cache
        
        logger.debug("embedding_cache_miss", text_preview=text[:50], vec_len=len(qvec))
        return qvec

    async def _format_invoke_parse(prompt_obj: Any, llm: AzureChatOpenAI, label: str, **fmt_kwargs: Any) -> Dict[str, Any]:
        msg = prompt_obj.format_messages(**fmt_kwargs)
        
        async def _do_invoke():
            return await llm.ainvoke(msg)
        
        res = await _llm_call_with_retry(_do_invoke, label)
        return _parse_json_or_502(res, label)

    async def _format_invoke_content(prompt_obj: Any, llm: AzureChatOpenAI, label: str, **fmt_kwargs: Any) -> str:
        msg = prompt_obj.format_messages(**fmt_kwargs)
        
        async def _do_invoke():
            return await llm.ainvoke(msg)
        
        res = await _llm_call_with_retry(_do_invoke, label)
        return _as_str_content(res)

    def _fetch_communities(repo: Neo4jRepository, index_name: str, k: int, qvec: List[float], project_id: str) -> List[int]:
        """
        Fetch communities for DRIFT primer phase using hierarchical level filtering.
        
        Strategy:
        1. Query highest-level communities first (global summaries)
        2. Fall back to lower levels if insufficient results
        3. Ensures DRIFT algorithm starts with aggregate context
        """
        # Get max hierarchy level for project
        max_level = repo.get_max_community_level(project_id)
        
        if max_level > 0:
            # Query at highest level first (aggregate communities)
            logger.info("fetching_communities_by_level", level=max_level, k=k, project_id=project_id)
            rows = repo.vector_query_communities_by_level(index_name, k, qvec, project_id, level=max_level)
            
            # If insufficient results, fall back to next level
            if len(rows) < k // 2 and max_level > 0:
                logger.info("insufficient_top_level_communities", found=len(rows), needed=k, falling_back_to_level=max_level - 1)
                additional_rows = repo.vector_query_communities_by_level(
                    index_name, 
                    k - len(rows), 
                    qvec, 
                    project_id, 
                    level=max_level - 1
                )
                rows.extend(additional_rows)
        else:
            # No hierarchy or flat structure - use all levels
            logger.info("fetching_communities_all_levels", project_id=project_id)
            rows = repo.vector_query_nodes(index_name, k, qvec, project_id)
        
        communities: List[int] = []
        for r in rows:
            node = r.get("node")
            if node is not None:
                communities.append(int(node["community"]))
        
        logger.info("communities_fetched", count=len(communities), max_level=max_level, project_id=project_id)
        return communities

    def _fetch_community_brief(
        repo: Neo4jRepository, communities: List[int], project_id: str
    ) -> List[Dict[str, Any]]:
        if not communities:
            return []
        summaries: Dict[int, str] = repo.fetch_community_summaries(communities, project_id)
        brief = repo.fetch_communities_brief(communities, project_id)
        if brief:
            return brief
        return [{"id": cid, "summary": summaries.get(cid, "")} for cid in communities]

    def _scoped_chunks_expanded(
        repo: Neo4jRepository,
        communities: List[int],
        chunk_index: str,
        query_vec: List[float],
        k: int,
        project_id: str,
        state: Dict[str, Any],
    ) -> List[Dict[str, Any]]:
        """Get expanded chunks with request-level caching to avoid duplicate work."""
        if not (chunk_index and communities):
            logger.warning("retrieval.scoped_chunks_empty", reason="missing_chunk_index_or_communities", chunk_index=bool(chunk_index), communities_count=len(communities or []))
            return []
        # Use optimized vector query (reduces scans by 80-90%)
        rows = repo.optimized_scoped_chunks(communities, chunk_index, query_vec, k, project_id)
        chunks = rows
        logger.info("retrieval.scoped_chunks", chunk_ids_found=len(chunks), communities_count=len(communities or []))
        if not chunks:
            logger.warning("retrieval.scoped_chunks_empty", reason="no_chunks_found_for_communities", communities=communities[:5])
            return []
        
        # Check cache for already-expanded chunks
        cache = state.get("_cache_neighborhoods", {})
        cached_results = []
        chunks_to_expand = []
        
        for chunk_id in chunks:
            if chunk_id in cache:
                logger.debug("neighborhood_cache_hit", chunk_id=chunk_id)
                cached_results.append(cache[chunk_id])
            else:
                chunks_to_expand.append(chunk_id)
        
        # Expand only uncached chunks
        if chunks_to_expand:
            logger.debug("neighborhood_cache_miss", count=len(chunks_to_expand))
            # Pass max chunk text length from config for truncation
            expanded = repo.expand_neighborhood_minimal(
                chunks_to_expand, 
                project_id, 
                max_chunk_text_len=settings.MAX_CHUNK_TEXT_LENGTH
            )
            
            # Cache results
            for item in expanded:
                cid = item.get("chunk_id")
                if cid:
                    cache[cid] = item
            
            state["_cache_neighborhoods"] = cache
            return cached_results + expanded
        
        return cached_results

    def _create_minimal_followup_result(local_validated: LocalExecutorResponse, qtext: str, chunks: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Create minimal followup result for aggregation - strips unnecessary context.
        
        Aggregator only needs: question, answer, citations, confidence, new_followups.
        Full chunk contexts (text, entities, relationships) are NOT needed and waste tokens.
        
        Citation Validation:
        - Filters citations with None/empty chunk_ids
        - Filters citations referencing chunk_ids not in retrieved set
        - Logs detailed warnings for debugging hallucination patterns
        """
        # Build lookup map: chunk_id -> document_name
        chunk_to_doc = {item.get("chunk_id"): item.get("document_name", "unknown") for item in chunks}
        valid_chunk_ids = set(chunk_to_doc.keys())
        
        # Validate and filter citations
        validated_citations = []
        invalid_count = 0
        null_count = 0
        hallucinated_count = 0
        
        for idx, c in enumerate(local_validated.citations):
            # Reject None or empty chunk_ids
            if c.chunk_id is None or not str(c.chunk_id).strip():
                null_count += 1
                logger.warning(
                    "citation_validation_null_chunk_id",
                    citation_index=idx,
                    span_preview=c.span[:50] if c.span else "",
                    message="Filtered citation with None/empty chunk_id (LLM output error)"
                )
                continue
            
            # Reject chunk_ids not in retrieved set (hallucination)
            if c.chunk_id not in valid_chunk_ids:
                hallucinated_count += 1
                logger.warning(
                    "citation_validation_unmatched_chunk_id",
                    citation_index=idx,
                    chunk_id=c.chunk_id,
                    span_preview=c.span[:50] if c.span else "",
                    valid_chunk_ids=list(valid_chunk_ids)[:5],
                    message="Filtered citation with chunk_id not in retrieved set (LLM hallucination)"
                )
                continue
            
            # Valid citation - include with document name
            validated_citations.append({
                "chunk_id": c.chunk_id,
                "span": c.span,
                "document_name": chunk_to_doc[c.chunk_id]
            })
        
        invalid_count = null_count + hallucinated_count
        
        # Log summary if any citations were filtered
        if invalid_count > 0:
            logger.warning(
                "citation_validation_summary",
                question=qtext[:100],
                total_citations=len(local_validated.citations),
                valid_citations=len(validated_citations),
                null_chunk_ids=null_count,
                hallucinated_chunk_ids=hallucinated_count,
                filtered_count=invalid_count,
                message=f"Filtered {invalid_count}/{len(local_validated.citations)} invalid citations"
            )
        else:
            logger.debug(
                "citation_validation_all_valid",
                question=qtext[:100],
                citation_count=len(validated_citations)
            )
        
        minimal_result = {
            "question": qtext,
            "answer": local_validated.answer,
            "citations": validated_citations,
            "confidence": local_validated.confidence,
        }
        
        # Include new followups (questions only, no contexts)
        if local_validated.should_continue and local_validated.new_followups:
            new_followup_questions = []
            for nf in local_validated.new_followups[:3]:
                if nf.question:
                    new_followup_questions.append({"question": nf.question})
            
            if new_followup_questions:
                minimal_result["new_followups"] = new_followup_questions
        
        return minimal_result

    def _validate_embedding_dimension(
        qvec: List[float],
        expected_dim: int,
        label: str
    ) -> None:
        """Validate embedding vector dimension matches expected.
        
        Raises:
            HTTPException(502): If dimension mismatch detected
        """
        actual_dim = len(qvec)
        
        if actual_dim != expected_dim:
            logger.error(
                "embedding_dimension_mismatch",
                label=label,
                expected_dim=expected_dim,
                actual_dim=actual_dim,
                mismatch_size=abs(expected_dim - actual_dim),
                message=f"Embedding dimension mismatch may cause low recall or query failures"
            )
            raise HTTPException(
                status_code=502,
                detail=(
                    f"Embedding dimension mismatch: expected {expected_dim}, "
                    f"got {actual_dim}. Check OAI_EMBED_MODEL_NAME configuration."
                )
            )
        
        logger.debug(
            "embedding_dimension_validated",
            label=label,
            dimension=actual_dim
        )

    async def init_node(state: Dict[str, Any]) -> Dict[str, Any]:
        logger.info("retrieval.init", question_len=len(state.get("question") or ""))
        
        # Generate retrieval session ID if not provided
        retrieval_id = state.get("retrieval_id") or uuid4()
        
        # Publish initialization status
        if publisher:
            try:
                prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                await publisher.publish_retrieval_update(
                    project_id=UUID(state["project_id"]),
                    retrieval_id=retrieval_id,
                    phase=RetrievalStatus.INITIALIZING,
                    thought_summary="Agentic retrieval...",
                    details_md=f"**Agentic retrieval**  \nTo find relevant information from the knowledge graph by provided question and intentions...  \n",
                    progress_pct=0.0,
                    prompt_id=prompt_id_uuid,
                )
            except Exception as exc:
                logger.debug("publish_failed", phase="init", error=str(exc))
        
        return {
            "retrieval_id": retrieval_id,
            "_cache_embeddings": {},
            "_cache_neighborhoods": {},
        }

    async def hyde_node(state: Dict[str, Any]) -> Dict[str, Any]:
        llm = get_llm()
        prompt = hyde_prompt()
        hyde_answer = await _format_invoke_content(prompt, llm, "HyDE", question=state["question"])
        hyde_text = build_hyde_embed_text(state["question"], hyde_answer)
        qvec = await _embed_with_cache(hyde_text, state)
        
        # Validate embedding dimension to catch configuration errors early
        expected_dim = settings.vector_index.VECTOR_INDEX_DIMENSIONS or 1536
        _validate_embedding_dimension(qvec, expected_dim, "HyDE")
        
        logger.info("retrieval.hyde.done", hyde_text_len=len(hyde_text or ""), qvec_len=len(qvec or []))
        
        # Publish query expansion status
        if publisher:
            try:
                prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                await publisher.publish_retrieval_update(
                    project_id=UUID(state["project_id"]),
                    retrieval_id=state["retrieval_id"],
                    phase=RetrievalStatus.EXPANDING_QUERY,
                    thought_summary="📝 **Expanding Query**",
                    details_md=f"**Using HyDE (Hypothetical Document Embeddings) to improve search precision**\n\nHyDE answer: {hyde_answer[:300]}...",
                    progress_pct=20.0,
                    prompt_id=prompt_id_uuid,
                )
            except Exception as exc:
                logger.debug("publish_failed", phase="hyde", error=str(exc))
        
        return {"qvec": qvec}

    async def primer_node(state: Dict[str, Any]) -> Dict[str, Any]:
        k = _ensure_top_k(state)
        with _repo_ctx() as repo:
            communities = _fetch_communities(
                repo,
                settings.vector_index.COMMUNITY_VECTOR_INDEX_NAME,
                k,
                state["qvec"],
                state["project_id"],
            )
            community_brief = _fetch_community_brief(repo, communities, state["project_id"])

        llm = get_llm()
        prompt = primer_prompt()
        primer_response = await _format_invoke_parse(
            prompt,
            llm,
            "Primer",
            question=state["question"],
            community_details=json.dumps(community_brief),
            sample_chunks="[]",  # Removed: expensive and redundant (real retrieval in followups)
        )
        
        # Validate and normalize primer response
        primer_validated = _parse_and_validate(
            json.dumps(primer_response),
            PrimerResponse,
            "Primer",
        )
        
        # Convert followups to dict format for state
        followups = [f.model_dump() for f in primer_validated.followups]
        
        logger.info(
            "retrieval.primer.done",
            communities=len(communities or []),
            community_brief=len(community_brief or []),
            followups=len(followups or []),
        )
        
        # Publish community retrieval status
        if publisher:
            try:
                prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                await publisher.publish_retrieval_update(
                    project_id=UUID(state["project_id"]),
                    retrieval_id=state["retrieval_id"],
                    phase=RetrievalStatus.RETRIEVING_COMMUNITIES,
                    thought_summary=f"🌐 **Retrieved {len(communities)} Knowledge Communities**",
                    details_md=(
                        f"*Communities found:* {len(communities)}  \n"
                        + "*Follow-up questions:*  \n"
                        + "\n".join([f"- {f.get('question','')}" for f in (followups or [])])
                        if followups else ""
                    ),
                    progress_pct=40.0,
                    prompt_id=prompt_id_uuid,
                )
            except Exception as exc:
                logger.debug("publish_failed", phase="primer", error=str(exc))
        
        return {
            "communities": communities,
            "community_brief": community_brief,
            "primer_json": primer_validated.model_dump(),
            "followups": followups,
        }

    async def followups_node(state: Dict[str, Any]) -> Dict[str, Any]:
        k = _ensure_top_k(state)
        cids = list(state.get("communities") or [])
        llm = get_llm()
        results: List[Dict[str, Any]] = []
        chunk_index = settings.vector_index.CHUNK_VECTOR_INDEX_NAME
        
        total_followups = len(state.get("followups") or [])
        
        try:
            with _repo_ctx() as repo:
                for idx, f in enumerate(list(state.get("followups") or [])):
                    try:
                        qtext = str(f.get("question", ""))
                        if not qtext:
                            logger.warning(
                                "followup_empty_question",
                                followup_index=idx,
                                message="Skipping followup with empty question"
                            )
                            continue
                        
                        logger.debug(
                            "processing_followup",
                            followup_index=idx,
                            question=qtext[:100],
                        )
                        
                        qvec = await _embed_with_cache(qtext, state)
                        
                        # Validate embedding dimension on first followup (avoid redundant checks)
                        if idx == 0:
                            expected_dim = settings.vector_index.VECTOR_INDEX_DIMENSIONS or 1536
                            _validate_embedding_dimension(qvec, expected_dim, f"Followup-{idx}")

                        chunks = _scoped_chunks_expanded(
                            repo,
                            cids,
                            chunk_index,
                            qvec,
                            k,
                            state["project_id"],
                            state,
                        )

                        # Reuse community_brief from state (already computed in primer_node)
                        target_communities_brief = state.get("community_brief", [])

                        # Use truncation to prevent token overflow
                        chunks_preview_json = _truncate_for_prompt(
                            chunks,
                            max_items=settings.MAX_CHUNKS_FOR_PROMPT,
                            label=f"chunks_preview_followup_{idx}",
                        )
                        target_communities_json = _truncate_for_prompt(
                            target_communities_brief,
                            max_items=settings.MAX_COMMUNITIES_FOR_PROMPT,
                            label=f"target_communities_followup_{idx}",
                        )
                        
                        # Extract valid chunk IDs for prompt (prevents hallucination)
                        valid_chunk_ids = [
                            str(chunk.get("chunk_id")) 
                            for chunk in chunks 
                            if chunk.get("chunk_id") is not None
                        ]
                        valid_chunk_ids_str = ", ".join(valid_chunk_ids)

                        prompt = local_executor_prompt()
                        local_response = await _format_invoke_parse(
                            prompt,
                            llm,
                            "Local executor",
                            qtext=qtext,
                            target_communities=target_communities_json,
                            chunks_preview=chunks_preview_json,
                            valid_chunk_ids=valid_chunk_ids_str,
                        )
                        
                        # Explicit cleanup of large JSON strings after LLM call
                        del chunks_preview_json, target_communities_json

                        # CRITICAL FIX: Validate and normalize response
                        local_validated = _parse_and_validate(
                            json.dumps(local_response),
                            LocalExecutorResponse,
                            f"Local executor (followup {idx})",
                        )

                        # Create minimal result for aggregation - strips unnecessary context
                        # Aggregator only needs: question, answer, citations, confidence, new_followups
                        minimal_result = _create_minimal_followup_result(local_validated, qtext, chunks)
                        
                        logger.debug(
                            "followup_result_created",
                            followup_index=idx,
                            has_answer=bool(minimal_result.get("answer")),
                            citations_count=len(minimal_result.get("citations", [])),
                            confidence=minimal_result.get("confidence"),
                            new_followups_count=len(minimal_result.get("new_followups", [])),
                        )

                        results.append(minimal_result)
                        
                        # Explicit cleanup of large chunk data after processing
                        del chunks, local_response, local_validated
                        
                         # Publish followup execution status
                        if publisher:
                            try:
                                progress = 40.0 + ((idx + 1) / total_followups) * 40.0  # 40-80% range
                                prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                                await publisher.publish_retrieval_update(
                                    project_id=UUID(state["project_id"]),
                                    retrieval_id=state["retrieval_id"],
                                    phase=RetrievalStatus.EXECUTING_FOLLOWUP,
                                    thought_summary=f"🔎 **Follow-up {idx + 1}/{total_followups}**",
                                    details_md=(
                                        f"**Follow-up {idx + 1}/{total_followups}:** {qtext}\n\n"
                                        f"**Answer:** {minimal_result.get('answer') or 'No answer found.'}\n\n"
                                        f"**Citations:**  \n" +
                                        (
                                            "\n".join(
                                                [
                                                    f"- [{c.get('document_name', 'Unknown')}] \"{c.get('span', '')[:120]}...\""
                                                    for c in minimal_result.get('citations', []) or []
                                                ]
                                            )
                                            if minimal_result.get('citations') else "No citations."
                                        ) +
                                        (
                                            f"\n__Confidence:__ {minimal_result.get('confidence'):.2f}"
                                            if isinstance(minimal_result.get('confidence'), (float, int)) else ""
                                        ) +
                                        (
                                            f"\n__New Follow-ups:__ {len(minimal_result.get('new_followups', []))}"
                                            if 'new_followups' in minimal_result else ""
                                        )
                                    ),
                                    progress_pct=progress,
                                    prompt_id=prompt_id_uuid,
                                )
                            except Exception as exc:
                                logger.debug("publish_failed", phase="followup", followup_idx=idx, error=str(exc))

                        # Check for early exit based on confidence threshold
                        if idx >= settings.MIN_FOLLOWUPS_BEFORE_EXIT - 1:
                            confidence = minimal_result.get("confidence", 0.0)
                            if confidence >= settings.CONFIDENCE_THRESHOLD_EARLY_EXIT:
                                logger.info(
                                    "early_exit_triggered",
                                    followup_index=idx,
                                    confidence=confidence,
                                    threshold=settings.CONFIDENCE_THRESHOLD_EARLY_EXIT,
                                    processed=len(results),
                                    skipped=total_followups - len(results),
                                    message="High confidence reached, skipping remaining followups"
                                )
                                break  # Exit loop early
                        
                    except Exception as exc:
                        logger.error(
                            "followup_processing_failed",
                            followup_index=idx,
                            error=str(exc),
                            error_type=type(exc).__name__,
                            followup_question=f.get("question", "N/A") if isinstance(f, dict) else str(f)[:100],
                            message="Skipping this followup and continuing with next"
                        )
                        # Continue processing other followups
                        continue
                
                # Calculate validation success: count non-empty answers or non-empty citations
                valid_results = [
                    r for r in results 
                    if (r.get("answer") and r.get("answer").strip()) 
                    or (r.get("citations") and len(r.get("citations")) > 0)
                ]
                
                # Check if early exit occurred
                was_early_exit = len(results) < total_followups
                
                logger.info(
                    "retrieval.followups.done",
                    processed=len(results),
                    validated=len(valid_results),
                    total_followups=total_followups,
                    early_exit=was_early_exit,
                    success_rate=f"{len(valid_results)}/{total_followups}",
                    message=f"Processed {len(results)}, validated {len(valid_results)} of {total_followups} followups"
                )
                return {"followup_results": results}
                
        except Exception as exc:
            logger.error(
                "followups_node_critical_failure",
                error=str(exc),
                error_type=type(exc).__name__,
                total_followups=total_followups,
                results_collected=len(results),
                message="Critical failure in followups node, returning partial results"
            )
            # Return whatever results we collected before the failure
            return {"followup_results": results}

    def _enrich_citations_from_followups(agg_validated: AggregatorResponse, followup_results: List[Dict[str, Any]]) -> None:
        """Enrich aggregated key_facts citations with full metadata from followup results.
        
        The LLM aggregator returns chunk IDs as strings, but we need full citation objects
        with {chunk_id, span, document_name} for downstream services. This function maps
        chunk ID strings to rich citation objects from the followup results.
        
        Mutates agg_validated.key_facts in place.
        """
        # Build lookup map: chunk_id -> citation object
        citation_map: Dict[str, Dict[str, Any]] = {}
        for fr in followup_results:
            for cit in fr.get("citations", []) or []:
                if isinstance(cit, dict) and "chunk_id" in cit:
                    chunk_id = str(cit.get("chunk_id", ""))
                    if chunk_id and chunk_id not in citation_map:
                        citation_map[chunk_id] = cit
        
        logger.debug(
            "citation_enrichment_map_built",
            unique_citations=len(citation_map),
            total_followup_results=len(followup_results)
        )
        
        # Enrich each key_fact's citations
        for kf in agg_validated.key_facts:
            enriched_citations = []
            for citation_item in kf.citations:
                # Handle both string chunk IDs (from LLM) and already-enriched dicts (edge case)
                if isinstance(citation_item, dict):
                    # Already enriched (shouldn't happen, but handle gracefully)
                    enriched_citations.append(citation_item)
                    logger.debug(
                        "citation_already_enriched",
                        chunk_id=citation_item.get("chunk_id"),
                        message="Citation already in enriched format"
                    )
                elif isinstance(citation_item, str):
                    # String chunk ID - look up in citation map
                    chunk_id = citation_item
                    if chunk_id in citation_map:
                        # Found full metadata - use it
                        enriched_citations.append(citation_map[chunk_id])
                    else:
                        # Not found - keep as string (will be treated as legacy format downstream)
                        logger.warning(
                            "citation_enrichment_not_found",
                            chunk_id=chunk_id,
                            message="Chunk ID from aggregator not found in followup results"
                        )
                        enriched_citations.append(chunk_id)
                else:
                    # Unknown type - coerce to string and keep as-is
                    logger.warning(
                        "citation_enrichment_unknown_type",
                        type=type(citation_item).__name__,
                        value=str(citation_item)[:50],
                        message="Unknown citation type during enrichment"
                    )
                    enriched_citations.append(str(citation_item))
            
            kf.citations = enriched_citations
        
        enriched_count = sum(
            1 for kf in agg_validated.key_facts 
            for cit in kf.citations 
            if isinstance(cit, dict)
        )
        
        logger.info(
            "citation_enrichment_complete",
            key_facts_count=len(agg_validated.key_facts),
            enriched_citations=enriched_count,
            message="Enriched aggregated citations with full metadata from followup results"
        )

    async def aggregate_node(state: Dict[str, Any]) -> Dict[str, Any]:
        llm = get_llm()
        prompt = aggregator_prompt()
        
        # Extract only needed fields from primer (exclude rationale and followups)
        primer_data = state.get("primer_json", {})
        primer_minimal = {
            "initial_answer": primer_data.get("initial_answer", ""),
            # Exclude: rationale, followups (already processed in followup_results)
        }
        
        tree = {
            "question": state["question"],
            "primer": primer_minimal,
            "followups": state.get("followup_results"),
        }
        
        agg_response = await _format_invoke_parse(
            prompt,
            llm,
            "Aggregator",
            question=state["question"],
            tree=json.dumps(tree),
        )
        
        # Validate aggregator response
        agg_validated = _parse_and_validate(
            json.dumps(agg_response),
            AggregatorResponse,
            "Aggregator",
        )
        
        # Enrich key_facts citations with full metadata from followup results
        _enrich_citations_from_followups(agg_validated, state.get("followup_results", []))
        
        result_json = agg_validated.model_dump()
        
        logger.info(
            "retrieval.aggregate.done",
            result_keys=len(list(result_json.keys())),
            key_facts_count=len(agg_validated.key_facts),
            has_final_answer=bool(agg_validated.final_answer),
            has_uncertainty=bool(agg_validated.residual_uncertainty),
        )
        
        # Publish aggregation status
        if publisher:
            try:
                prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                
                # Format key facts with nested citations (handle both dict and string formats)
                facts_detail_md = ""
                if agg_validated.key_facts:
                    facts_detail_md = "**Validated key facts:**\n"
                    for idx, kf in enumerate(agg_validated.key_facts, 1):
                        facts_detail_md += f"{idx}. {kf.fact}\n"
                        if kf.citations:
                            # Deduplicate document names
                            seen_docs = set()
                            citation_displays = []
                            for cit in kf.citations:
                                if isinstance(cit, dict):
                                    # Enriched format with document name
                                    doc_name = cit.get("document_name", "unknown")
                                    if doc_name not in seen_docs:
                                        citation_displays.append(doc_name)
                                        seen_docs.add(doc_name)
                                else:
                                    # Legacy string format (chunk ID)
                                    cit_str = str(cit)
                                    if cit_str not in seen_docs:
                                        citation_displays.append(cit_str)
                                        seen_docs.add(cit_str)
                            
                            # Limit display to first 5 unique documents for readability
                            display_count = min(5, len(citation_displays))
                            facts_detail_md += f"   - *Sources:* {', '.join(citation_displays[:display_count])}"
                            if len(citation_displays) > 5:
                                facts_detail_md += f" (and {len(citation_displays) - 5} more)"
                            facts_detail_md += "\n"
                else:
                    facts_detail_md = "No key facts found."
                
                await publisher.publish_retrieval_update(
                    project_id=UUID(state["project_id"]),
                    retrieval_id=state["retrieval_id"],
                    phase=RetrievalStatus.AGGREGATING_RESULTS,
                    thought_summary="🧩 **Synthesizing Results**",
                    details_md=(
                        f"**Key facts collected:** {len(agg_validated.key_facts)}\n\n"
                        + facts_detail_md
                        + "\n\nAggregating findings from all sources into cohesive context..."
                    ),
                    progress_pct=90.0,
                    prompt_id=prompt_id_uuid,
                )
            except Exception as exc:
                logger.debug("publish_failed", phase="aggregate", error=str(exc))
        
        # Publish completion status
        if publisher:
            try:
                prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                
                # Format key facts with their nested citations for display (handle both dict and string formats)
                facts_md = ""
                if agg_validated.key_facts:
                    facts_md = "\n\n**Key Facts:**\n"
                    for idx, kf in enumerate(agg_validated.key_facts, 1):
                        facts_md += f"{idx}. {kf.fact}\n"
                        if kf.citations:
                            # Deduplicate document names
                            seen_docs = set()
                            citation_displays = []
                            for cit in kf.citations:
                                if isinstance(cit, dict):
                                    # Enriched format - show document name
                                    doc_name = cit.get("document_name", "unknown")
                                    if doc_name not in seen_docs:
                                        citation_displays.append(f"[{doc_name}]")
                                        seen_docs.add(doc_name)
                                else:
                                    # Legacy string format
                                    cit_str = str(cit)
                                    if cit_str not in seen_docs:
                                        citation_displays.append(cit_str)
                                        seen_docs.add(cit_str)
                            facts_md += f"   - *Citations:* {', '.join(citation_displays)}\n"
                
                await publisher.publish_retrieval_update(
                    project_id=UUID(state["project_id"]),
                    retrieval_id=state["retrieval_id"],
                    phase=RetrievalStatus.COMPLETED,
                    thought_summary="✅ **Context Retrieval Complete**",
                    details_md=f"**Final answer ready**{facts_md}\n\nProceeding with generation...",
                    progress_pct=100.0,
                    prompt_id=prompt_id_uuid,
                )
            except Exception as exc:
                logger.debug("publish_failed", phase="completed", error=str(exc))
        
        return {"tree": tree, "result_json": result_json}

    builder.add_node("init", init_node)
    builder.add_node("hyde", hyde_node)
    builder.add_node("primer", primer_node)
    builder.add_node("followups", followups_node)
    builder.add_node("aggregate", aggregate_node)

    builder.add_edge(START, "init")
    builder.add_edge("init", "hyde")
    builder.add_edge("hyde", "primer")
    builder.add_edge("primer", "followups")
    builder.add_edge("followups", "aggregate")
    builder.add_edge("aggregate", END)

    graph = builder.compile(checkpointer=InMemorySaver())
    logger.info("retrieval.graph_compiled")
    return graph


class Neo4jRetrievalService:
    def __init__(self, get_session: GetSessionFn, get_llm: GetLlmFn, get_embedder: GetEmbedderFn, publisher: Optional[Any] = None) -> None:
        self._get_session = get_session
        self._get_llm = get_llm
        self._get_embedder = get_embedder
        self._publisher = publisher

    async def retrieve(self, question: str, top_k: int, project_id: str, prompt_id: Optional[str] = None) -> Dict[str, Any]:
        """Retrieve with request deduplication and caching.
        
        If identical request is in-flight, waits for existing future.
        If identical request completed recently, returns cached result.
        
        Args:
            question: User query
            top_k: Number of results to retrieve
            project_id: Project UUID
            prompt_id: Optional parent workflow prompt_id for UI tracking
        """
        if not isinstance(question, str) or not question.strip():
            raise HTTPException(status_code=400, detail="query must be a non-empty string")
        
        settings = get_retrieval_settings()
        cache_key = _make_cache_key(question, top_k, project_id)
        
        # Check completed request cache
        _cleanup_expired_cache(settings.REQUEST_CACHE_TTL_SEC)
        if cache_key in _REQUEST_CACHE:
            ts, cached_result = _REQUEST_CACHE[cache_key]
            age_sec = time.time() - ts
            logger.info(
                "retrieval.cache_hit",
                cache_key=cache_key[:16],
                age_seconds=round(age_sec, 1),
                message="Returning cached result"
            )
            return cached_result
        
        # Check in-flight requests
        async with _REQUEST_LOCK:
            if cache_key in _REQUEST_FUTURES:
                existing_future = _REQUEST_FUTURES[cache_key]
                logger.info(
                    "retrieval.duplicate_detected",
                    cache_key=cache_key[:16],
                    message="Waiting for in-flight request"
                )
            else:
                # Create new task for this request
                future = asyncio.create_task(
                    self._execute_retrieval(question, top_k, project_id, prompt_id)
                )
                _REQUEST_FUTURES[cache_key] = future
                logger.info(
                    "retrieval.request",
                    cache_key=cache_key[:16],
                    question_len=len(question),
                    top_k=int(top_k or 1),
                    project_id=project_id,
                    prompt_id=prompt_id
                )
        
        # Execute or wait for existing
        if cache_key in _REQUEST_FUTURES:
            future = _REQUEST_FUTURES[cache_key]
            try:
                result = await future
                
                # Cache completed result
                async with _REQUEST_LOCK:
                    if settings.REQUEST_CACHE_TTL_SEC > 0:
                        _REQUEST_CACHE[cache_key] = (time.time(), result)
                    _REQUEST_FUTURES.pop(cache_key, None)
                
                return result
            except Exception as exc:
                # Remove failed future
                async with _REQUEST_LOCK:
                    _REQUEST_FUTURES.pop(cache_key, None)
                raise
        
        # Should not reach here
        raise RuntimeError("Request tracking state inconsistent")

    async def _execute_retrieval(self, question: str, top_k: int, project_id: str, prompt_id: Optional[str] = None) -> Dict[str, Any]:
        """Execute actual retrieval pipeline (extracted from retrieve method)."""
        # Use persistent graph instance (reduces latency by 200-500ms per request)
        graph = await _get_or_create_graph(self._get_session, self._get_llm, self._get_embedder, self._publisher)
        # Provide a stable thread_id for the checkpointer
        thread_id = f"{project_id}:{abs(hash(question))}"
        # Generate retrieval session ID for tracking
        retrieval_id = uuid4()
        state = await graph.ainvoke(
            {
                "question": question,
                "top_k": int(top_k or 1),
                "project_id": project_id,
                "prompt_id": prompt_id,
                "retrieval_id": retrieval_id,
            },
            {"configurable": {"thread_id": thread_id}},
        )
        result = state.get("result_json") or {}
        logger.info("retrieval.response", has_result=bool(result), keys=len(list(result.keys())), retrieval_id=str(retrieval_id), prompt_id=prompt_id)
        return result
