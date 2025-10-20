from typing import Any, Callable, Dict, List, TypedDict
import json
import asyncio
import random
from contextlib import contextmanager
import structlog
from pydantic import BaseModel

from langgraph.graph import StateGraph, START, END
from langgraph.checkpoint.memory import InMemorySaver
from langchain_openai import AzureChatOpenAI, AzureOpenAIEmbeddings
from langchain_core.messages.utils import count_tokens_approximately
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


GetSessionFn = Callable[[], Any]
GetLlmFn = Callable[[], AzureChatOpenAI]
GetEmbedderFn = Callable[[], AzureOpenAIEmbeddings]

logger = structlog.get_logger(__name__)


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
            error_str = str(exc)
            is_rate_limit = "429" in error_str or "rate limit" in error_str.lower()
            is_last_attempt = attempt >= max_attempts - 1
            
            if not is_rate_limit or is_last_attempt:
                # Not a rate limit error or exhausted retries - propagate exception
                logger.error(
                    "llm_call_failed",
                    label=label,
                    attempt=attempt + 1,
                    max_attempts=max_attempts,
                    error=error_str,
                    error_type=type(exc).__name__,
                    is_rate_limit=is_rate_limit,
                )
                raise
            
            # Calculate backoff with exponential + jitter
            exponent = attempt
            backoff = backoff_base * (backoff_factor ** exponent)
            backoff = min(backoff, backoff_max)
            # Add jitter: ±25% random variation
            jitter = backoff * 0.25 * (2 * random.random() - 1)
            sleep_time = max(0.1, backoff + jitter)
            
            logger.warning(
                "llm_rate_limit_retry",
                label=label,
                attempt=attempt + 1,
                max_attempts=max_attempts,
                sleep_seconds=round(sleep_time, 2),
                error=error_str,
                message=f"Rate limit hit, retrying after {sleep_time:.1f}s"
            )
            
            await asyncio.sleep(sleep_time)
    
    # Should not reach here due to raise in loop, but for type safety
    raise RuntimeError(f"Max retries exhausted for {label}")


class _State(TypedDict, total=False):
    question: str
    top_k: int
    project_id: str
    qvec: List[float]
    communities: List[int]
    community_brief: List[Dict[str, Any]]
    primer_json: Dict[str, Any]
    followups: List[Dict[str, Any]]
    followup_results: List[Dict[str, Any]]
    tree: Dict[str, Any]
    result_json: Dict[str, Any]


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
    
    try:
        actual_tokens = count_tokens_approximately(serialized)
    except Exception:
        # Fallback to heuristic if count fails
        actual_tokens = len(serialized) // 4
    
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


async def _create_graph(get_session: GetSessionFn, get_llm: GetLlmFn, get_embedder: GetEmbedderFn):
    settings = get_retrieval_settings()

    builder = StateGraph(_State)


    @contextmanager
    def _repo_ctx():
        with get_session() as session:
            yield Neo4jRepository(session)

    def _ensure_top_k(state: Dict[str, Any]) -> int:
        try:
            return max(5, int(state.get("top_k") or 1))
        except Exception:  # noqa: BLE001
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
        rows = repo.vector_query_nodes(index_name, k, qvec, project_id)
        communities: List[int] = []
        for r in rows:
            node = r.get("node")
            if node is not None:
                communities.append(int(node["community"]))
        return communities

    def _sample_chunks(
        repo: Neo4jRepository,
        communities: List[int],
        chunk_index: str,
        qvec: List[float],
        project_id: str,
    ) -> Dict[int, List[int]]:
        if not (communities and chunk_index):
            return {}
        return repo.sample_chunks_for_communities(communities, chunk_index, qvec, project_id)

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
    ) -> List[Dict[str, Any]]:
        if not (chunk_index and communities):
            logger.warning("retrieval.scoped_chunks_empty", reason="missing_chunk_index_or_communities", chunk_index=bool(chunk_index), communities_count=len(communities or []))
            return []
        rows = repo.scoped_chunk_ids(communities, chunk_index, query_vec, k, project_id)
        chunks = rows
        logger.info("retrieval.scoped_chunks", chunk_ids_found=len(chunks), communities_count=len(communities or []))
        if not chunks:
            logger.warning("retrieval.scoped_chunks_empty", reason="no_chunks_found_for_communities", communities=communities[:5])
            return []
        return repo.expand_neighborhood_minimal(chunks, project_id)

    def _create_minimal_followup_result(local_validated: LocalExecutorResponse, qtext: str) -> Dict[str, Any]:
        """Create minimal followup result for aggregation - strips unnecessary context.
        
        Aggregator only needs: question, answer, citations, confidence, new_followups.
        Full chunk contexts (text, entities, relationships) are NOT needed and waste tokens.
        """
        minimal_result = {
            "question": qtext,
            "answer": local_validated.answer,
            "citations": [{"chunk_id": c.chunk_id, "span": c.span} for c in local_validated.citations],
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

    async def init_node(state: Dict[str, Any]) -> Dict[str, Any]:
        logger.info("retrieval.init", question_len=len(state.get("question") or ""))
        return {}

    async def hyde_node(state: Dict[str, Any]) -> Dict[str, Any]:
        llm = get_llm()
        prompt = hyde_prompt()
        hyde_answer = await _format_invoke_content(prompt, llm, "HyDE", question=state["question"])
        hyde_text = build_hyde_embed_text(state["question"], hyde_answer)
        qvec = await _embed_one_or_502(hyde_text)
        logger.info("retrieval.hyde.done", hyde_text_len=len(hyde_text or ""), qvec_len=len(qvec or []))
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
            chunk_index = settings.vector_index.CHUNK_VECTOR_INDEX_NAME
            sampled = _sample_chunks(
                repo,
                communities,
                chunk_index,
                state["qvec"],
                state["project_id"],
            )
            community_brief = _fetch_community_brief(repo, communities, state["project_id"])

        sample_chunks = [{"community_id": cid, "chunk_ids": ids} for cid, ids in sampled.items()]
        llm = get_llm()
        prompt = primer_prompt()
        primer_response = await _format_invoke_parse(
            prompt,
            llm,
            "Primer",
            question=state["question"],
            community_details=json.dumps(community_brief),
            sample_chunks=json.dumps(sample_chunks),
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
            sampled_total=sum(len(v or []) for v in sampled.values()),
            community_brief=len(community_brief or []),
            followups=len(followups or []),
        )
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
                        
                        qvec = await _embed_one_or_502(qtext)

                        chunks = _scoped_chunks_expanded(
                            repo,
                            cids,
                            chunk_index,
                            qvec,
                            k,
                            state["project_id"],
                        )

                        target_communities_brief = (
                            _fetch_community_brief(repo, cids, state["project_id"]) if cids else []
                        )

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

                        prompt = local_executor_prompt()
                        local_response = await _format_invoke_parse(
                            prompt,
                            llm,
                            "Local executor",
                            qtext=qtext,
                            target_communities=target_communities_json,
                            chunks_preview=chunks_preview_json,
                        )

                        # CRITICAL FIX: Validate and normalize response
                        local_validated = _parse_and_validate(
                            json.dumps(local_response),
                            LocalExecutorResponse,
                            f"Local executor (followup {idx})",
                        )

                        # Create minimal result for aggregation - strips unnecessary context
                        # Aggregator only needs: question, answer, citations, confidence, new_followups
                        minimal_result = _create_minimal_followup_result(local_validated, qtext)
                        
                        logger.debug(
                            "followup_result_created",
                            followup_index=idx,
                            has_answer=bool(minimal_result.get("answer")),
                            citations_count=len(minimal_result.get("citations", [])),
                            confidence=minimal_result.get("confidence"),
                            new_followups_count=len(minimal_result.get("new_followups", [])),
                        )

                        results.append(minimal_result)
                        
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
                
                logger.info(
                    "retrieval.followups.done",
                    processed=len(results),
                    validated=len(valid_results),
                    total_followups=total_followups,
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

    async def aggregate_node(state: Dict[str, Any]) -> Dict[str, Any]:
        llm = get_llm()
        prompt = aggregator_prompt()
        tree = {
            "question": state["question"],
            "primer": state.get("primer_json"),
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
        
        result_json = agg_validated.model_dump()
        
        logger.info(
            "retrieval.aggregate.done",
            result_keys=len(list(result_json.keys())),
            key_facts_count=len(agg_validated.key_facts),
            has_final_answer=bool(agg_validated.final_answer),
            has_uncertainty=bool(agg_validated.residual_uncertainty),
        )
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
    def __init__(self, get_session: GetSessionFn, get_llm: GetLlmFn, get_embedder: GetEmbedderFn) -> None:
        self._get_session = get_session
        self._get_llm = get_llm
        self._get_embedder = get_embedder

    async def retrieve(self, question: str, top_k: int, project_id: str) -> Dict[str, Any]:
        if not isinstance(question, str) or not question.strip():
            raise HTTPException(status_code=400, detail="query must be a non-empty string")
        logger.info(
                "retrieval.request",
                question_len=len(question),
                top_k=int(top_k or 1),
                project_id=str(project_id),
            )
        graph = await _create_graph(self._get_session, self._get_llm, self._get_embedder)
        # Provide a stable thread_id for the checkpointer
        thread_id = f"{project_id}:{abs(hash(question))}"
        state = await graph.ainvoke(
            {
                "question": question,
                "top_k": int(top_k or 1),
                "project_id": project_id,
            },
            {"configurable": {"thread_id": thread_id}},
        )
        result = state.get("result_json") or {}
        logger.info("retrieval.response", has_result=bool(result), keys=len(list(result.keys())))
        return result
