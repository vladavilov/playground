"""Followups node for parallel execution of followup questions.

Implements parallel followup processing:
1. For each followup question, retrieve relevant chunks
2. Generate answers with citations
3. Execute all followups in parallel for performance
4. Validate and filter results
"""

from typing import Any, Dict, List, Optional
from uuid import UUID
from contextlib import contextmanager
import asyncio
import structlog

from retrieval_ms.nodes.base_node import BaseNode
from retrieval_ms.prompts import local_executor_prompt
from retrieval_ms.repositories.neo4j_repository import Neo4jRepository
from retrieval_ms.response_models import LocalExecutorResponse
from models.progress_messages import RetrievalStatus
from utils.json_utils import parse_and_validate
from utils.embedding_service import EmbeddingService
from utils.chunk_utils import (
    calculate_overlap_ratio,
    compress_chunks_adaptive,
    truncate_for_prompt,
    reorder_chunks_by_novelty,
)
from utils.citation_utils import validate_citations
from config import get_retrieval_settings

logger = structlog.get_logger(__name__)

# Constants for adaptive processing and nested followups
OVERLAP_RATIO_THRESHOLD = 0.5  # Threshold for triggering adaptive compression
MAX_NESTED_FOLLOWUPS = 2  # Maximum nested followups per execution run
NESTED_INDEX_MULTIPLIER = 1000  # Multiplier for nested followup index calculation
SPAN_TRUNCATION_LENGTH = 150  # Maximum length for citation span extraction
MINIMUM_SPAN_LENGTH = 5  # Minimum span length for valid citations


class FollowupsNode(BaseNode):
    """Execute followup questions in parallel for maximum throughput."""
    
    @staticmethod
    def _filter_none_results(results: List[Optional[Dict[str, Any]]]) -> List[Dict[str, Any]]:
        """Filter out None results from parallel execution.
        
        Args:
            results: List of results that may contain None values
            
        Returns:
            List with None values filtered out
        """
        return [r for r in results if r is not None]
    
    @staticmethod
    def _is_valid_followup_result(result: Dict[str, Any]) -> bool:
        """Check if a followup result is valid.
        
        A result is valid if it has:
        - Non-empty answer OR
        - At least one valid citation
        
        Args:
            result: Followup result dict
            
        Returns:
            True if result is valid, False otherwise
        """
        has_answer = bool(result.get("answer", "").strip())
        has_citations = bool(result.get("citations") and len(result.get("citations")) > 0)
        return has_answer or has_citations
    
    @contextmanager
    def _repo_ctx(self):
        """Context manager for Neo4j repository."""
        with self._get_session() as session:
            yield Neo4jRepository(session)
    
    def _get_scoped_chunks_expanded(
        self,
        repo: Neo4jRepository,
        communities: List[int],
        chunk_index: str,
        query_vec: List[float],
        k: int,
        project_id: str,
        state: Dict[str, Any],
    ) -> List[Dict[str, Any]]:
        """Get expanded chunks with request-level caching."""
        settings = get_retrieval_settings()
        
        if not (chunk_index and communities):
            logger.warning(
                "retrieval.scoped_chunks_empty",
                reason="missing_chunk_index_or_communities",
                chunk_index=bool(chunk_index),
                communities_count=len(communities or [])
            )
            return []
        
        # Use optimized vector query
        rows = repo.optimized_scoped_chunks(communities, chunk_index, query_vec, k, project_id)
        chunks = rows
        
        logger.info("retrieval.scoped_chunks", chunk_ids_found=len(chunks), communities_count=len(communities))
        
        if not chunks:
            logger.warning("retrieval.scoped_chunks_empty", reason="no_chunks_found_for_communities", communities=communities[:5])
            return []
        
        # Reorder chunks to prioritize unseen content
        seen_chunk_ids = state.get("_seen_chunk_ids", set())
        chunks = reorder_chunks_by_novelty(chunks, seen_chunk_ids)
        
        # Check cache for already-expanded chunks
        cache = state.get("_cache_neighborhoods", {})
        
        # Separate cached and uncached chunks
        cached_results = [cache[chunk_id] for chunk_id in chunks if chunk_id in cache]
        chunks_to_expand = [chunk_id for chunk_id in chunks if chunk_id not in cache]
        
        if cached_results:
            logger.debug("neighborhood_cache_hit", count=len(cached_results))
        
        # Expand only uncached chunks
        if chunks_to_expand:
            logger.debug("neighborhood_cache_miss", count=len(chunks_to_expand))
            expanded = repo.expand_neighborhood_minimal(
                chunks_to_expand,
                project_id,
                max_chunk_text_len=settings.MAX_CHUNK_TEXT_LENGTH
            )
            
            # Cache new results
            for item in expanded:
                if cid := item.get("chunk_id"):
                    cache[cid] = item
            
            state["_cache_neighborhoods"] = cache
            return cached_results + expanded
        
        return cached_results
    
    async def _spawn_and_execute_nested_followups(
        self,
        idx: int,
        local_validated: LocalExecutorResponse,
        state: Dict[str, Any],
        k: int,
        cids: List[int],
        chunk_index: str,
        depth: int,
        nested_counter: asyncio.Lock,
        nested_count_dict: Dict[str, int],
    ) -> List[Dict[str, Any]]:
        """Spawn and execute nested followups if available.
        
        Args:
            idx: Parent followup index
            local_validated: LLM response with new_followups
            state: Graph state
            k: Top-k chunks to retrieve
            cids: Community IDs
            chunk_index: Vector index name
            depth: Current nesting depth
            nested_counter: Lock for thread-safe nested followup counting
            nested_count_dict: Shared dict to track total nested followups spawned
            
        Returns:
            List of nested followup results
        """
        nested_results = []
        
        if not (local_validated.new_followups and depth == 0 and nested_counter and nested_count_dict):
            return nested_results
        
        # Check if we can spawn nested followups
        current_count = nested_count_dict.get("count", 0)
        if current_count >= MAX_NESTED_FOLLOWUPS:
            return nested_results
        
        # Determine how many nested followups we can spawn
        available_slots = MAX_NESTED_FOLLOWUPS - current_count
        nested_followups_to_spawn = local_validated.new_followups[:available_slots]
        
        if not nested_followups_to_spawn:
            return nested_results
        
        # Atomically reserve slots for nested followups
        nested_followups_to_spawn_final = []
        async with nested_counter:
            # Double-check limit after acquiring lock
            current_count = nested_count_dict.get("count", 0)
            if current_count < MAX_NESTED_FOLLOWUPS:
                available_slots = MAX_NESTED_FOLLOWUPS - current_count
                nested_followups_to_spawn_final = local_validated.new_followups[:available_slots]
                
                if nested_followups_to_spawn_final:
                    # Update counter atomically
                    nested_count_dict["count"] = current_count + len(nested_followups_to_spawn_final)
                    logger.info(
                        "nested_followups_spawning",
                        parent_index=idx,
                        spawning_count=len(nested_followups_to_spawn_final),
                        total_nested_count=nested_count_dict["count"],
                        message="Spawning nested followup executions"
                    )
        
        # Execute nested followups outside lock (in parallel)
        if nested_followups_to_spawn_final:
            nested_tasks = [
                self._process_single_followup(
                    idx=idx * NESTED_INDEX_MULTIPLIER + nested_idx,
                    followup={"question": nf.question},
                    state=state,
                    k=k,
                    cids=cids,
                    chunk_index=chunk_index,
                    total_followups=len(nested_followups_to_spawn_final),
                    depth=depth + 1,
                    nested_counter=nested_counter,
                    nested_count_dict=nested_count_dict,
                )
                for nested_idx, nf in enumerate(nested_followups_to_spawn_final)
            ]
            
            # Execute nested followups in parallel
            nested_results_with_none = await asyncio.gather(*nested_tasks, return_exceptions=False)
            nested_results = self._filter_none_results(nested_results_with_none)
            
            logger.info(
                "nested_followups_completed",
                parent_index=idx,
                spawned_count=len(nested_followups_to_spawn_final),
                completed_count=len(nested_results),
                message="Nested followup executions completed"
            )
        
        return nested_results
    
    async def _publish_followup_progress(
        self,
        idx: int,
        minimal_result: Dict[str, Any],
        qtext: str,
        total_followups: int,
        depth: int,
        state: Dict[str, Any],
    ) -> None:
        """Publish progress update for a followup execution.
        
        Args:
            idx: Followup index
            minimal_result: Followup result with answer and citations
            qtext: Question text
            total_followups: Total number of followups
            depth: Nesting depth (0 = initial, 1 = nested)
            state: Graph state with project_id, retrieval_id, prompt_id
        """
        if not self._publisher:
            return
        
        try:
            progress = 40.0 + ((idx + 1) / total_followups) * 40.0
            prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
            depth_label = f" (nested depth {depth})" if depth > 0 else ""
            
            await self._publisher.publish_retrieval_update(
                project_id=UUID(state["project_id"]),
                retrieval_id=state["retrieval_id"],
                phase=RetrievalStatus.EXECUTING_FOLLOWUP,
                thought_summary=f"🔎 **Follow-up {idx + 1}/{total_followups}{depth_label}**",
                details_md=(
                    f"**Follow-up {idx + 1}/{total_followups}{depth_label}:** {qtext}\n\n"
                    f"**Answer:** {minimal_result.get('answer') or 'No answer found.'}\n\n"
                    f"**Citations:**  \n" +
                    (
                        "\n".join([
                            f"- [{c.get('document_name', 'Unknown')}] \"{c.get('span', '')[:120]}...\""
                            for c in minimal_result.get('citations', []) or []
                        ])
                        if minimal_result.get('citations') else "No citations."
                    ) +
                    (
                        f"\n__Confidence:__ {minimal_result.get('confidence'):.2f}"
                        if isinstance(minimal_result.get('confidence'), (float, int)) else ""
                    )
                ),
                progress_pct=progress,
                prompt_id=prompt_id_uuid,
            )
        except Exception as exc:
            logger.debug("publish_failed", phase="followup", followup_idx=idx, error=str(exc))
    
    def _create_minimal_followup_result(
        self,
        local_validated: LocalExecutorResponse,
        qtext: str,
        chunks: List[Dict[str, Any]],
        context_label: str,
    ) -> Dict[str, Any]:
        """Create minimal followup result with validated citations.
        
        Note: validate_citations already returns enriched dict citations with
        {chunk_id, span, document_name}, so no additional enrichment is needed.
        """
        # Build lookup map: chunk_id -> document_name
        chunk_to_doc = {
            str(item.get("chunk_id")): item.get("document_name", "unknown")
            for item in chunks
            if item.get("chunk_id") is not None
        }
        valid_chunk_ids = set(chunk_to_doc.keys())
        
        # Validate citations using shared utility (returns enriched dicts)
        validated_citations = validate_citations(
            local_validated.citations,
            valid_chunk_ids,
            chunk_to_doc,
            context_label=context_label,
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
    
    async def _process_single_followup(
        self,
        idx: int,
        followup: Dict[str, Any],
        state: Dict[str, Any],
        k: int,
        cids: List[int],
        chunk_index: str,
        total_followups: int,
        depth: int = 0,
        nested_counter: Optional[asyncio.Lock] = None,
        nested_count_dict: Optional[Dict[str, int]] = None,
    ) -> Optional[Dict[str, Any]]:
        """Process a single followup with error isolation and nested followup support.
        
        Args:
            idx: Followup index
            followup: Followup question dict
            state: Graph state
            k: Top-k chunks to retrieve
            cids: Community IDs
            chunk_index: Vector index name
            total_followups: Total number of followups in current batch
            depth: Nesting depth (0 = initial, 1 = nested)
            nested_counter: Lock for thread-safe nested followup counting
            nested_count_dict: Shared dict to track total nested followups spawned
            
        Returns:
            Followup result dict with optional nested results
        """
        settings = get_retrieval_settings()
        
        try:
            qtext = str(followup.get("question", ""))
            if not qtext:
                logger.warning("followup_empty_question", followup_index=idx, message="Skipping empty question")
                return None
            
            logger.debug("processing_followup_parallel", followup_index=idx, question=qtext[:100])
            
            # Generate embedding with caching
            embedder = self._get_embedder()
            embedding_service = EmbeddingService(
                embedder,
                expected_dimensions=settings.vector_index.VECTOR_INDEX_DIMENSIONS
            )
            
            cache = state.get("_cache_embeddings", {})
            qvec = await embedding_service.embed_with_cache(qtext, cache)
            state["_cache_embeddings"] = cache
            
            # Validate dimension on first followup only
            if idx == 0:
                embedding_service.validate_dimension(qvec, f"Followup-{idx}")
            
            # Fetch and expand chunks
            with self._repo_ctx() as repo:
                chunks = self._get_scoped_chunks_expanded(
                    repo, cids, chunk_index, qvec, k, state["project_id"], state
                )
            
            if not chunks:
                logger.info("followup_no_data_found", followup_index=idx, message=f"No chunks for followup {idx}")
                return None
            
            # Adaptive compression based on chunk repetition
            seen_chunk_ids = state.get("_seen_chunk_ids", set())
            current_chunk_ids = {c.get("chunk_id") for c in chunks if c.get("chunk_id")}
            overlap_ratio = calculate_overlap_ratio(current_chunk_ids, seen_chunk_ids)
            
            # Reduce limit if high overlap
            adaptive_limit = settings.MAX_CHUNKS_FOR_PROMPT
            if overlap_ratio > OVERLAP_RATIO_THRESHOLD:
                adaptive_limit = max(10, settings.MAX_CHUNKS_FOR_PROMPT // 2)
                logger.info(
                    "adaptive_chunk_limit_reduced",
                    followup_index=idx,
                    overlap_ratio=overlap_ratio,
                    threshold=OVERLAP_RATIO_THRESHOLD,
                    original_limit=settings.MAX_CHUNKS_FOR_PROMPT,
                    adaptive_limit=adaptive_limit,
                )
            
            # Update seen chunks
            seen_chunk_ids.update(current_chunk_ids)
            state["_seen_chunk_ids"] = seen_chunk_ids
            
            # Apply adaptive compression
            chunks_for_prompt = compress_chunks_adaptive(chunks, overlap_ratio)
            
            # Prepare prompt data
            target_communities_brief = state.get("community_brief", [])
        
            chunks_for_prompt_truncated = chunks_for_prompt[:adaptive_limit]
            chunks_preview_json = truncate_for_prompt(
                chunks_for_prompt_truncated,
                max_items=adaptive_limit,
                label=f"chunks_preview_followup_{idx}",
            )
            target_communities_json = truncate_for_prompt(
                target_communities_brief,
                max_items=settings.MAX_COMMUNITIES_FOR_PROMPT,
                label=f"target_communities_followup_{idx}",
            )
            
            # Extract valid chunk IDs from the same truncated chunks used in prompt
            valid_chunk_ids_str = ", ".join([
                str(chunk.get("chunk_id"))
                for chunk in chunks_for_prompt_truncated
                if chunk.get("chunk_id") is not None
            ])
            
            # Execute LLM call (direct, no retry)
            llm = self._get_llm()
            prompt = local_executor_prompt()
            msg = prompt.format_messages(
                qtext=qtext,
                target_communities=target_communities_json,
                chunks_preview=chunks_preview_json,
                valid_chunk_ids=valid_chunk_ids_str,
            )
            
            local_response = await llm.ainvoke(msg)
            
            # Validate response
            local_validated = parse_and_validate(
                local_response,
                LocalExecutorResponse,
                f"Local executor (followup {idx})",
            )
            
            # Create minimal result with validated citations
            minimal_result = self._create_minimal_followup_result(
                local_validated,
                qtext,
                chunks,
                context_label=f"followup_{idx}",
            )
            
            logger.debug(
                "followup_result_created",
                followup_index=idx,
                has_answer=bool(minimal_result.get("answer")),
                citations_count=len(minimal_result.get("citations", [])),
                confidence=minimal_result.get("confidence"),
            )
            
            # Quality check (for observability, still return result)
            if not self._is_valid_followup_result(minimal_result):
                is_high_confidence = minimal_result.get("confidence", 0) >= 0.7
                logger.warning(
                    "followup_low_quality",
                    followup_index=idx,
                    has_answer=bool(minimal_result.get("answer", "").strip()),
                    has_citations=bool(minimal_result.get("citations")),
                    confidence=minimal_result.get("confidence", 0),
                    is_high_confidence=is_high_confidence,
                )
            
            # Publish progress
            await self._publish_followup_progress(
                idx, minimal_result, qtext, total_followups, depth, state
            )
            
            # Handle nested followups if new_followups are found and limit not reached
            nested_results = await self._spawn_and_execute_nested_followups(
                idx, local_validated, state, k, cids, chunk_index, depth, nested_counter, nested_count_dict
            )
            
            # Attach nested results to parent result
            if nested_results:
                minimal_result["nested_followups"] = nested_results
            
            return minimal_result
            
        except Exception as exc:
            logger.error(
                "followup_processing_failed",
                followup_index=idx,
                error=str(exc),
                error_type=type(exc).__name__,
                followup_question=followup.get("question", "N/A") if isinstance(followup, dict) else str(followup)[:100],
            )
            return None
    
    async def execute(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """Execute all followups in parallel with nested followup support.
        
        Args:
            state: Graph state with followups, communities, project_id
            
        Returns:
            State updates with followup_results (including nested results) and optional no_data_found flag
        """
        settings = get_retrieval_settings()
        k = self._ensure_top_k(state)
        cids = list(state.get("communities") or [])
        chunk_index = settings.vector_index.CHUNK_VECTOR_INDEX_NAME
        
        followups = list(state.get("followups") or [])
        total_followups = len(followups)
        
        if not followups:
            logger.warning("followups_node_no_followups", message="No followups to process")
            return {"followup_results": []}
        
        try:
            # Initialize nested followup tracking
            nested_counter = asyncio.Lock()
            nested_count_dict: Dict[str, int] = {"count": 0}
            
            # Execute all followups in parallel
            logger.info(
                "followups_parallel_execution_start",
                total_followups=total_followups,
                max_nested_followups=MAX_NESTED_FOLLOWUPS,
                message=f"Executing {total_followups} followups in parallel with nested followup support (max {MAX_NESTED_FOLLOWUPS})"
            )
            
            tasks = [
                self._process_single_followup(
                    idx, f, state, k, cids, chunk_index, total_followups,
                    depth=0,
                    nested_counter=nested_counter,
                    nested_count_dict=nested_count_dict,
                )
                for idx, f in enumerate(followups)
            ]
            results_with_none = await asyncio.gather(*tasks, return_exceptions=False)
            
            # Filter out None results
            results = self._filter_none_results(results_with_none)
            
            # Flatten nested results into main results list
            flattened_results = []
            for result in results:
                flattened_results.append(result)
                # Add nested followups as separate entries if they exist
                nested_followups = result.get("nested_followups", [])
                if nested_followups:
                    flattened_results.extend(nested_followups)
                    logger.debug(
                        "nested_followups_flattened",
                        parent_question=result.get("question", "")[:50],
                        nested_count=len(nested_followups),
                        message="Flattened nested followups for external script compatibility"
                    )
            
            # Validate results
            valid_results = [r for r in flattened_results if self._is_valid_followup_result(r)]
            
            total_nested_spawned = nested_count_dict.get("count", 0)
            logger.info(
                "retrieval.followups.done",
                processed=len(results),
                flattened_total=len(flattened_results),
                validated=len(valid_results),
                total_followups=total_followups,
                nested_followups_spawned=total_nested_spawned,
                success_rate=f"{len(valid_results)}/{total_followups}",
                message="Followup execution completed with nested followups (flattened)"
            )
            
            # Check if no data was found
            if len(flattened_results) == 0:
                logger.warning("followups_all_empty", message="No data found in any followups")
                return {"followup_results": [], "no_data_found": True}
            
            return {"followup_results": flattened_results}
            
        except Exception as exc:
            logger.error(
                "followups_node_critical_failure",
                error=str(exc),
                error_type=type(exc).__name__,
                total_followups=total_followups,
            )
            return {"followup_results": []}

