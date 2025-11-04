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
import json
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


class FollowupsNode(BaseNode):
    """Execute followup questions in parallel for maximum throughput."""
    
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
    
    def _create_minimal_followup_result(
        self,
        local_validated: LocalExecutorResponse,
        qtext: str,
        chunks: List[Dict[str, Any]],
        context_label: str,
    ) -> Dict[str, Any]:
        """Create minimal followup result with validated citations."""
        # Build lookup map: chunk_id -> document_name
        chunk_to_doc = {item.get("chunk_id"): item.get("document_name", "unknown") for item in chunks}
        valid_chunk_ids = set(chunk_to_doc.keys())
        
        # Validate citations using shared utility
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
    ) -> Optional[Dict[str, Any]]:
        """Process a single followup with error isolation."""
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
            if overlap_ratio > 0.5:
                adaptive_limit = max(10, settings.MAX_CHUNKS_FOR_PROMPT // 2)
                logger.info(
                    "adaptive_chunk_limit_reduced",
                    followup_index=idx,
                    overlap_ratio=overlap_ratio,
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
            chunks_preview_json = truncate_for_prompt(
                chunks_for_prompt,
                max_items=adaptive_limit,
                label=f"chunks_preview_followup_{idx}",
            )
            target_communities_json = truncate_for_prompt(
                target_communities_brief,
                max_items=settings.MAX_COMMUNITIES_FOR_PROMPT,
                label=f"target_communities_followup_{idx}",
            )
            
            # Extract valid chunk IDs from truncated chunks (after compression/truncation)
            # This ensures valid_chunk_ids matches what's in chunks_preview_json
            chunks_for_prompt_truncated = chunks_for_prompt[:adaptive_limit]
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
            has_valid_citation = len(minimal_result.get("citations", [])) > 0
            is_high_confidence = minimal_result.get("confidence", 0) >= 0.7
            has_answer = bool(minimal_result.get("answer", "").strip())
            
            if not has_answer or not (has_valid_citation or is_high_confidence):
                logger.warning(
                    "followup_low_quality",
                    followup_index=idx,
                    has_answer=has_answer,
                    has_citations=has_valid_citation,
                    confidence=minimal_result.get("confidence", 0),
                )
            
            # Publish progress
            if self._publisher:
                try:
                    progress = 40.0 + ((idx + 1) / total_followups) * 40.0
                    prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                    await self._publisher.publish_retrieval_update(
                        project_id=UUID(state["project_id"]),
                        retrieval_id=state["retrieval_id"],
                        phase=RetrievalStatus.EXECUTING_FOLLOWUP,
                        thought_summary=f"🔎 **Follow-up {idx + 1}/{total_followups}**",
                        details_md=(
                            f"**Follow-up {idx + 1}/{total_followups}:** {qtext}\n\n"
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
        """Execute all followups in parallel.
        
        Args:
            state: Graph state with followups, communities, project_id
            
        Returns:
            State updates with followup_results and optional no_data_found flag
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
            # Execute all followups in parallel
            logger.info(
                "followups_parallel_execution_start",
                total_followups=total_followups,
                message=f"Executing {total_followups} followups in parallel"
            )
            
            tasks = [
                self._process_single_followup(idx, f, state, k, cids, chunk_index, total_followups)
                for idx, f in enumerate(followups)
            ]
            results_with_none = await asyncio.gather(*tasks, return_exceptions=False)
            
            # Filter out None results
            results = [r for r in results_with_none if r is not None]
            
            # Calculate validation success
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
            )
            
            # Check if no data was found
            if len(results) == 0:
                logger.warning("followups_all_empty", message="No data found in any followups")
                return {"followup_results": [], "no_data_found": True}
            
            return {"followup_results": results}
            
        except Exception as exc:
            logger.error(
                "followups_node_critical_failure",
                error=str(exc),
                error_type=type(exc).__name__,
                total_followups=total_followups,
            )
            return {"followup_results": []}

