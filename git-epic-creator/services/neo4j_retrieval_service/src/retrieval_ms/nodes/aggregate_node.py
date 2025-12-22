"""Aggregate node for synthesizing final answer from followup results.

Implements result aggregation:
1. Check for no-data condition
2. Build reasoning tree from followup results
3. Generate final answer with key facts
4. Enrich citations with full metadata
5. Deduplicate top-level citations
"""

from typing import Any, Dict, List
from uuid import UUID
import json
import structlog

from retrieval_ms.nodes.base_node import BaseNode
from retrieval_ms.prompts import aggregator_prompt
from retrieval_ms.response_models import AggregatorResponse
from models.progress_messages import RetrievalStatus
from utils.json_utils import parse_and_validate
from utils.citation_utils import (
    enrich_citations,
    deduplicate_citations,
    filter_valid_citations,
    format_citations_for_display,
)

logger = structlog.get_logger(__name__)


class AggregateNode(BaseNode):
    """Synthesize final answer and key facts from all followup results."""
    
    def _build_citation_map(self, followup_results: List[Dict[str, Any]]) -> Dict[str, Dict[str, Any]]:
        """Build lookup map from chunk_id to full citation object."""
        citation_map: Dict[str, Dict[str, Any]] = {}
        
        for fr in followup_results:
            for cit in fr.get("citations", []) or []:
                if isinstance(cit, dict) and "chunk_id" in cit:
                    chunk_id = str(cit.get("chunk_id", ""))
                    if chunk_id and chunk_id not in citation_map:
                        citation_map[chunk_id] = cit
        
        logger.debug(
            "citation_map_built",
            unique_citations=len(citation_map),
            total_followup_results=len(followup_results)
        )
        
        return citation_map
    
    def _extract_valid_citations_for_prompt(self, followup_results: List[Dict[str, Any]]) -> str:
        """Extract valid citations from followup results and format with chunk_id.
        
        Returns a formatted string listing all valid citations with chunk_id for the aggregator prompt.
        Format: [document_name] "citation_text" (chunk_id: <id>)
        """
        valid_citations = []
        seen_citations = set()  # Deduplicate by (document_name, span_prefix)
        
        for fr in followup_results:
            for cit in fr.get("citations", []) or []:
                if isinstance(cit, dict):
                    chunk_id = cit.get("chunk_id")
                    document_name = cit.get("document_name", "unknown")
                    span = cit.get("span", "")
                    
                    # Skip if missing required fields (chunk_id is critical)
                    if not chunk_id or not document_name or not span or document_name == "unknown":
                        continue
                    
                    # Deduplicate by document_name and first 100 chars of span
                    span_prefix = span[:100].strip()
                    citation_key = (document_name, span_prefix)
                    
                    if citation_key not in seen_citations:
                        seen_citations.add(citation_key)
                        # Format as [document_name] "citation_text" (chunk_id: <id>)
                        # Truncate span to 200 chars for prompt (full span in tree)
                        span_truncated = span[:200] + "..." if len(span) > 200 else span
                        formatted_citation = f'[{document_name}] "{span_truncated}" (chunk_id: {chunk_id})'
                        valid_citations.append(formatted_citation)
        
        if not valid_citations:
            return "No valid citations found."
        
        # Limit to 50 citations to prevent prompt overflow
        limited_citations = valid_citations[:50]
        result = "\n".join(f"  - {cit}" for cit in limited_citations)
        
        if len(valid_citations) > 50:
            result += f"\n  ... and {len(valid_citations) - 50} more citations"
        
        logger.debug(
            "valid_citations_extracted",
            total_citations=len(valid_citations),
            included_in_prompt=len(limited_citations),
        )
        
        return result
    
    def _enrich_key_facts_citations(
        self,
        agg_validated: AggregatorResponse,
        followup_results: List[Dict[str, Any]],
    ) -> None:
        """Enrich aggregated key_facts citations with full metadata.
        
        Processes citations from aggregator LLM (chunk_id strings) and enriches them
        with full metadata (chunk_id, span, document_name) from followup results.
        
        Mutates agg_validated.key_facts in place.
        """
        citation_map = self._build_citation_map(followup_results)
        
        # Track enrichment statistics
        total_citations = 0
        string_citations = 0  # chunk_id strings from LLM
        dict_citations = 0  # already enriched
        
        # Enrich each key_fact's citations
        for kf_idx, kf in enumerate(agg_validated.key_facts):
            original_citations = kf.citations
            total_citations += len(original_citations)
            
            # Count input types
            for cit in original_citations:
                if isinstance(cit, str):
                    string_citations += 1
                elif isinstance(cit, dict):
                    dict_citations += 1
            
            kf.citations = enrich_citations(
                original_citations,
                citation_map,
                context_label=f"aggregation_keyfact_{kf_idx}"
            )
        
        # Post-enrichment filtering and validation
        filtered_count = 0
        invalid_citations = 0
        
        for kf_idx, kf in enumerate(agg_validated.key_facts):
            original_count = len(kf.citations)
            kf.citations = filter_valid_citations(kf.citations)
            filtered_count += original_count - len(kf.citations)
            
            # Validate enriched citations have required fields
            for cit in kf.citations:
                if isinstance(cit, dict):
                    # Ensure citation has all required fields
                    if not cit.get("chunk_id"):
                        invalid_citations += 1
                        logger.warning(
                            "citation_missing_chunk_id",
                            key_fact_index=kf_idx,
                            citation=cit,
                            message="Enriched citation missing chunk_id"
                        )
                    elif not cit.get("document_name"):
                        invalid_citations += 1
                        logger.warning(
                            "citation_missing_document_name",
                            key_fact_index=kf_idx,
                            chunk_id=cit.get("chunk_id"),
                            message="Enriched citation missing document_name"
                        )
                elif isinstance(cit, str):
                    # String citation that wasn't enriched - this is an error
                    invalid_citations += 1
                    logger.warning(
                        "citation_enrichment_failed",
                        key_fact_index=kf_idx,
                        chunk_id=cit,
                        message="Citation remained as string after enrichment (enrichment failed)"
                    )
        
        # Calculate enrichment success rate
        enriched_count = sum(
            1 for kf in agg_validated.key_facts
            for cit in kf.citations
            if isinstance(cit, dict) and cit.get("chunk_id") and cit.get("document_name")
        )
        
        enrichment_success_rate = (
            (enriched_count / total_citations * 100) if total_citations > 0 else 0.0
        )
        
        # Log detailed statistics
        logger.info(
            "citation_enrichment_complete",
            key_facts_count=len(agg_validated.key_facts),
            total_citations=total_citations,
            string_citations_input=string_citations,
            dict_citations_input=dict_citations,
            enriched_citations=enriched_count,
            enrichment_success_rate=f"{enrichment_success_rate:.1f}%",
            filtered_count=filtered_count,
            invalid_citations=invalid_citations,
            message=f"Enriched {enriched_count}/{total_citations} citations ({enrichment_success_rate:.1f}% success rate)"
        )
        
        # Warn if enrichment success rate is low
        if total_citations > 0 and enrichment_success_rate < 80.0:
            logger.warning(
                "citation_enrichment_low_success_rate",
                success_rate=enrichment_success_rate,
                total_citations=total_citations,
                enriched_citations=enriched_count,
                message=f"Low enrichment success rate: {enrichment_success_rate:.1f}% - may indicate citation mapping issues"
            )
        
        if invalid_citations > 0:
            logger.warning(
                "citation_enrichment_invalid_citations",
                invalid_count=invalid_citations,
                message=f"Found {invalid_citations} invalid citations after enrichment"
            )
    
    async def execute(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """Execute aggregation phase: synthesize final answer.
        
        Args:
            state: Graph state with followup_results, primer_json, question
            
        Returns:
            State updates with tree and result_json
        """
        # Check if no data was found
        if state.get("no_data_found", False):
            logger.warning("aggregate_no_data_found", message="No data found in retrieval")
            
            empty_result = {
                "final_answer": "",
                "key_facts": [],
                "residual_uncertainty": "",
                "no_data_found": True
            }
            
            # Publish completion with no data status
            publisher = self._publisher_from_state(state)
            if publisher:
                try:
                    prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                    await publisher.publish_retrieval_update(
                        project_id=UUID(state["project_id"]),
                        retrieval_id=state["retrieval_id"],
                        phase=RetrievalStatus.COMPLETED,
                        thought_summary="**No Data Found**",
                        details_md="No relevant communities or chunks found in the knowledge graph for this query.",
                        progress_pct=100.0,
                        prompt_id=prompt_id_uuid,
                    )
                except Exception as exc:
                    logger.debug("publish_failed", phase="no_data_completion", error=str(exc))
            
            return {"tree": {}, "result_json": empty_result}
        
        # Build reasoning tree
        primer_data = state.get("primer_json", {})
        primer_minimal = {
            "initial_answer": primer_data.get("initial_answer", ""),
        }
        
        tree = {
            "question": state["question"],
            "primer": primer_minimal,
            "followups": state.get("followup_results"),
        }
        
        # Extract valid citations for prompt validation
        followup_results = state.get("followup_results", [])
        valid_citations_str = self._extract_valid_citations_for_prompt(followup_results)
        
        # Generate aggregated response
        llm = self._get_llm()
        prompt = aggregator_prompt()
        msg = prompt.format_messages(
            question=state["question"],
            tree=json.dumps(tree),
            valid_citations=valid_citations_str,
        )
        
        agg_response = await llm.ainvoke(msg)
        
        # Validate response
        agg_validated = parse_and_validate(
            agg_response,
            AggregatorResponse,
            "Aggregator",
        )
        
        # Enrich citations
        self._enrich_key_facts_citations(agg_validated, state.get("followup_results", []))
        
        result_json = agg_validated.model_dump()
        
        # Build top-level deduplicated citations with validation
        # Only include citations that have both chunk_id and document_name
        all_citations = []
        invalid_final_citations = 0
        
        for kf_idx, kf in enumerate(agg_validated.key_facts):
            for cit in kf.citations:
                if isinstance(cit, dict):
                    chunk_id = cit.get("chunk_id")
                    document_name = cit.get("document_name")
                    
                    # Validate citation has both required fields
                    if chunk_id and document_name and document_name != "unknown":
                        all_citations.append(cit)
                    else:
                        invalid_final_citations += 1
                        logger.warning(
                            "citation_excluded_from_final_output",
                            key_fact_index=kf_idx,
                            chunk_id=chunk_id,
                            document_name=document_name,
                            has_chunk_id=bool(chunk_id),
                            has_document_name=bool(document_name and document_name != "unknown"),
                            message="Citation excluded from final output: missing required fields"
                        )
                elif isinstance(cit, str):
                    # String citation that wasn't enriched - exclude from final output
                    invalid_final_citations += 1
                    logger.warning(
                        "citation_string_excluded_from_final_output",
                        key_fact_index=kf_idx,
                        chunk_id=cit,
                        message="String citation excluded from final output: enrichment failed"
                    )
        
        result_json["citations"] = deduplicate_citations(all_citations)
        
        # Log final citation statistics
        if invalid_final_citations > 0:
            logger.warning(
                "citations_excluded_from_final_output",
                excluded_count=invalid_final_citations,
                valid_citations=len(result_json["citations"]),
                message=f"Excluded {invalid_final_citations} invalid citations from final output"
            )
        
        logger.info(
            "retrieval.aggregate.citations_finalized",
            total_key_facts=len(agg_validated.key_facts),
            valid_top_level_citations=len(result_json["citations"]),
            excluded_citations=invalid_final_citations,
            message=f"Final output: {len(result_json['citations'])} valid citations"
        )
        
        logger.info(
            "retrieval.aggregate.done",
            result_keys=len(list(result_json.keys())),
            key_facts_count=len(agg_validated.key_facts),
            top_level_citations=len(result_json["citations"]),
            has_final_answer=bool(agg_validated.final_answer),
            has_uncertainty=bool(agg_validated.residual_uncertainty),
        )
        
        # Publish aggregation status
        publisher = self._publisher_from_state(state)
        if publisher:
            try:
                prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                
                # Format key facts with citations
                facts_detail_md = ""
                if agg_validated.key_facts:
                    for kf in agg_validated.key_facts:
                        facts_detail_md += f"  - {kf.fact}\n"
                        if kf.citations:
                            # Use shared citation display formatter
                            citation_display = format_citations_for_display(kf.citations, max_display=5)
                            if citation_display:
                                facts_detail_md += f"    *Sources:* {citation_display}\n"
                else:
                    facts_detail_md = "No key facts found."
                
                await publisher.publish_retrieval_update(
                    project_id=UUID(state["project_id"]),
                    retrieval_id=state["retrieval_id"],
                    phase=RetrievalStatus.AGGREGATING_RESULTS,
                    thought_summary="🧩 **Synthesizing Results**",
                    details_md=(
                        f"*Key facts collected:* {len(agg_validated.key_facts)}  \n"
                        + facts_detail_md
                        + "\n\nAggregating findings from all sources into cohesive context..."
                    ),
                    progress_pct=90.0,
                    prompt_id=prompt_id_uuid,
                )
            except Exception as exc:
                logger.debug("publish_failed", phase="aggregate", error=str(exc))
        
        # Publish completion
        if publisher:
            try:
                prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                await publisher.publish_retrieval_update(
                    project_id=UUID(state["project_id"]),
                    retrieval_id=state["retrieval_id"],
                    phase=RetrievalStatus.COMPLETED,
                    thought_summary="✅ **Context Retrieval Complete**",
                    details_md="**Final answer ready.** Proceeding with generation...",
                    progress_pct=100.0,
                    prompt_id=prompt_id_uuid,
                )
            except Exception as exc:
                logger.debug("publish_failed", phase="completed", error=str(exc))
        
        return {"tree": tree, "result_json": result_json}

