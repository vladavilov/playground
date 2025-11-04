"""Shared citation validation and enrichment utilities for RAG pipelines.

This module provides utilities for:
- Validating citations against retrieved chunks
- Filtering invalid/hallucinated citations
- Enriching citations with document metadata
- Deduplication and quality checks
"""

from typing import Any, Union
import structlog

logger = structlog.get_logger(__name__)


def validate_citations(
    citations: list[Any],
    valid_chunk_ids: set[str],
    chunk_to_doc_map: dict[str, str],
    context_label: str = "unknown",
) -> list[dict[str, Any]]:
    """Validate and filter citations against retrieved chunks.
    
    Filters out:
    - None or empty chunk_ids (LLM output errors)
    - chunk_ids not in retrieved set (hallucination)
    - Spans that are too short (< 5 characters)
    - Duplicate citations (same chunk_id + span prefix)
    
    Args:
        citations: List of citation objects with chunk_id and span attributes
        valid_chunk_ids: Set of chunk IDs that were actually retrieved
        chunk_to_doc_map: Mapping from chunk_id to document_name
        context_label: Label for logging context (e.g., "followup_0")
        
    Returns:
        List of validated citation dictionaries with {chunk_id, span, document_name}
    """
    validated_citations = []
    null_count = 0
    hallucinated_count = 0
    short_span_count = 0
    duplicate_count = 0
    seen_citations = set()  # Track (chunk_id, span_prefix) for deduplication
    
    for idx, citation in enumerate(citations):
        # Handle both object (with .chunk_id attribute) and dict formats
        if hasattr(citation, "chunk_id"):
            chunk_id = citation.chunk_id
            span = getattr(citation, "span", "")
        elif isinstance(citation, dict):
            chunk_id = citation.get("chunk_id")
            span = citation.get("span", "")
        else:
            logger.warning(
                "citation_validation_unknown_type",
                context=context_label,
                citation_index=idx,
                type=type(citation).__name__,
                message="Unknown citation type, skipping"
            )
            continue
        
        # Reject None or empty chunk_ids
        if chunk_id is None or not str(chunk_id).strip():
            null_count += 1
            logger.warning(
                "citation_validation_null_chunk_id",
                context=context_label,
                citation_index=idx,
                span_preview=span[:50] if span else "",
                message="Filtered citation with None/empty chunk_id (LLM output error)"
            )
            continue
        
        chunk_id = str(chunk_id).strip()
        
        # Reject chunk_ids not in retrieved set (hallucination)
        if chunk_id not in valid_chunk_ids:
            hallucinated_count += 1
            logger.warning(
                "citation_validation_unmatched_chunk_id",
                context=context_label,
                citation_index=idx,
                chunk_id=chunk_id,
                span_preview=span[:50] if span else "",
                valid_chunk_ids=list(valid_chunk_ids)[:5],
                message="Filtered citation with chunk_id not in retrieved set (LLM hallucination)"
            )
            continue
        
        # Reject spans that are too short (< 5 characters)
        if len(str(span).strip()) < 5:
            short_span_count += 1
            logger.warning(
                "citation_validation_short_span",
                context=context_label,
                citation_index=idx,
                chunk_id=chunk_id,
                span_length=len(str(span).strip()),
                span=span,
                message="Filtered citation with span length < 5 characters"
            )
            continue
        
        # Reject duplicate citations (same chunk_id + span prefix)
        span_fingerprint = (chunk_id, str(span)[:100])
        if span_fingerprint in seen_citations:
            duplicate_count += 1
            logger.debug(
                "citation_validation_duplicate",
                context=context_label,
                citation_index=idx,
                chunk_id=chunk_id,
                message="Filtered duplicate citation with same chunk_id and span"
            )
            continue
        seen_citations.add(span_fingerprint)
        
        # Valid citation - include with document name
        validated_citations.append({
            "chunk_id": chunk_id,
            "span": str(span),
            "document_name": chunk_to_doc_map.get(chunk_id, "unknown")
        })
    
    invalid_count = null_count + hallucinated_count + short_span_count + duplicate_count
    
    # Log summary if any citations were filtered
    if invalid_count > 0:
        logger.warning(
            "citation_validation_summary",
            context=context_label,
            total_citations=len(citations),
            valid_citations=len(validated_citations),
            null_chunk_ids=null_count,
            hallucinated_chunk_ids=hallucinated_count,
            short_spans=short_span_count,
            duplicates=duplicate_count,
            filtered_count=invalid_count,
            message=f"Filtered {invalid_count}/{len(citations)} invalid citations"
        )
    else:
        logger.debug(
            "citation_validation_all_valid",
            context=context_label,
            citation_count=len(validated_citations)
        )
    
    return validated_citations


def enrich_citations(
    aggregated_citations: list[Union[str, dict]],
    citation_map: dict[str, dict[str, Any]],
    context_label: str = "aggregation",
) -> list[Union[str, dict]]:
    """Enrich aggregated citations with full metadata from source results.
    
    The LLM aggregator returns chunk IDs as strings, but we need full citation objects
    with {chunk_id, span, document_name} for downstream services. This function maps
    chunk ID strings to rich citation objects.
    
    Args:
        aggregated_citations: List of citations (strings or dicts) from aggregator
        citation_map: Mapping from chunk_id to full citation object
        context_label: Label for logging context
        
    Returns:
        List of enriched citations (preserves both string and dict formats)
    """
    enriched = []
    not_found_count = 0
    
    for citation_item in aggregated_citations:
        # Handle both string chunk IDs (from LLM) and already-enriched dicts
        if isinstance(citation_item, dict):
            # Already enriched - preserve as-is
            enriched.append(citation_item)
            logger.debug(
                "citation_already_enriched",
                context=context_label,
                chunk_id=citation_item.get("chunk_id"),
                message="Citation already in enriched format"
            )
        elif isinstance(citation_item, str):
            # String chunk ID - look up in citation map
            chunk_id = citation_item.strip()
            
            # Skip empty/whitespace-only citations
            if not chunk_id:
                logger.debug(
                    "citation_enrichment_empty_skipped",
                    context=context_label,
                    message="Skipped empty/whitespace-only string citation"
                )
                continue
            
            if chunk_id in citation_map:
                # Found full metadata - use it
                enriched.append(citation_map[chunk_id])
            else:
                # Not found - keep as string (will be treated as legacy format downstream)
                not_found_count += 1
                logger.warning(
                    "citation_enrichment_not_found",
                    context=context_label,
                    chunk_id=chunk_id,
                    message="Chunk ID from aggregator not found in source results"
                )
                enriched.append(chunk_id)
        else:
            # Unknown type - coerce to string and keep as-is
            logger.warning(
                "citation_enrichment_unknown_type",
                context=context_label,
                type=type(citation_item).__name__,
                value=str(citation_item)[:50],
                message="Unknown citation type during enrichment"
            )
            enriched.append(str(citation_item))
    
    if not_found_count > 0:
        logger.warning(
            "citation_enrichment_not_found_summary",
            context=context_label,
            not_found_count=not_found_count,
            total=len(aggregated_citations),
            message=f"{not_found_count} citations not found in source results"
        )
    
    return enriched


def deduplicate_citations(citations: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Deduplicate citations by chunk_id, keeping first occurrence.
    
    Args:
        citations: List of citation dictionaries
        
    Returns:
        Deduplicated list of citations
    """
    seen_ids = set()
    deduplicated = []
    
    for citation in citations:
        chunk_id = citation.get("chunk_id")
        if chunk_id and chunk_id not in seen_ids:
            deduplicated.append(citation)
            seen_ids.add(chunk_id)
    
    if len(deduplicated) < len(citations):
        logger.debug(
            "citations_deduplicated",
            original=len(citations),
            deduplicated=len(deduplicated),
            removed=len(citations) - len(deduplicated),
        )
    
    return deduplicated


def filter_valid_citations(citations: list[Union[str, dict]]) -> list[Union[str, dict]]:
    """Filter out invalid citation entries (empty strings, empty dicts, None values).
    
    Args:
        citations: List of citation entries (strings or dicts)
        
    Returns:
        Filtered list with only valid entries
    """
    filtered = []
    
    for cit in citations:
        if isinstance(cit, dict):
            # Valid if has non-empty chunk_id
            if cit.get("chunk_id") and str(cit.get("chunk_id")).strip():
                filtered.append(cit)
        elif isinstance(cit, str):
            # Valid if non-empty string
            if cit.strip():
                filtered.append(cit)
    
    if len(filtered) < len(citations):
        logger.debug(
            "citations_filtered",
            original=len(citations),
            filtered=len(filtered),
            removed=len(citations) - len(filtered),
        )
    
    return filtered


def format_citations_for_display(
    citations: list[Union[str, dict]],
    max_display: int = 5,
) -> str:
    """Format citations for UI display with deduplication and truncation.
    
    Extracts document names from citations, deduplicates them, and formats
    as a comma-separated list with "and N more" suffix if truncated.
    
    Args:
        citations: List of citation objects (dicts with document_name or strings)
        max_display: Maximum number of document names to display (default: 5)
        
    Returns:
        Formatted string like "doc1, doc2, doc3 (and 2 more)" or empty string if no citations
        
    Examples:
        >>> cits = [
        ...     {"chunk_id": "1", "document_name": "file.py"},
        ...     {"chunk_id": "2", "document_name": "file.py"},  # duplicate
        ...     {"chunk_id": "3", "document_name": "config.yaml"},
        ... ]
        >>> format_citations_for_display(cits)
        'file.py, config.yaml'
        
        >>> format_citations_for_display(cits, max_display=1)
        'file.py (and 1 more)'
    """
    if not citations:
        return ""
    
    # Extract and deduplicate document names
    seen_docs = set()
    citation_displays = []
    
    for cit in citations:
        if isinstance(cit, dict):
            doc_name = cit.get("document_name", "unknown")
            if doc_name not in seen_docs:
                citation_displays.append(doc_name)
                seen_docs.add(doc_name)
        elif isinstance(cit, str):
            # String citations (legacy format or chunk IDs)
            cit_str = str(cit).strip()
            if cit_str and cit_str not in seen_docs:
                citation_displays.append(cit_str)
                seen_docs.add(cit_str)
    
    if not citation_displays:
        return ""
    
    # Truncate and format
    display_count = min(max_display, len(citation_displays))
    result = ", ".join(citation_displays[:display_count])
    
    # Add "and N more" suffix if truncated
    if len(citation_displays) > max_display:
        remaining = len(citation_displays) - max_display
        result += f" (and {remaining} more)"
    
    return result

