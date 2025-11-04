"""Shared citation parser for GraphRAG retrieval responses.

Handles parsing citations from retrieval service responses with support for:
- Legacy string format (chunk_id only)
- New dict format (chunk_id, span, document_name)
- Validation and warnings for missing data
"""

from typing import Any, List
import structlog

logger = structlog.get_logger(__name__)


def parse_citations_from_response(
    data: dict[str, Any],
    citation_model_class: type,
) -> List[Any]:
    """Parse citations from GraphRAG retrieval response.
    
    Handles both key_facts citations and top-level citations, supporting
    legacy string format and new dict format.
    
    Args:
        data: Response data from retrieval service
        citation_model_class: Pydantic model class for citations (e.g., Citation)
        
    Returns:
        List of citation model instances
    """
    citations = []
    
    # Parse citations from key_facts
    try:
        for kf in data.get("key_facts", []) or []:
            for cit in kf.get("citations", []) or []:
                citation = _parse_single_citation(cit, "key_fact")
                if citation:
                    citations.append(citation_model_class(**citation))
    except Exception as exc:
        logger.warning("key_facts_citation_parse_error", error=str(exc))
    
    # Parse top-level citations
    try:
        for cit in data.get("citations", []) or []:
            citation = _parse_single_citation(cit, "top_level")
            if citation:
                citations.append(citation_model_class(**citation))
    except Exception as exc:
        logger.warning("toplevel_citation_parse_error", error=str(exc))
    
    logger.debug("citations_parsed", count=len(citations))
    return citations


def _parse_single_citation(cit: Any, source: str) -> dict[str, str] | None:
    """Parse a single citation from response data.
    
    Args:
        cit: Citation data (string or dict)
        source: Source context for logging (e.g., "key_fact", "top_level")
        
    Returns:
        Dict with chunk_id, text_preview, document_name or None if invalid
    """
    try:
        if isinstance(cit, str):
            # Legacy format: just chunk_id
            if not cit.strip():
                return None
            
            logger.debug(
                "legacy_citation_format",
                chunk_id=cit,
                source=source,
                message="Citation in legacy string format"
            )
            return {
                "chunk_id": cit,
                "text_preview": "",
                "document_name": "unknown"
            }
            
        elif isinstance(cit, dict):
            # New format: full citation object
            chunk_id = str(cit.get("chunk_id", ""))
            span = str(cit.get("span", ""))
            doc_name = str(cit.get("document_name", "unknown"))
            
            # Validate chunk_id
            if not chunk_id or not chunk_id.strip():
                logger.warning(
                    "citation_empty_chunk_id",
                    citation=cit,
                    source=source,
                    message="Citation has empty chunk_id - skipping"
                )
                return None
            
            # Warn if document_name is unknown or empty
            if doc_name == "unknown" or not doc_name.strip():
                logger.debug(
                    "citation_unknown_document",
                    chunk_id=chunk_id,
                    span_preview=span[:50] if span else "",
                    source=source,
                    message="Citation has 'unknown' document name"
                )
            
            # Create text preview from span
            text_preview = span[:150] + "..." if len(span) > 150 else span
            
            return {
                "chunk_id": chunk_id,
                "text_preview": text_preview,
                "document_name": doc_name
            }
        else:
            logger.warning(
                "citation_unknown_type",
                type=type(cit).__name__,
                source=source,
            )
            return None
            
    except Exception as exc:
        logger.warning(
            "citation_parse_error",
            error=str(exc),
            citation_preview=str(cit)[:100],
            source=source,
        )
        return None


def parse_key_facts(data: dict[str, Any]) -> List[str]:
    """Extract key facts from retrieval response.
    
    Args:
        data: Response data from retrieval service
        
    Returns:
        List of key fact strings
    """
    key_facts = []
    
    try:
        for kf in data.get("key_facts", []) or []:
            fact = kf.get("fact")
            if isinstance(fact, str) and fact:
                key_facts.append(fact)
    except Exception as exc:
        logger.warning("key_facts_parse_error", error=str(exc))
    
    return key_facts

