"""Shared utilities for AI services.

This package provides reusable utilities for:
- LLM client creation and management
- Token counting and cost estimation
- JSON parsing and validation
- Citation processing and validation
- Chunk manipulation and compression
- Embedding generation with caching
- Retry policies and error handling
"""

from utils.llm_client_factory import create_llm, create_embedder, clear_llm_cache
from utils.token_utils import count_tokens, count_message_tokens, get_token_encoder
from utils.json_utils import (
    extract_string_content,
    parse_json_or_error,
    parse_and_validate,
    safe_json_loads,
    pretty_json,
)
from utils.citation_utils import (
    validate_citations,
    enrich_citations,
    deduplicate_citations,
    filter_valid_citations,
    format_citations_for_display,
)
from utils.chunk_utils import (
    extract_salient_sentences,
    compress_chunks_adaptive,
    truncate_for_prompt,
    calculate_overlap_ratio,
    reorder_chunks_by_novelty,
    truncate_chunk_text,
    batch_chunks,
)
from utils.embedding_service import EmbeddingService
from utils.citation_parser import parse_citations_from_response, parse_key_facts

__all__ = [
    # LLM client factory
    "create_llm",
    "create_embedder",
    "clear_llm_cache",
    # Token utilities
    "count_tokens",
    "count_message_tokens",
    "get_token_encoder",
    # JSON utilities
    "extract_string_content",
    "parse_json_or_error",
    "parse_and_validate",
    "safe_json_loads",
    "pretty_json",
    # Citation utilities
    "validate_citations",
    "enrich_citations",
    "deduplicate_citations",
    "filter_valid_citations",
    "format_citations_for_display",
    # Citation parser utilities
    "parse_citations_from_response",
    "parse_key_facts",
    # Chunk utilities
    "extract_salient_sentences",
    "compress_chunks_adaptive",
    "truncate_for_prompt",
    "calculate_overlap_ratio",
    "reorder_chunks_by_novelty",
    "truncate_chunk_text",
    "batch_chunks",
    # Embedding service
    "EmbeddingService",
]
