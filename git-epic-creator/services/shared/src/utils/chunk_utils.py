"""Shared chunk processing utilities for RAG pipelines.

This module provides utilities for:
- Extracting salient sentences from chunks
- Adaptive compression based on repetition
- Truncation for prompt size management
- Token counting and size monitoring
"""

from typing import Any
import json
import structlog

from utils.token_utils import count_tokens

logger = structlog.get_logger(__name__)


def extract_salient_sentences(text: str, max_sentences: int = 3) -> str:
    """Extract salient sentences from chunk text.
    
    Simple extraction based on sentence boundaries. Used when chunk repetition is high
    to reduce token usage while preserving key information.
    
    Args:
        text: Full chunk text
        max_sentences: Maximum number of sentences to extract
        
    Returns:
        Compressed text with top N sentences
    """
    if not text:
        return ""
    
    # Simple sentence splitting on common terminators
    sentences = []
    for terminator in ['. ', '.\n', '! ', '!\n', '? ', '?\n']:
        if terminator in text:
            sentences = text.split(terminator)
            break
    
    if not sentences:
        # No clear sentence boundaries - return truncated text
        return text[:500]
    
    # Take first N sentences (prioritize early content)
    selected = sentences[:max_sentences]
    result = '. '.join(s.strip() for s in selected if s.strip())
    
    # Add ellipsis if truncated
    if len(sentences) > max_sentences:
        result += "..."
    
    return result


def compress_chunks_adaptive(
    chunks: list[dict[str, Any]],
    overlap_ratio: float,
    compression_threshold: float = 0.5,
    max_sentences: int = 3,
) -> list[dict[str, Any]]:
    """Compress chunk text when repetition is detected.
    
    Args:
        chunks: List of chunk dictionaries with 'text' field
        overlap_ratio: Ratio of overlapping chunks (0.0-1.0)
        compression_threshold: Only compress if overlap_ratio exceeds this
        max_sentences: Number of sentences to extract per chunk
        
    Returns:
        Compressed chunks with extracted salient sentences if overlap > threshold
    """
    if overlap_ratio <= compression_threshold:
        return chunks
    
    compressed = []
    for chunk in chunks:
        compressed_chunk = chunk.copy()
        original_text = chunk.get("text", "")
        
        if original_text:
            compressed_text = extract_salient_sentences(original_text, max_sentences=max_sentences)
            compressed_chunk["text"] = compressed_text
            compressed_chunk["_compressed"] = True  # Mark for debugging
        
        compressed.append(compressed_chunk)
    
    logger.debug(
        "chunks_compressed",
        total_chunks=len(chunks),
        overlap_ratio=overlap_ratio,
        compression_threshold=compression_threshold,
        message="Applied adaptive compression to reduce token usage"
    )
    
    return compressed


def truncate_for_prompt(
    data: list[dict[str, Any]],
    max_items: int,
    label: str,
    model_name: str = "gpt-4",
    warn_threshold_tokens: int = 2000,
) -> str:
    """Truncate list data for prompt inclusion with size monitoring.
    
    Prevents token overflow by limiting array size and logging when truncation occurs.
    Also monitors serialized size to warn about potential token pressure.
    
    Args:
        data: List of dictionaries to serialize
        max_items: Maximum number of items to include
        label: Label for logging context
        model_name: Model name for token counting
        warn_threshold_tokens: Warn if token count exceeds this
        
    Returns:
        JSON-serialized string of truncated data
    """
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
    
    return serialized


def calculate_overlap_ratio(current_chunk_ids: set[str], seen_chunk_ids: set[str]) -> float:
    """Calculate overlap ratio between current and previously seen chunks.
    
    Args:
        current_chunk_ids: Set of chunk IDs in current batch
        seen_chunk_ids: Set of chunk IDs seen in previous batches
        
    Returns:
        Overlap ratio (0.0-1.0)
    """
    if not current_chunk_ids:
        return 0.0
    
    overlap_count = len(current_chunk_ids.intersection(seen_chunk_ids))
    return overlap_count / len(current_chunk_ids)


def reorder_chunks_by_novelty(
    chunks: list[Any],
    seen_chunk_ids: set[str],
    chunk_id_field: str = "chunk_id",
) -> list[Any]:
    """Reorder chunks to prioritize unseen content (novelty).
    
    Places unseen chunks first, then seen chunks, to maximize information diversity
    across multiple retrieval iterations.
    
    Args:
        chunks: List of chunk objects or IDs
        seen_chunk_ids: Set of chunk IDs already processed
        chunk_id_field: Field name to extract chunk ID from dict (if chunks are dicts)
        
    Returns:
        Reordered chunk list with unseen chunks first
    """
    if not seen_chunk_ids:
        return chunks
    
    unseen = []
    seen = []
    
    for chunk in chunks:
        # Handle both dict and string formats
        if isinstance(chunk, dict):
            chunk_id = chunk.get(chunk_id_field)
        else:
            chunk_id = chunk
        
        if chunk_id in seen_chunk_ids:
            seen.append(chunk)
        else:
            unseen.append(chunk)
    
    if unseen:
        logger.debug(
            "chunks_reordered_by_novelty",
            total_chunks=len(chunks),
            unseen_count=len(unseen),
            seen_count=len(seen),
            message="Reordered chunks to prioritize unseen content"
        )
    
    return unseen + seen


def truncate_chunk_text(text: str, max_length: int) -> str:
    """Truncate chunk text to maximum length with ellipsis.
    
    Args:
        text: Chunk text to truncate
        max_length: Maximum character length
        
    Returns:
        Truncated text with ellipsis if needed
    """
    if not text or len(text) <= max_length:
        return text
    
    return text[:max_length] + "..."


def batch_chunks(chunks: list[Any], batch_size: int) -> list[list[Any]]:
    """Split chunks into batches of specified size.
    
    Args:
        chunks: List of chunks to batch
        batch_size: Number of chunks per batch
        
    Returns:
        List of chunk batches
    """
    if batch_size <= 0:
        raise ValueError("batch_size must be positive")
    
    return [chunks[i:i + batch_size] for i in range(0, len(chunks), batch_size)]

