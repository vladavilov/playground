"""Shared token counting utilities using tiktoken for accurate measurement.

This module provides token counting for various OpenAI/Azure OpenAI models,
with fallback heuristics when tiktoken fails.
"""

from functools import lru_cache
import structlog
import tiktoken

logger = structlog.get_logger(__name__)


@lru_cache(maxsize=1)
def get_token_encoder(model_name: str = "gpt-4"):
    """Get or create cached tiktoken encoder for specified model.
    
    Args:
        model_name: Model name for tiktoken (e.g., 'gpt-4', 'gpt-3.5-turbo')
        
    Returns:
        Tiktoken encoder instance
        
    Raises:
        ImportError: If tiktoken is not installed
    """
    try:
        encoder = tiktoken.encoding_for_model(model_name)
        logger.debug("tiktoken_encoder_initialized", model=model_name)
        return encoder
    except Exception as exc:
        logger.warning(
            "tiktoken_init_failed",
            model=model_name,
            error=str(exc),
            message="Falling back to cl100k_base encoding"
        )
        return tiktoken.get_encoding("cl100k_base")


def count_tokens(text: str, model_name: str = "gpt-4", use_heuristic_fallback: bool = True) -> int:
    """Count tokens using tiktoken for accurate measurement.
    
    Args:
        text: Text to count tokens for
        model_name: Model name for tiktoken encoding
        use_heuristic_fallback: If True, falls back to heuristic (len/4) on error
        
    Returns:
        Token count as integer
        
    Raises:
        ImportError: If tiktoken is not installed and fallback is disabled
    """
    if not text:
        return 0
    
    try:
        encoder = get_token_encoder(model_name)
        return len(encoder.encode(text))
    except Exception as exc:
        if use_heuristic_fallback:
            logger.warning(
                "token_count_fallback",
                error=str(exc),
                text_len=len(text),
                message="Using heuristic token count (len/4)"
            )
            return len(text) // 4
        else:
            raise


def count_message_tokens(messages: list, model_name: str = "gpt-4") -> int:
    """Count tokens in LangChain message list (compatible with trim_messages).
    
    This function provides compatibility with LangChain's trim_messages token_counter
    parameter, replacing count_tokens_approximately with accurate tiktoken counting.
    
    Args:
        messages: List of message objects (LangChain format)
        model_name: Model name for tiktoken encoding
        
    Returns:
        Total token count for all messages
        
    Example:
        >>> from langchain_core.messages.utils import trim_messages
        >>> trimmed = trim_messages(
        ...     messages,
        ...     strategy="last",
        ...     token_counter=count_message_tokens,  # Use our accurate counter
        ...     max_tokens=512,
        ... )
    """
    total_tokens = 0
    for msg in messages:
        # Extract text content from message object
        if hasattr(msg, 'content'):
            content = msg.content
            if isinstance(content, str):
                total_tokens += count_tokens(content, model_name=model_name)
            elif isinstance(content, list):
                # Handle multi-modal content (list of content blocks)
                for block in content:
                    if isinstance(block, dict) and 'text' in block:
                        total_tokens += count_tokens(block['text'], model_name=model_name)
                    elif isinstance(block, str):
                        total_tokens += count_tokens(block, model_name=model_name)
        
        # Add overhead for message formatting (role, etc.)
        # OpenAI message format adds ~4 tokens per message
        total_tokens += 4
    
    return total_tokens
