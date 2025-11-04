"""Shared JSON parsing and validation utilities for LLM responses.

This module provides utilities for:
- Parsing JSON from LLM responses (string or object with .content)
- Validating against Pydantic models with graceful fallbacks
- Extracting string content from various response formats
"""

from typing import Any, TypeVar
import json
import structlog
from pydantic import BaseModel
from fastapi import HTTPException

logger = structlog.get_logger(__name__)

T = TypeVar("T", bound=BaseModel)


def extract_string_content(obj: Any) -> str:
    """Extract string content from LLM response object.
    
    Handles various response formats:
    - String directly
    - Object with .content attribute (LangChain responses)
    - Object with .text attribute
    
    Args:
        obj: LLM response object or string
        
    Returns:
        Extracted string content
    """
    if isinstance(obj, str):
        return obj
    
    if hasattr(obj, "content"):
        return str(obj.content)
    
    if hasattr(obj, "text"):
        return str(obj.text)
    
    return str(obj)


def parse_json_or_error(raw: Any, label: str, status_code: int = 502) -> dict[str, Any]:
    """Parse JSON from LLM response with error handling.
    
    Args:
        raw: Raw LLM response (string or object with .content)
        label: Label for logging context
        status_code: HTTP status code to use in exception
        
    Returns:
        Parsed JSON dictionary
        
    Raises:
        HTTPException: If JSON parsing fails
    """
    content = extract_string_content(raw)
    
    try:
        parsed = json.loads(content)
    except json.JSONDecodeError as exc:
        logger.error(
            "json_parse_failed",
            label=label,
            error=str(exc),
            content_preview=content[:200],
        )
        raise HTTPException(
            status_code=status_code,
            detail=f"{label} JSON parse failed: {exc}"
        )
    
    if not isinstance(parsed, dict):
        logger.warning(
            "json_not_dict",
            label=label,
            type=type(parsed).__name__,
            message="Parsed JSON is not a dictionary, returning empty dict"
        )
        return {}
    
    return parsed


def parse_and_validate(
    raw: Any,
    model_class: type[T],
    label: str,
    use_strict: bool = False,
) -> T:
    """Parse JSON and validate against Pydantic model with normalization.
    
    This function provides:
    - JSON parsing with error handling
    - Pydantic validation with automatic normalization
    - Graceful fallback to default model on validation failure
    - Comprehensive logging for debugging
    
    Args:
        raw: Raw LLM response (string or object with .content)
        model_class: Pydantic model class for validation
        label: Label for logging context
        use_strict: If True, uses strict validation (no coercion)
        
    Returns:
        Validated and normalized model instance
        
    Raises:
        HTTPException: Only on JSON parse failure (502)
    """
    content = extract_string_content(raw)
    
    try:
        parsed = json.loads(content)
    except json.JSONDecodeError as exc:
        logger.error(
            "json_parse_failed",
            label=label,
            error=str(exc),
            content_preview=content[:200],
        )
        raise HTTPException(
            status_code=502,
            detail=f"{label} JSON parse failed: {exc}"
        )
    
    # First attempt: standard validation
    try:
        validated = model_class.model_validate(parsed, strict=use_strict)
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
            message="Attempting normalized fallback with strict=False"
        )
        
        # Second attempt: non-strict validation (allows coercion)
        if use_strict:
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
                logger.error(
                    "response_validation_fallback_failed",
                    label=label,
                    model=model_class.__name__,
                    error=str(exc2)[:200],
                    message="Both strict and non-strict validation failed, returning empty model"
                )
        
        # Last resort: return empty model
        logger.error(
            "response_validation_fatal",
            label=label,
            model=model_class.__name__,
            message="Returning empty model as last resort"
        )
        return model_class()


def safe_json_loads(text: str, default: Any = None) -> Any:
    """Safely parse JSON with default fallback.
    
    Args:
        text: JSON string to parse
        default: Default value to return on parse failure
        
    Returns:
        Parsed JSON or default value
    """
    try:
        return json.loads(text)
    except (json.JSONDecodeError, TypeError, ValueError):
        return default


def pretty_json(obj: Any, indent: int = 2) -> str:
    """Convert object to pretty-printed JSON string.
    
    Args:
        obj: Object to serialize
        indent: Indentation level
        
    Returns:
        Pretty-printed JSON string
    """
    try:
        return json.dumps(obj, indent=indent, ensure_ascii=False)
    except (TypeError, ValueError) as exc:
        logger.warning("json_serialize_failed", error=str(exc))
        return str(obj)

