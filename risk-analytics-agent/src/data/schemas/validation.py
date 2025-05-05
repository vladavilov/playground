"""Validation utilities for data schemas.

This module provides utilities for validating data against schemas, including
error handling, batch validation, and custom validation logic.
"""

import json
import logging
from typing import Any, Dict, List, Optional, Tuple, Type, TypeVar, Union

from pydantic import BaseModel, ValidationError

from .base import BaseSchema

logger = logging.getLogger(__name__)

T = TypeVar('T', bound=BaseSchema)


def validate_data(
    data: Union[Dict[str, Any], str],
    schema_class: Type[T],
    raise_exception: bool = False
) -> Tuple[Optional[T], Optional[Dict[str, Any]]]:
    """Validate data against a schema.
    
    Args:
        data: Data to validate (dict or JSON string)
        schema_class: Schema class to validate against
        raise_exception: Whether to raise an exception on validation error
        
    Returns:
        Tuple[Optional[T], Optional[Dict[str, Any]]]: Tuple of (validated model, errors)
        If validation succeeds, errors will be None. If validation fails, model will be None.
    """
    # If data is a string, try to parse as JSON
    if isinstance(data, str):
        try:
            data = json.loads(data)
        except json.JSONDecodeError as e:
            error = {"json_error": str(e)}
            if raise_exception:
                raise ValueError(f"Invalid JSON: {e}")
            return None, error
    
    try:
        # Validate data against schema
        model = schema_class(**data)
        return model, None
    except ValidationError as e:
        # Extract validation errors
        errors = e.errors()
        formatted_errors = {f"{error['loc'][0]}": error['msg'] for error in errors}
        
        logger.warning(f"Validation error for {schema_class.__name__}: {formatted_errors}")
        
        if raise_exception:
            raise
        
        return None, formatted_errors


def validate_batch(
    data_list: List[Dict[str, Any]],
    schema_class: Type[T]
) -> Tuple[List[T], List[Dict[str, Any]]]:
    """Validate a batch of data against a schema.
    
    Args:
        data_list: List of data dictionaries to validate
        schema_class: Schema class to validate against
        
    Returns:
        Tuple[List[T], List[Dict[str, Any]]]: Tuple of (valid models, invalid entries with errors)
    """
    valid_models = []
    invalid_entries = []
    
    for i, data in enumerate(data_list):
        model, errors = validate_data(data, schema_class)
        
        if model:
            valid_models.append(model)
        else:
            invalid_entries.append({
                "index": i,
                "data": data,
                "errors": errors
            })
    
    if invalid_entries:
        logger.warning(f"Batch validation: {len(invalid_entries)} out of {len(data_list)} entries failed validation")
    
    return valid_models, invalid_entries


def convert_to_dto(
    model: BaseModel,
    exclude_none: bool = True,
    by_alias: bool = True
) -> Dict[str, Any]:
    """Convert a Pydantic model to a DTO (Data Transfer Object).
    
    Args:
        model: The Pydantic model to convert
        exclude_none: Whether to exclude None values
        by_alias: Whether to use field aliases in output (camelCase)
        
    Returns:
        Dict[str, Any]: The model converted to a dictionary
    """
    return model.dict(exclude_none=exclude_none, by_alias=by_alias)


def validate_json_string(
    json_string: str,
    schema_class: Type[T],
    raise_exception: bool = False
) -> Tuple[Optional[T], Optional[Dict[str, Any]]]:
    """Validate a JSON string against a schema.
    
    Args:
        json_string: JSON string to validate
        schema_class: Schema class to validate against
        raise_exception: Whether to raise an exception on validation error
        
    Returns:
        Tuple[Optional[T], Optional[Dict[str, Any]]]: Tuple of (validated model, errors)
    """
    try:
        data = json.loads(json_string)
        return validate_data(data, schema_class, raise_exception)
    except json.JSONDecodeError as e:
        error = {"json_error": str(e)}
        if raise_exception:
            raise ValueError(f"Invalid JSON: {e}")
        return None, error 