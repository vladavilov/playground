"""
Centralizes error response formatting and handling.
"""

import re
from typing import Dict, Any, List, Optional
from fastapi.responses import JSONResponse
import neo4j
import structlog

logger = structlog.get_logger(__name__)


class ErrorHandler:
    """Handles error formatting and responses."""

    def __init__(self):
        """Initialize the error handler."""
        logger.info("Error handler initialized")

    def format_neo4j_error(self, error: neo4j.exceptions.Neo4jError) -> JSONResponse:
        """
        Format Neo4j-specific errors.
        
        Args:
            error: Neo4j exception instance
            
        Returns:
            JSONResponse: Formatted error response
        """
        error_type = self._determine_error_type(error)
        sanitized_message = self._sanitize_error_message(str(error))
        detail = self._create_error_detail(error_type, sanitized_message)
        
        response_data = {
            "status": "error",
            "detail": detail
        }
        
        logger.error("Neo4j error occurred", error_type=error_type, message=sanitized_message)
        return JSONResponse(status_code=500, content=response_data)
    
    def format_generic_error(self, error: Exception) -> JSONResponse:
        """
        Format generic exceptions.
        
        Args:
            error: Exception instance
            
        Returns:
            JSONResponse: Formatted error response
        """
        sanitized_message = self._sanitize_error_message(str(error))
        detail = self._create_error_detail("Unexpected error", sanitized_message)
        
        response_data = {
            "status": "error",
            "detail": detail
        }
        
        logger.error("Generic error occurred", message=sanitized_message)
        return JSONResponse(status_code=500, content=response_data)
    
    def format_schema_error(self, message: str, context: str) -> JSONResponse:
        """
        Format schema-related errors.
        
        Args:
            message: Error message
            context: Additional context
            
        Returns:
            JSONResponse: Formatted error response
        """
        detail = self._create_error_detail(message, context)
        
        response_data = {
            "status": "error",
            "detail": detail
        }
        
        logger.error("Schema error occurred", message=message, context=context)
        return JSONResponse(status_code=500, content=response_data)
    
    def format_success_response(self, message: str, data: Dict[str, Any]) -> JSONResponse:
        """
        Format successful responses.
        
        Args:
            message: Success message
            data: Response data
            
        Returns:
            JSONResponse: Formatted success response
        """
        response_data = {
            "status": message,
            **data
        }
        
        logger.info("Success response created", message=message)
        return JSONResponse(status_code=200, content=response_data)
    
    def format_validation_error(self, message: str, errors: List[Dict[str, str]]) -> JSONResponse:
        """
        Format validation errors.
        
        Args:
            message: Validation error message
            errors: List of validation errors
            
        Returns:
            JSONResponse: Formatted validation error response
        """
        response_data = {
            "status": "error",
            "detail": message,
            "validation_errors": errors
        }
        
        logger.error("Validation error occurred", message=message, errors=errors)
        return JSONResponse(status_code=400, content=response_data)
    
    def format_timeout_error(self, message: str, timeout: float) -> JSONResponse:
        """
        Format timeout errors.
        
        Args:
            message: Timeout error message
            timeout: Timeout value
            
        Returns:
            JSONResponse: Formatted timeout error response
        """
        detail = self._create_error_detail(message, f"Timeout: {timeout}s")
        
        response_data = {
            "status": "error",
            "detail": detail
        }
        
        logger.error("Timeout error occurred", message=message, timeout=timeout)
        return JSONResponse(status_code=504, content=response_data)
    
    def format_health_check_error(self, message: str) -> JSONResponse:
        """
        Format health check errors.
        
        Args:
            message: Health check error message
            
        Returns:
            JSONResponse: Formatted health check error response
        """
        response_data = {
            "status": "unhealthy",
            "detail": message
        }
        
        logger.error("Health check error occurred", message=message)
        return JSONResponse(status_code=503, content=response_data)
    
    def is_retryable_error(self, error: Exception) -> bool:
        """
        Check if an error is retryable.
        
        Args:
            error: Exception instance
            
        Returns:
            bool: True if error is retryable
        """
        retryable_errors = (
            neo4j.exceptions.TransientError,
            neo4j.exceptions.ServiceUnavailable
        )
        
        is_retryable = isinstance(error, retryable_errors)
        logger.debug("Error retry check", error_type=type(error).__name__, retryable=is_retryable)
        return is_retryable
    
    def get_error_severity(self, error: Exception) -> str:
        """
        Get error severity classification.
        
        Args:
            error: Exception instance
            
        Returns:
            str: Error severity level (high, medium, low)
        """
        high_severity = (neo4j.exceptions.AuthError, neo4j.exceptions.ClientError)
        medium_severity = (neo4j.exceptions.ServiceUnavailable, neo4j.exceptions.TransientError)
        
        if isinstance(error, high_severity):
            severity = "high"
        elif isinstance(error, medium_severity):
            severity = "medium"
        else:
            severity = "low"
        
        logger.debug("Error severity determined", error_type=type(error).__name__, severity=severity)
        return severity
    
    def _create_error_detail(self, message: str, context: Optional[str]) -> str:
        """
        Create error detail message.
        
        Args:
            message: Main error message
            context: Additional context
            
        Returns:
            str: Combined error detail
        """
        if context and context.strip():
            return f"{message}: {context}"
        return message
    
    def _determine_error_type(self, error: Exception) -> str:
        """
        Determine error type from exception.
        
        Args:
            error: Exception instance
            
        Returns:
            str: Error type description
        """
        if isinstance(error, neo4j.exceptions.ServiceUnavailable):
            return "Neo4j service unavailable"
        elif isinstance(error, neo4j.exceptions.AuthError):
            return "Neo4j authentication failed"
        elif isinstance(error, neo4j.exceptions.ClientError):
            return "Neo4j client error"
        elif isinstance(error, neo4j.exceptions.TransientError):
            return "Neo4j transient error"
        elif isinstance(error, neo4j.exceptions.Neo4jError):
            return "Neo4j error"
        else:
            return "Unexpected error"
    
    def _sanitize_error_message(self, message: str) -> str:
        """
        Sanitize error message to remove sensitive information.
        
        Args:
            message: Original error message
            
        Returns:
            str: Sanitized error message
        """
        # Remove sensitive patterns
        sensitive_patterns = [
            r'password=\S+',
            r'token=\S+',
            r'key=\S+',
            r'secret=\S+'
        ]
        
        sanitized = message
        for pattern in sensitive_patterns:
            sanitized = re.sub(pattern, '[REDACTED]', sanitized, flags=re.IGNORECASE)
        
        return sanitized 