"""Centralized error response formatting and handling."""

import re
from typing import Optional
from fastapi.responses import JSONResponse
import neo4j
import structlog

logger = structlog.get_logger(__name__)


class ErrorHandler:
    """Format common error responses."""

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