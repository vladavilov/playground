"""Centralized error response formatting and handling."""

import re
from typing import Optional
from fastapi import FastAPI, HTTPException, Request
from fastapi.responses import JSONResponse
from fastapi.exceptions import RequestValidationError
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
    
    def format_http_exception(self, error: HTTPException) -> JSONResponse:
        """Format FastAPI HTTPException preserving status code and headers."""
        sanitized_message = self._sanitize_error_message(str(error.detail))
        detail = self._create_error_detail("HTTP error", sanitized_message)

        response_data = {
            "status": "error",
            "detail": detail,
        }

        # Preserve headers like WWW-Authenticate if provided
        headers = getattr(error, "headers", None) or {}
        logger.warning(
            "HTTPException handled",
            status_code=error.status_code,
            message=sanitized_message,
        )
        return JSONResponse(status_code=error.status_code, content=response_data, headers=headers)

    def format_validation_error(self, error: RequestValidationError) -> JSONResponse:
        """Format request validation errors into a concise, consistent structure."""
        # Build a concise summary; avoid leaking raw payloads
        sanitized_message = self._sanitize_error_message("Request validation failed")
        detail = self._create_error_detail("Validation error", sanitized_message)

        response_data = {
            "status": "error",
            "detail": detail,
        }

        logger.warning("Request validation error", errors=error.errors())
        return JSONResponse(status_code=422, content=response_data)

    def register_exception_handlers(self, app: FastAPI) -> None:
        """Register global exception handlers on a FastAPI app."""

        @app.exception_handler(HTTPException)
        def _http_exception_handler(_: Request, exc: HTTPException):  # type: ignore[override]
            return self.format_http_exception(exc)

        @app.exception_handler(RequestValidationError)
        def _validation_exception_handler(_: Request, exc: RequestValidationError):  # type: ignore[override]
            return self.format_validation_error(exc)

        @app.exception_handler(neo4j.exceptions.Neo4jError)
        def _neo4j_exception_handler(_: Request, exc: neo4j.exceptions.Neo4jError):  # type: ignore[override]
            return self.format_neo4j_error(exc)

        @app.exception_handler(Exception)
        def _generic_exception_handler(_: Request, exc: Exception):  # type: ignore[override]
            return self.format_generic_error(exc)

    
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