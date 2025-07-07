import logging
import time
import traceback
from typing import Callable
from fastapi import Request, Response
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.types import ASGIApp

logger = logging.getLogger(__name__)


class ExceptionLoggingMiddleware(BaseHTTPMiddleware):
    """
    Middleware to catch and log unhandled exceptions with request context.
    """
    
    def __init__(self, app: ASGIApp):
        super().__init__(app)
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """
        Process the request and catch any unhandled exceptions.
        """
        start_time = time.time()
        
        try:
            response = await call_next(request)
            return response
        except Exception as exc:
            # Log the exception with full context
            process_time = time.time() - start_time
            
            logger.error(
                f"Unhandled exception in middleware: {type(exc).__name__}: {exc}",
                extra={
                    "request_method": request.method,
                    "request_url": str(request.url),
                    "request_path": request.url.path,
                    "request_query_params": dict(request.query_params),
                    "request_headers": dict(request.headers),
                    "process_time": process_time,
                    "client_ip": request.client.host if request.client else "unknown",
                },
                exc_info=True
            )
            
            # Return a generic error response
            return JSONResponse(
                status_code=500,
                content={"detail": "Internal server error"}
            )


class RequestResponseLoggingMiddleware(BaseHTTPMiddleware):
    """
    Middleware to log all requests and responses.
    """
    
    def __init__(self, app: ASGIApp):
        super().__init__(app)
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """
        Log incoming requests and outgoing responses.
        """
        start_time = time.time()
        
        # Log incoming request
        logger.info(
            f"Request: {request.method} {request.url.path}",
            extra={
                "request_method": request.method,
                "request_url": str(request.url),
                "request_path": request.url.path,
                "request_query_params": dict(request.query_params),
                "client_ip": request.client.host if request.client else "unknown",
            }
        )
        
        try:
            response = await call_next(request)
            
            # Calculate process time
            process_time = time.time() - start_time
            
            # Log response
            logger.info(
                f"Response: {response.status_code} {request.method} {request.url.path}",
                extra={
                    "request_method": request.method,
                    "request_path": request.url.path,
                    "response_status_code": response.status_code,
                    "process_time": process_time,
                    "client_ip": request.client.host if request.client else "unknown",
                }
            )
            
            return response
            
        except Exception as exc:
            # Log failed request
            process_time = time.time() - start_time
            
            logger.error(
                f"Request failed: {request.method} {request.url.path} - {type(exc).__name__}: {exc}",
                extra={
                    "request_method": request.method,
                    "request_path": request.url.path,
                    "process_time": process_time,
                    "client_ip": request.client.host if request.client else "unknown",
                },
                exc_info=True
            )
            
            # Re-raise the exception so it can be handled by other middleware
            raise 