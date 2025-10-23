"""Shared utility functions for Celery task operations.

Provides reusable helpers for common Celery task patterns to eliminate code duplication
across services following DRY principles.
"""

from typing import Any, Optional
import structlog


def extract_auth_header(request: Any, logger: Optional[Any] = None, project_id: Optional[str] = None) -> str:
    """
    Extract and validate Authentication header from Celery task request.
    
    This function provides centralized authentication header extraction logic
    to prevent code duplication across multiple Celery task implementations.
    
    Args:
        request: Celery task request object (self.request in bound tasks)
        logger: Optional structlog logger for error logging
        project_id: Optional project ID for logging context
        
    Returns:
        Authentication header value (string)
        
    Raises:
        RuntimeError: If headers are None or Authentication header is missing
        
    Example:
        @celery_app.task(bind=True, name="my_task")
        def my_task(self, project_id: str):
            auth_header = extract_auth_header(
                request=self.request,
                logger=logger,
                project_id=project_id
            )
            # Use auth_header for HTTP calls...
    """
    log = logger or structlog.get_logger(__name__)
    
    # Build context for logging
    log_context = {"task_id": getattr(request, "id", "unknown")}
    if project_id:
        log_context["project_id"] = project_id
    
    # Check if headers exist
    if not request.headers:
        error_msg = (
            "Task headers are None - Authentication token required. "
            "Ensure task_protocol=2 is set in Celery config and worker is restarted."
        )
        log.error("celery_task_headers_missing", **log_context, error=error_msg)
        raise RuntimeError(error_msg)
    
    # Extract Authentication header
    auth_header = request.headers.get('Authentication')
    if not auth_header:
        error_msg = "Missing Authentication header in task"
        log.error(
            "celery_auth_header_missing",
            **log_context,
            headers=dict(request.headers),
            error=error_msg
        )
        raise RuntimeError(error_msg)
    
    # Log success for debugging
    log.info(
        "celery_auth_header_extracted",
        **log_context,
        token_length=len(auth_header)
    )
    
    return auth_header

