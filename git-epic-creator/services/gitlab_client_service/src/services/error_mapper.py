"""Error mapping for GitLab exceptions to unified error responses."""

from typing import Dict, Any, Optional
import structlog
from gitlab.exceptions import (
    GitlabAuthenticationError,
    GitlabGetError,
    GitlabCreateError,
    GitlabUpdateError,
    GitlabDeleteError,
    GitlabListError,
    GitlabError,
    GitlabHttpError
)

logger = structlog.get_logger(__name__)


class ErrorMapper(Exception):
    """
    Custom exception class for mapped errors.
    
    Can be raised and caught like a normal exception, but also provides
    structured error response formatting.
    """
    
    def __init__(
        self,
        code: str,
        message: str,
        gitlab_status: Optional[int] = None,
        details: Optional[Dict[str, Any]] = None
    ):
        """
        Initialize error mapper.
        
        Args:
            code: Error code (e.g., 'unauthorized', 'not_found')
            message: Human-readable error message
            gitlab_status: HTTP status code from GitLab (if applicable)
            details: Additional error details
        """
        self.code = code
        self.message = message
        self.gitlab_status = gitlab_status
        self.details = details
        super().__init__(message)
    
    def to_dict(self) -> Dict[str, Any]:
        """
        Convert error to dict format for API responses.
        
        Returns:
            Dictionary with error structure
        """
        error_dict = {
            "error": {
                "code": self.code,
                "message": self.message,
                "gitlab_status": self.gitlab_status
            }
        }
        
        if self.details:
            error_dict["error"]["details"] = self.details
        
        return error_dict


def map_gitlab_error(error: Exception) -> Dict[str, Any]:
    """
    Map GitLab exceptions to unified error response format.
    
    Args:
        error: Exception from python-gitlab or standard Python exception
        
    Returns:
        Dictionary with structured error information
    """
    # Extract status code if available
    gitlab_status = getattr(error, 'response_code', None)
    
    # Authentication errors
    if isinstance(error, GitlabAuthenticationError):
        return ErrorMapper(
            code="unauthorized",
            message="GitLab authentication failed. Check access token.",
            gitlab_status=gitlab_status
        ).to_dict()
    
    # HTTP errors with specific status codes
    if isinstance(error, (GitlabHttpError, GitlabGetError, GitlabCreateError, 
                         GitlabUpdateError, GitlabListError)):
        
        # Rate limiting (429)
        if gitlab_status == 429:
            return ErrorMapper(
                code="rate_limited",
                message="GitLab rate limit exceeded. Please retry later.",
                gitlab_status=gitlab_status
            ).to_dict()
        
        # Not found (404)
        if gitlab_status == 404:
            return ErrorMapper(
                code="not_found",
                message="GitLab resource not found.",
                gitlab_status=gitlab_status
            ).to_dict()
        
        # Forbidden (403)
        if gitlab_status == 403:
            return ErrorMapper(
                code="forbidden",
                message="Access to GitLab resource forbidden.",
                gitlab_status=gitlab_status
            ).to_dict()
        
        # Conflict (409)
        if gitlab_status == 409:
            return ErrorMapper(
                code="conflict",
                message="GitLab resource conflict.",
                gitlab_status=gitlab_status
            ).to_dict()
        
        # Service unavailable (503)
        if gitlab_status == 503:
            return ErrorMapper(
                code="gitlab_unavailable",
                message="GitLab service temporarily unavailable.",
                gitlab_status=gitlab_status
            ).to_dict()
        
        # Specific operation errors
        if isinstance(error, GitlabCreateError):
            return ErrorMapper(
                code="gitlab_create_failed",
                message=f"GitLab create operation failed: {str(error)}",
                gitlab_status=gitlab_status
            ).to_dict()
        
        if isinstance(error, GitlabUpdateError):
            return ErrorMapper(
                code="gitlab_update_failed",
                message=f"GitLab update operation failed: {str(error)}",
                gitlab_status=gitlab_status
            ).to_dict()
        
        if isinstance(error, GitlabListError):
            return ErrorMapper(
                code="gitlab_list_failed",
                message=f"GitLab list operation failed: {str(error)}",
                gitlab_status=gitlab_status
            ).to_dict()
    
    # Generic GitLab errors
    if isinstance(error, GitlabError):
        logger.error("GitLab error occurred", error=str(error), error_type=type(error).__name__)
        return ErrorMapper(
            code="gitlab_error",
            message=f"GitLab error: {str(error)}",
            gitlab_status=gitlab_status
        ).to_dict()
    
    # Unexpected errors
    logger.error("Unexpected error", error=str(error), error_type=type(error).__name__)
    return ErrorMapper(
        code="internal_error",
        message=f"Internal error: {str(error)}",
        gitlab_status=None
    ).to_dict()


