"""Helper functions for generating user-friendly error tips."""

from typing import Optional, Tuple
import structlog
from gitlab.exceptions import (
    GitlabAuthenticationError,
    GitlabGetError,
    GitlabError
)

logger = structlog.get_logger(__name__)


def get_error_tip_for_gitlab_exception(error: Exception) -> Tuple[str, Optional[str]]:
    """
    Map GitLab exception to user-friendly error message and actionable tip.
    
    Args:
        error: GitLab exception
        
    Returns:
        Tuple of (error_message, error_tip)
    """
    error_message = str(error)
    error_tip = None
    
    # Authentication errors (401)
    if isinstance(error, GitlabAuthenticationError):
        error_message = "GitLab authentication failed"
        error_tip = "TIP: Re-authenticate with GitLab SSO by clicking the 'Connect GitLab' button in the connections panel."
        logger.info("Authentication error detected", error=str(error))
    
    # HTTP errors with specific status codes
    elif isinstance(error, GitlabGetError):
        status_code = getattr(error, 'response_code', None)
        
        if status_code == 401:
            error_message = "GitLab authentication failed"
            error_tip = "TIP: Re-authenticate with GitLab SSO by clicking the 'Connect GitLab' button in the connections panel."
        
        elif status_code == 403:
            error_message = "Access denied to GitLab resource"
            error_tip = "TIP: Your GitLab token may not have sufficient permissions. Try re-authenticating or contact your GitLab administrator."
        
        elif status_code == 404:
            error_message = "GitLab project or resource not found"
            error_tip = "TIP: Please verify that the project ID is correct and that you have access to this project."
        
        elif status_code == 429:
            error_message = "GitLab API rate limit exceeded"
            error_tip = "TIP: GitLab has rate-limited your requests. Please wait a few minutes and try again."
        
        elif status_code == 500 or status_code == 502 or status_code == 503:
            error_message = f"GitLab server error ({status_code})"
            error_tip = "TIP: GitLab is experiencing issues. Please try again in a few minutes."
        
        else:
            error_message = f"GitLab API error ({status_code}): {str(error)}"
        
        logger.info("GitLab HTTP error detected", status_code=status_code, error=str(error))
    
    # Generic GitLab errors
    elif isinstance(error, GitlabError):
        error_message = f"GitLab error: {str(error)}"
        error_tip = "TIP: Please check your GitLab connection and try again."
        logger.info("Generic GitLab error detected", error=str(error))
    
    # Unknown errors
    else:
        error_message = f"Unexpected error: {str(error)}"
        logger.warning("Unknown error type", error=str(error), error_type=type(error).__name__)
    
    return (error_message, error_tip)

