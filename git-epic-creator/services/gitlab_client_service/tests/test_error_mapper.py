"""Tests for error mapper functionality."""

import pytest
from unittest.mock import Mock
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
from services.error_mapper import ErrorMapper, map_gitlab_error


def test_map_authentication_error():
    """Test mapping of GitLab authentication errors."""
    error = GitlabAuthenticationError("Invalid token")
    
    result = map_gitlab_error(error)
    
    assert result["error"]["code"] == "unauthorized"
    assert "authentication" in result["error"]["message"].lower()
    assert result["error"]["gitlab_status"] is None


def test_map_get_error_404():
    """Test mapping of GitLab 404 GET errors."""
    response = Mock()
    response.status_code = 404
    response.content = b"Not found"
    error = GitlabGetError(response_body="Not found", response_code=404)
    error.response_code = 404
    
    result = map_gitlab_error(error)
    
    assert result["error"]["code"] == "not_found"
    assert result["error"]["gitlab_status"] == 404


def test_map_get_error_403():
    """Test mapping of GitLab 403 GET errors."""
    error = GitlabGetError(response_body="Forbidden", response_code=403)
    error.response_code = 403
    
    result = map_gitlab_error(error)
    
    assert result["error"]["code"] == "forbidden"
    assert result["error"]["gitlab_status"] == 403


def test_map_create_error():
    """Test mapping of GitLab CREATE errors."""
    error = GitlabCreateError(response_body="Creation failed", response_code=400)
    error.response_code = 400
    
    result = map_gitlab_error(error)
    
    assert result["error"]["code"] == "gitlab_create_failed"
    assert result["error"]["gitlab_status"] == 400


def test_map_update_error():
    """Test mapping of GitLab UPDATE errors."""
    error = GitlabUpdateError(response_body="Update failed", response_code=422)
    error.response_code = 422
    
    result = map_gitlab_error(error)
    
    assert result["error"]["code"] == "gitlab_update_failed"
    assert result["error"]["gitlab_status"] == 422


def test_map_rate_limit_error():
    """Test mapping of GitLab rate limit errors (429)."""
    error = GitlabHttpError(response_body="Rate limit exceeded", response_code=429)
    error.response_code = 429
    
    result = map_gitlab_error(error)
    
    assert result["error"]["code"] == "rate_limited"
    assert result["error"]["gitlab_status"] == 429
    assert "rate limit" in result["error"]["message"].lower()


def test_map_service_unavailable():
    """Test mapping of GitLab service unavailable errors (503)."""
    error = GitlabHttpError(response_body="Service unavailable", response_code=503)
    error.response_code = 503
    
    result = map_gitlab_error(error)
    
    assert result["error"]["code"] == "gitlab_unavailable"
    assert result["error"]["gitlab_status"] == 503


def test_map_generic_gitlab_error():
    """Test mapping of generic GitLab errors without status code."""
    error = GitlabError("Something went wrong")
    
    result = map_gitlab_error(error)
    
    assert result["error"]["code"] == "gitlab_error"
    assert result["error"]["gitlab_status"] is None
    assert "Something went wrong" in result["error"]["message"]


def test_map_unexpected_exception():
    """Test mapping of unexpected exceptions."""
    error = ValueError("Unexpected error")
    
    result = map_gitlab_error(error)
    
    assert result["error"]["code"] == "internal_error"
    assert result["error"]["gitlab_status"] is None
    assert "Unexpected error" in result["error"]["message"]


def test_error_mapper_as_exception():
    """Test ErrorMapper can be raised as an exception."""
    mapper = ErrorMapper(
        code="test_error",
        message="Test error message",
        gitlab_status=400
    )
    
    with pytest.raises(ErrorMapper) as exc_info:
        raise mapper
    
    assert exc_info.value.code == "test_error"
    assert exc_info.value.message == "Test error message"
    assert exc_info.value.gitlab_status == 400


def test_error_mapper_to_dict():
    """Test ErrorMapper converts to dict correctly."""
    mapper = ErrorMapper(
        code="validation_error",
        message="Invalid input",
        gitlab_status=400,
        details={"field": "title"}
    )
    
    result = mapper.to_dict()
    
    assert result["error"]["code"] == "validation_error"
    assert result["error"]["message"] == "Invalid input"
    assert result["error"]["gitlab_status"] == 400
    assert result["error"]["details"] == {"field": "title"}


def test_map_list_error():
    """Test mapping of GitLab LIST errors."""
    error = GitlabListError(response_body="List failed", response_code=400)
    error.response_code = 400
    
    result = map_gitlab_error(error)
    
    assert result["error"]["code"] == "gitlab_list_failed"
    assert result["error"]["gitlab_status"] == 400


def test_map_conflict_error():
    """Test mapping of GitLab conflict errors (409)."""
    error = GitlabHttpError(response_body="Conflict", response_code=409)
    error.response_code = 409
    
    result = map_gitlab_error(error)
    
    assert result["error"]["code"] == "conflict"
    assert result["error"]["gitlab_status"] == 409


