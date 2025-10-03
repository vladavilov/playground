"""Tests for GitLab Client Service dependencies."""

import pytest
from unittest.mock import Mock, MagicMock
from fastapi import HTTPException, Request
from dependencies import (
    get_gitlab_access_token,
    get_gitlab_client,
    get_redis_client_dep,
)


def test_get_gitlab_access_token_success():
    """Test successful extraction of GitLab access token from header."""
    request = Mock(spec=Request)
    request.headers = {"GitLab-Access-Token": "test-gitlab-token-123"}
    
    token = get_gitlab_access_token(request)
    
    assert token == "test-gitlab-token-123"


def test_get_gitlab_access_token_missing():
    """Test error when GitLab access token header is missing."""
    request = Mock(spec=Request)
    request.headers = {}
    
    with pytest.raises(HTTPException) as exc_info:
        get_gitlab_access_token(request)
    
    assert exc_info.value.status_code == 401
    assert "GitLab-Access-Token header required" in str(exc_info.value.detail)


def test_get_gitlab_access_token_empty():
    """Test error when GitLab access token header is empty."""
    request = Mock(spec=Request)
    request.headers = {"GitLab-Access-Token": ""}
    
    with pytest.raises(HTTPException) as exc_info:
        get_gitlab_access_token(request)
    
    assert exc_info.value.status_code == 401
    assert "GitLab-Access-Token header required" in str(exc_info.value.detail)


def test_get_gitlab_client_creates_client(monkeypatch):
    """Test GitLab client creation with proper configuration."""
    from config import GitLabClientSettings
    
    settings = GitLabClientSettings(
        GITLAB_BASE_URL="http://test-gitlab.com",
        HTTP_TIMEOUT_SEC=60.0,
        HTTP_MAX_RETRIES=5
    )
    
    # Mock gitlab.Gitlab to avoid actual network calls
    mock_gitlab_class = MagicMock()
    mock_gitlab_instance = MagicMock()
    mock_gitlab_class.return_value = mock_gitlab_instance
    
    monkeypatch.setattr("dependencies.gitlab.Gitlab", mock_gitlab_class)
    
    token = "test-token"
    client = get_gitlab_client(token, settings)
    
    # Verify Gitlab was called with correct parameters
    mock_gitlab_class.assert_called_once()
    call_args = mock_gitlab_class.call_args
    
    assert call_args[1]["url"] == "http://test-gitlab.com"
    assert call_args[1]["private_token"] == "test-token"
    assert call_args[1]["timeout"] == 60.0
    assert call_args[1]["retry_transient_errors"] is True
    assert client == mock_gitlab_instance


def test_get_redis_client_dep_returns_client(monkeypatch):
    """Test Redis client dependency returns configured client from shared library."""
    mock_redis_client = MagicMock()
    
    def mock_get_redis_client(is_pubsub_client=False):
        return mock_redis_client
    
    # Mock the shared library's get_redis_client at the point where dependencies imports it
    monkeypatch.setattr("dependencies.get_redis_client", mock_get_redis_client)
    
    client = get_redis_client_dep()
    
    assert client == mock_redis_client


def test_gitlab_client_configuration_with_ssl_verify(monkeypatch):
    """Test GitLab client respects SSL verification settings."""
    from config import GitLabClientSettings
    
    settings = GitLabClientSettings(
        GITLAB_BASE_URL="https://secure-gitlab.com",
        GITLAB_VERIFY_SSL=True
    )
    
    mock_gitlab_class = MagicMock()
    mock_gitlab_instance = MagicMock()
    mock_gitlab_class.return_value = mock_gitlab_instance
    
    monkeypatch.setattr("dependencies.gitlab.Gitlab", mock_gitlab_class)
    
    token = "test-token"
    client = get_gitlab_client(token, settings)
    
    call_args = mock_gitlab_class.call_args
    assert call_args[1]["ssl_verify"] is True


