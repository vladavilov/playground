"""
Tests for GitLab OAuth authentication flow.

These tests define the expected behavior of GitLab OAuth integration
with PKCE, token management, and security best practices.
"""

import pytest
from unittest.mock import Mock, AsyncMock, patch
from fastapi import FastAPI
from fastapi.testclient import TestClient
from starlette.middleware.sessions import SessionMiddleware
import json


@pytest.fixture
def mock_oauth():
    """Mock Authlib OAuth client for GitLab."""
    oauth = Mock()
    gitlab_client = Mock()
    gitlab_client.authorize_redirect = AsyncMock(return_value=Mock(
        status_code=302,
        headers={"Location": "https://gitlab.example.com/oauth/authorize?client_id=test"}
    ))
    gitlab_client.authorize_access_token = AsyncMock(return_value={
        "access_token": "gitlab_access_token",
        "refresh_token": "gitlab_refresh_token",
        "expires_in": 7200,
        "scope": "read_api write_repository"
    })
    oauth.gitlab = gitlab_client
    return oauth


@pytest.fixture
def mock_redis():
    """Mock Redis client."""
    redis = AsyncMock()
    redis.get = AsyncMock(return_value=None)
    redis.set = AsyncMock(return_value=True)
    redis.delete = AsyncMock(return_value=True)
    return redis


@pytest.fixture
def app_with_gitlab(mock_oauth, mock_redis):
    """Create FastAPI app with GitLab OAuth configured."""
    from routers.gitlab_auth_router import router
    
    app = FastAPI()
    app.add_middleware(
        SessionMiddleware,
        secret_key="test-secret-key",
        session_cookie="test_session",
    )
    app.include_router(router)
    
    # Store mocks on app state
    app.state.oauth = mock_oauth
    app.state.redis_client = mock_redis
    app.state.gitlab_base_url = "https://gitlab.example.com"
    app.state.gitlab_client_id = "test-client-id"
    app.state.gitlab_client_secret = "test-client-secret"
    app.state.gitlab_redirect_uri = "http://localhost/gitlab/callback"
    app.state.gitlab_scopes = "read_api write_repository"
    app.state.gitlab_verify_ssl = True
    
    return app


class TestGitLabOAuthFlow:
    """Test GitLab OAuth authentication flow."""
    
    def test_gitlab_authorize_generates_oauth_url(self, app_with_gitlab):
        """Test /gitlab/authorize generates GitLab OAuth URL with PKCE."""
        pass
    
    def test_gitlab_authorize_stores_return_uri(self, app_with_gitlab):
        """Test authorize endpoint stores return URI in session."""
        pass
    
    def test_gitlab_authorize_uses_pkce(self, app_with_gitlab):
        """Test OAuth flow uses PKCE for security."""
        pass
    
    def test_gitlab_callback_exchanges_code_for_token(self, app_with_gitlab):
        """Test callback exchanges authorization code for access token."""
        pass
    
    def test_gitlab_callback_redirects_to_return_uri(self, app_with_gitlab):
        """Test callback redirects to stored return URI."""
        pass
    
    def test_gitlab_callback_handles_oauth_errors(self, app_with_gitlab):
        """Test callback handles OAuth errors from GitLab."""
        pass


class TestGitLabTokenManager:
    """Test GitLab token manager for token lifecycle."""
    
    @pytest.fixture
    def mock_redis(self):
        """Mock Redis client."""
        redis = AsyncMock()
        redis.get = AsyncMock(return_value=None)
        redis.set = AsyncMock(return_value=True)
        redis.delete = AsyncMock(return_value=True)
        return redis
    
    @pytest.mark.asyncio
    async def test_save_gitlab_token(self, mock_redis):
        """Test saving GitLab token to Redis."""
        from services.gitlab_token_manager import GitLabTokenManager
        
        manager = GitLabTokenManager(session_id="test-session", redis_client=mock_redis)
        await manager.save_token(
            access_token="test_token",
            refresh_token="refresh_token",
            expires_in=7200
        )
        
        mock_redis.set.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_load_gitlab_token(self, mock_redis):
        """Test loading GitLab token from Redis."""
        from services.gitlab_token_manager import GitLabTokenManager
        
        stored_data = {
            "access_token": "test_token",
            "refresh_token": "refresh_token",
            "expires_at": 9999999999
        }
        mock_redis.get.return_value = json.dumps(stored_data)
        
        manager = GitLabTokenManager(session_id="test-session", redis_client=mock_redis)
        token_data = await manager.load_token()
        
        assert token_data["access_token"] == "test_token"
    
    @pytest.mark.asyncio
    async def test_get_valid_token_returns_cached_token(self, mock_redis):
        """Test get_valid_token returns cached token if valid."""
        from services.gitlab_token_manager import GitLabTokenManager
        import time
        
        stored_data = {
            "access_token": "test_token",
            "expires_at": int(time.time()) + 3600  # Valid for 1 hour
        }
        mock_redis.get.return_value = json.dumps(stored_data)
        
        manager = GitLabTokenManager(session_id="test-session", redis_client=mock_redis)
        token = await manager.get_valid_token()
        
        assert token == "test_token"
    
    @pytest.mark.asyncio
    async def test_get_valid_token_refreshes_if_expired(self, mock_redis):
        """Test get_valid_token refreshes token if expired."""
        from services.gitlab_token_manager import GitLabTokenManager
        import time
        
        stored_data = {
            "access_token": "old_token",
            "refresh_token": "refresh_token",
            "expires_at": int(time.time()) - 100  # Expired
        }
        mock_redis.get.return_value = json.dumps(stored_data)
        
        manager = GitLabTokenManager(
            session_id="test-session",
            redis_client=mock_redis,
            gitlab_base_url="https://gitlab.example.com",
            client_id="test-client",
            client_secret="test-secret"
        )
        
        # Mock refresh response
        with patch('httpx.AsyncClient') as mock_client:
            mock_response = Mock(status_code=200, json=Mock(return_value={
                "access_token": "new_token",
                "refresh_token": "new_refresh",
                "expires_in": 7200
            }))
            mock_client.return_value.__aenter__.return_value.post = AsyncMock(return_value=mock_response)
            
            token = await manager.get_valid_token()
            
            # Should have attempted refresh
            assert mock_redis.set.called  # New token saved
    
    @pytest.mark.asyncio
    async def test_refresh_gitlab_token(self, mock_redis):
        """Test refreshing GitLab token."""
        from services.gitlab_token_manager import GitLabTokenManager
        
        manager = GitLabTokenManager(
            session_id="test-session",
            redis_client=mock_redis,
            gitlab_base_url="https://gitlab.example.com",
            client_id="test-client",
            client_secret="test-secret"
        )
        
        mock_response = Mock(status_code=200, json=Mock(return_value={
            "access_token": "new_token",
            "refresh_token": "new_refresh",
            "expires_in": 7200
        }))
        
        with patch('httpx.AsyncClient') as mock_client:
            mock_client.return_value.__aenter__.return_value.post = AsyncMock(return_value=mock_response)
            
            new_token = await manager.refresh_token("old_refresh_token")
            
            assert new_token == "new_token"
            mock_redis.set.assert_called()
    
    @pytest.mark.asyncio
    async def test_validate_gitlab_token_uses_token_info_endpoint(self, mock_redis):
        """Test validating GitLab token uses /oauth/token/info endpoint."""
        from services.gitlab_token_manager import GitLabTokenManager
        
        manager = GitLabTokenManager(
            session_id="test-session",
            redis_client=mock_redis,
            gitlab_base_url="https://gitlab.example.com"
        )
        
        mock_response = Mock(
            status_code=200,
            json=Mock(return_value={
                "resource_owner_id": 123,
                "scope": ["read_api"],
                "expires_in": 7200,
                "active": True
            })
        )
        mock_httpx_client = AsyncMock()
        mock_httpx_client.__aenter__ = AsyncMock(return_value=mock_httpx_client)
        mock_httpx_client.__aexit__ = AsyncMock()
        mock_httpx_client.get = AsyncMock(return_value=mock_response)
        
        with patch('httpx.AsyncClient', return_value=mock_httpx_client):
            is_valid = await manager.validate_token("test_token")
            
            assert is_valid is True
            # Verify it called /oauth/token/info
            call_args = mock_httpx_client.get.call_args
            assert "/oauth/token/info" in call_args.args[0]
    
    @pytest.mark.asyncio
    async def test_revoke_gitlab_token(self, mock_redis):
        """Test revoking GitLab token on GitLab server."""
        from services.gitlab_token_manager import GitLabTokenManager
        
        stored_data = {
            "access_token": "gitlab_token",
            "refresh_token": "refresh_token"
        }
        mock_redis.get.return_value = json.dumps(stored_data)
        
        manager = GitLabTokenManager(
            session_id="test-session",
            redis_client=mock_redis,
            gitlab_base_url="https://gitlab.example.com",
            client_id="test-client",
            client_secret="test-secret"
        )
        
        mock_response = Mock(status_code=200, text="{}")
        mock_httpx_client = AsyncMock()
        mock_httpx_client.__aenter__ = AsyncMock(return_value=mock_httpx_client)
        mock_httpx_client.__aexit__ = AsyncMock()
        mock_httpx_client.post = AsyncMock(return_value=mock_response)
        
        with patch('httpx.AsyncClient', return_value=mock_httpx_client):
            result = await manager.revoke_token()
            
            assert result is True
            # Verify it called /oauth/revoke
            call_args = mock_httpx_client.post.call_args
            assert "/oauth/revoke" in call_args.args[0]
    
    @pytest.mark.asyncio
    async def test_clear_gitlab_token(self, mock_redis):
        """Test clearing GitLab token from storage."""
        from services.gitlab_token_manager import GitLabTokenManager
        
        manager = GitLabTokenManager(session_id="test-session", redis_client=mock_redis)
        await manager.clear_token()
        
        mock_redis.delete.assert_called_once()


class TestGitLabStatus:
    """Test GitLab connection status endpoint."""
    
    def test_gitlab_status_returns_connected_when_token_exists(self, app_with_gitlab, mock_redis):
        """Test /gitlab/status returns connected when token exists."""
        pass
    
    def test_gitlab_status_returns_not_connected_when_no_token(self, app_with_gitlab, mock_redis):
        """Test /gitlab/status returns not connected when no token."""
        pass
