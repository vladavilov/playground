"""
Tests for Azure token provider.
"""
import pytest
from unittest.mock import AsyncMock, patch, MagicMock
from datetime import datetime, timezone, timedelta
import httpx
from utils.azure_token_provider import AzureTokenProvider, TokenInfo


class TestTokenInfo:
    """Test TokenInfo dataclass."""

    def test_token_info_initialization(self):
        """Test TokenInfo initialization with all fields."""
        expires_at = datetime.now(timezone.utc) + timedelta(hours=1)
        token = TokenInfo(
            access_token="test_token",
            expires_at=expires_at,
            token_type="Bearer"
        )
        
        assert token.access_token == "test_token"
        assert token.expires_at == expires_at
        assert token.token_type == "Bearer"

    def test_token_info_default_token_type(self):
        """Test TokenInfo default token type."""
        expires_at = datetime.now(timezone.utc) + timedelta(hours=1)
        token = TokenInfo(
            access_token="test_token",
            expires_at=expires_at
        )
        
        assert token.token_type == "Bearer"

    def test_is_expired_false_when_token_valid(self):
        """Test is_expired returns False when token is valid."""
        expires_at = datetime.now(timezone.utc) + timedelta(hours=1)
        token = TokenInfo(
            access_token="test_token",
            expires_at=expires_at
        )
        
        assert not token.is_expired

    def test_is_expired_true_when_token_expired(self):
        """Test is_expired returns True when token is expired."""
        expires_at = datetime.now(timezone.utc) - timedelta(hours=1)
        token = TokenInfo(
            access_token="test_token",
            expires_at=expires_at
        )
        
        assert token.is_expired

    def test_is_expired_true_within_buffer(self):
        """Test is_expired returns True when token expires within 5 minute buffer."""
        expires_at = datetime.now(timezone.utc) + timedelta(minutes=3)
        token = TokenInfo(
            access_token="test_token",
            expires_at=expires_at
        )
        
        assert token.is_expired

    def test_authorization_header(self):
        """Test authorization header generation."""
        expires_at = datetime.now(timezone.utc) + timedelta(hours=1)
        token = TokenInfo(
            access_token="test_token",
            expires_at=expires_at,
            token_type="Bearer"
        )
        
        assert token.authorization_header == "Bearer test_token"


class TestAzureTokenProvider:
    """Test AzureTokenProvider class."""

    @pytest.fixture
    def mock_settings(self):
        """Mock Azure auth settings."""
        settings = MagicMock()
        settings.AZURE_CLIENT_ID = "test_client_id"
        settings.AZURE_CLIENT_SECRET = "test_client_secret"
        settings.SCOPE_NAME = "api://test/user_impersonation"
        settings.OPENAPI_TOKEN_URL = "https://test.com/token"
        return settings

    @pytest.fixture
    def provider(self, mock_settings):
        """Create AzureTokenProvider instance with mocked settings."""
        with patch('utils.azure_token_provider.get_azure_auth_settings', return_value=mock_settings):
            return AzureTokenProvider()

    @pytest.mark.asyncio
    async def test_init(self, provider, mock_settings):
        """Test AzureTokenProvider initialization."""
        assert provider.settings == mock_settings
        assert provider._client is None
        assert provider._token is None

    @pytest.mark.asyncio
    async def test_context_manager(self, provider):
        """Test AzureTokenProvider as async context manager."""
        with patch.object(provider, '_ensure_client', new_callable=AsyncMock) as mock_ensure:
            async with provider as p:
                assert p == provider
                mock_ensure.assert_called_once()

    @pytest.mark.asyncio
    async def test_ensure_client(self, provider):
        """Test _ensure_client initializes HTTP client."""
        await provider._ensure_client()
        
        assert provider._client is not None
        assert isinstance(provider._client, httpx.AsyncClient)

    @pytest.mark.asyncio
    async def test_ensure_client_only_once(self, provider):
        """Test _ensure_client doesn't recreate existing client."""
        await provider._ensure_client()
        client1 = provider._client
        
        await provider._ensure_client()
        client2 = provider._client
        
        assert client1 is client2

    @pytest.mark.asyncio
    async def test_close(self, provider):
        """Test close method cleans up resources."""
        await provider._ensure_client()
        assert provider._client is not None
        
        await provider.close()
        assert provider._client is None

    @pytest.mark.asyncio
    async def test_acquire_token_success(self, provider):
        """Test successful token acquisition."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "access_token": "test_access_token",
            "expires_in": 3600,
            "token_type": "Bearer"
        }

        with patch.object(provider, '_ensure_client', new_callable=AsyncMock):
            provider._client = AsyncMock()
            provider._client.post.return_value = mock_response

            token = await provider._acquire_token()

            assert token.access_token == "test_access_token"
            assert token.token_type == "Bearer"
            assert not token.is_expired

    @pytest.mark.asyncio
    async def test_acquire_token_http_error(self, provider):
        """Test token acquisition with HTTP error."""
        mock_response = MagicMock()
        mock_response.status_code = 400
        mock_response.json.return_value = {
            "error": "invalid_client",
            "error_description": "Invalid client credentials"
        }
        mock_response.text = "Bad Request"
        mock_response.request = MagicMock()

        with patch.object(provider, '_ensure_client', new_callable=AsyncMock):
            provider._client = AsyncMock()
            provider._client.post.return_value = mock_response

            with pytest.raises(httpx.HTTPStatusError) as exc_info:
                await provider._acquire_token()

            assert "invalid_client" in str(exc_info.value)

    @pytest.mark.asyncio
    async def test_acquire_token_invalid_response(self, provider):
        """Test token acquisition with invalid response format."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"invalid": "response"}
        mock_response.text = "Invalid response"

        with patch.object(provider, '_ensure_client', new_callable=AsyncMock):
            provider._client = AsyncMock()
            provider._client.post.return_value = mock_response

            with pytest.raises(ValueError) as exc_info:
                await provider._acquire_token()

            assert "Invalid token response format" in str(exc_info.value)

    @pytest.mark.asyncio
    async def test_get_access_token_cached(self, provider):
        """Test get_access_token returns cached token when valid."""
        expires_at = datetime.now(timezone.utc) + timedelta(hours=1)
        provider._token = TokenInfo(
            access_token="cached_token",
            expires_at=expires_at
        )

        token = await provider.get_access_token()
        assert token == "cached_token"

    @pytest.mark.asyncio
    async def test_get_access_token_acquire_new(self, provider):
        """Test get_access_token acquires new token when cache expired."""
        with patch.object(provider, '_acquire_token', new_callable=AsyncMock) as mock_acquire:
            expires_at = datetime.now(timezone.utc) + timedelta(hours=1)
            mock_acquire.return_value = TokenInfo(
                access_token="new_token",
                expires_at=expires_at
            )

            token = await provider.get_access_token()
            
            assert token == "new_token"
            mock_acquire.assert_called_once()

    @pytest.mark.asyncio
    async def test_get_authorization_header_cached(self, provider):
        """Test get_authorization_header returns cached token when valid."""
        expires_at = datetime.now(timezone.utc) + timedelta(hours=1)
        provider._token = TokenInfo(
            access_token="cached_token",
            expires_at=expires_at
        )

        headers = await provider.get_authorization_header()
        assert headers == {"Authorization": "Bearer cached_token"}

    @pytest.mark.asyncio
    async def test_get_authorization_header_acquire_new(self, provider):
        """Test get_authorization_header acquires new token when cache expired."""
        with patch.object(provider, '_acquire_token', new_callable=AsyncMock) as mock_acquire:
            expires_at = datetime.now(timezone.utc) + timedelta(hours=1)
            mock_acquire.return_value = TokenInfo(
                access_token="new_token",
                expires_at=expires_at
            )

            headers = await provider.get_authorization_header()
            
            assert headers == {"Authorization": "Bearer new_token"}
            mock_acquire.assert_called_once()

    def test_invalidate_token(self, provider):
        """Test invalidate_token clears cached token."""
        expires_at = datetime.now(timezone.utc) + timedelta(hours=1)
        provider._token = TokenInfo(
            access_token="cached_token",
            expires_at=expires_at
        )

        provider.invalidate_token()
        assert provider._token is None

    @pytest.mark.asyncio
    async def test_retry_on_connection_error(self, provider):
        """Test retry mechanism on connection errors."""
        with patch.object(provider, '_ensure_client', new_callable=AsyncMock):
            provider._client = AsyncMock()
            provider._client.post.side_effect = [
                httpx.ConnectError("Connection failed"),
                httpx.ConnectError("Connection failed"),
                MagicMock(status_code=200, json=lambda: {
                    "access_token": "retry_token",
                    "expires_in": 3600,
                    "token_type": "Bearer"
                })
            ]

            token = await provider._acquire_token()
            
            assert token.access_token == "retry_token"
            assert provider._client.post.call_count == 3

    @pytest.mark.asyncio
    async def test_concurrent_token_access(self, provider):
        """Test concurrent access to tokens is handled correctly."""
        call_count = 0
        
        async def mock_acquire():
            nonlocal call_count
            call_count += 1
            expires_at = datetime.now(timezone.utc) + timedelta(hours=1)
            return TokenInfo(
                access_token=f"token_{call_count}",
                expires_at=expires_at
            )

        with patch.object(provider, '_acquire_token', side_effect=mock_acquire):
            # Multiple concurrent calls should only result in one token acquisition
            import asyncio
            tokens = await asyncio.gather(
                provider.get_access_token(),
                provider.get_access_token(),
                provider.get_access_token()
            )

            # All should return the same token
            assert all(token == tokens[0] for token in tokens)
            # Only one acquisition should have occurred
            assert call_count == 1