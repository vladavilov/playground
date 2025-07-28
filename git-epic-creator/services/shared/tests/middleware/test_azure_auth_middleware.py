"""
Tests for Azure authentication middleware.
"""

import pytest
from unittest.mock import Mock, AsyncMock
from fastapi import HTTPException
from fastapi_azure_auth import SingleTenantAzureAuthorizationCodeBearer
from fastapi_azure_auth.user import User
from fastapi_azure_auth.openid_config import OpenIdConfig
from middleware.azure_auth_middleware import AzureAuthMiddleware, get_current_user, get_current_active_user, require_roles, create_azure_scheme


@pytest.fixture
def mock_azure_scheme():
    """Mock Azure authentication scheme."""
    scheme = Mock(spec=SingleTenantAzureAuthorizationCodeBearer)
    scheme.openid_config = Mock()
    scheme.openid_config.load_config = AsyncMock()
    return scheme

@pytest.fixture
def azure_auth_middleware(mock_azure_scheme):
    """Create Azure auth middleware instance."""
    return AzureAuthMiddleware(mock_azure_scheme)

def test_azure_auth_middleware_init(mock_azure_scheme):
    """Test Azure auth middleware initialization."""
    middleware = AzureAuthMiddleware(mock_azure_scheme)
    assert middleware.azure_scheme is mock_azure_scheme

@pytest.mark.asyncio
async def test_load_openid_config_success(azure_auth_middleware):
    """Test successful OpenID config loading."""
    await azure_auth_middleware.load_openid_config()
    azure_auth_middleware.azure_scheme.openid_config.load_config.assert_called_once()

@pytest.mark.asyncio
async def test_load_openid_config_failure(azure_auth_middleware):
    """Test OpenID config loading failure."""
    azure_auth_middleware.azure_scheme.openid_config.load_config.side_effect = Exception("Config load failed")
    with pytest.raises(Exception, match="Config load failed"):
        await azure_auth_middleware.load_openid_config()

@pytest.fixture
def mock_user():
    """Mock authenticated user."""
    return User(
        claims={},
        preferred_username="testuser@example.com",
        roles=["User"],
        aud="test-audience",
        tid="test-tenant-id",
        access_token="test-token",
        is_guest=False,
        iat=1537231048,
        nbf=1537231048,
        exp=1537234948,
        iss="test-issuer",
        aio="test-aio",
        sub="test-subject",
        oid="test-object-id",
        uti="test-uti",
        rh="test-rh",
        ver="2.0"
    )

@pytest.mark.asyncio
async def test_get_current_user_success(mock_user):
    """Test successful user authentication."""
    mock_azure_scheme = AsyncMock(return_value=mock_user)
    result = await get_current_user(mock_azure_scheme)
    assert result == mock_user
    mock_azure_scheme.assert_called_once()

@pytest.mark.asyncio
async def test_get_current_user_http_exception():
    """Test user authentication HTTP exception."""
    mock_azure_scheme = AsyncMock(side_effect=HTTPException(status_code=401, detail="Unauthorized"))
    with pytest.raises(HTTPException) as exc_info:
        await get_current_user(mock_azure_scheme)
    assert exc_info.value.status_code == 401
    assert exc_info.value.detail == "Unauthorized"

@pytest.mark.asyncio
async def test_get_current_user_generic_exception():
    """Test user authentication generic exception."""
    mock_azure_scheme = AsyncMock(side_effect=Exception("Generic error"))
    with pytest.raises(HTTPException) as exc_info:
        await get_current_user(mock_azure_scheme)
    assert exc_info.value.status_code == 401
    assert exc_info.value.detail == "Authentication failed"

@pytest.fixture
def mock_active_user():
    """Mock active authenticated user."""
    return User(
        claims={},
        preferred_username="activeuser@example.com",
        roles=["User"],
        aud="test-audience",
        tid="test-tenant-id",
        access_token="test-token",
        is_guest=False,
        iat=1537231048,
        nbf=1537231048,
        exp=1537234948,
        iss="test-issuer",
        aio="test-aio",
        sub="test-subject",
        oid="test-object-id",
        uti="test-uti",
        rh="test-rh",
        ver="2.0"
    )

@pytest.fixture
def mock_guest_user():
    """Mock guest user."""
    return User(
        claims={},
        preferred_username="guestuser@example.com",
        roles=["Guest"],
        aud="test-audience",
        tid="test-tenant-id",
        access_token="test-token",
        is_guest=True,
        iat=1537231048,
        nbf=1537231048,
        exp=1537234948,
        iss="test-issuer",
        aio="test-aio",
        sub="test-subject",
        oid="test-object-id",
        uti="test-uti",
        rh="test-rh",
        ver="2.0"
    )

@pytest.mark.asyncio
async def test_get_current_active_user_success(mock_active_user):
    """Test successful active user validation."""
    result = await get_current_active_user(mock_active_user)
    assert result == mock_active_user

@pytest.mark.asyncio
async def test_get_current_active_user_guest_rejected(mock_guest_user):
    """Test guest user rejection."""
    with pytest.raises(HTTPException) as exc_info:
        await get_current_active_user(mock_guest_user)
    assert exc_info.value.status_code == 403
    assert exc_info.value.detail == "Guest users are not allowed"

@pytest.fixture
def mock_admin_user():
    """Mock user with admin role."""
    return User(
        claims={},
        preferred_username="admin@example.com",
        roles=["Admin", "User"],
        aud="test-audience",
        tid="test-tenant-id",
        access_token="test-token",
        is_guest=False,
        iat=1537231048,
        nbf=1537231048,
        exp=1537234948,
        iss="test-issuer",
        aio="test-aio",
        sub="test-subject",
        oid="test-object-id",
        uti="test-uti",
        rh="test-rh",
        ver="2.0"
    )

@pytest.fixture
def mock_regular_user():
    """Mock user with regular role."""
    return User(
        claims={},
        preferred_username="user@example.com",
        roles=["User"],
        aud="test-audience",
        tid="test-tenant-id",
        access_token="test-token",
        is_guest=False,
        iat=1537231048,
        nbf=1537231048,
        exp=1537234948,
        iss="test-issuer",
        aio="test-aio",
        sub="test-subject",
        oid="test-object-id",
        uti="test-uti",
        rh="test-rh",
        ver="2.0"
    )

@pytest.mark.asyncio
async def test_require_roles_success(mock_admin_user):
    """Test successful role validation."""
    validate_admin = require_roles(["Admin"])
    result = await validate_admin(mock_admin_user)
    assert result == mock_admin_user

@pytest.mark.asyncio
async def test_require_roles_failure(mock_regular_user):
    """Test role validation failure."""
    validate_admin = require_roles(["Admin"])
    with pytest.raises(HTTPException) as exc_info:
        await validate_admin(mock_regular_user)
    assert exc_info.value.status_code == 403
    assert "Admin" in exc_info.value.detail

@pytest.mark.asyncio
async def test_require_multiple_roles_success(mock_admin_user):
    """Test multiple role validation success."""
    validate_roles = require_roles(["Admin", "SuperUser"])
    result = await validate_roles(mock_admin_user)
    assert result == mock_admin_user


class TestCreateAzureScheme:
    """Test cases for create_azure_scheme function."""

    def test_create_azure_scheme_with_custom_openid_config(self):
        """Test creating Azure scheme with custom OpenID configuration."""
        # Arrange
        app_client_id = "test-client-id"
        tenant_id = "12345678-1234-1234-1234-123456789abc"
        scopes = {"api://test": "test"}
        openapi_authorization_url = "https://login.microsoftonline.com/test/oauth2/v2.0/authorize"  
        openapi_token_url = "https://login.microsoftonline.com/test/oauth2/v2.0/token"
        openid_config_url = "https://login.microsoftonline.com/test/v2.0/.well-known/openid-configuration"

        # Act
        result = create_azure_scheme(
            app_client_id=app_client_id,
            tenant_id=tenant_id,
            scopes=scopes,
            openapi_authorization_url=openapi_authorization_url,
            openapi_token_url=openapi_token_url,
            openid_config_url=openid_config_url
        )

        # Assert
        assert isinstance(result, SingleTenantAzureAuthorizationCodeBearer)
        assert result.app_client_id == app_client_id

    def test_create_azure_scheme_with_mock_service_config(self):
        """Test creating Azure scheme with mock service OpenID configuration for testing."""
        # Arrange
        app_client_id = "test-client-id"
        tenant_id = "12345678-1234-1234-1234-123456789abc"
        scopes = {"api://test": "test"}
        openapi_authorization_url = "http://mock-auth-service:8005/test/oauth2/v2.0/authorize"
        openapi_token_url = "http://mock-auth-service:8005/test/oauth2/v2.0/token"
        openid_config_url = "http://mock-auth-service:8005/test/v2.0/.well-known/openid-configuration"

        # Act
        result = create_azure_scheme(
            app_client_id=app_client_id,
            tenant_id=tenant_id,
            scopes=scopes,
            openapi_authorization_url=openapi_authorization_url,
            openapi_token_url=openapi_token_url,
            openid_config_url=openid_config_url
        )

        # Assert
        assert isinstance(result, SingleTenantAzureAuthorizationCodeBearer)
        assert result.app_client_id == app_client_id

    def test_create_azure_scheme_requires_openid_config_url(self):
        """Test that create_azure_scheme now requires openid_config_url parameter."""
        # Arrange
        app_client_id = "test-client-id"
        tenant_id = "12345678-1234-1234-1234-123456789abc"
        scopes = {"api://test": "test"}
        openapi_authorization_url = "https://login.microsoftonline.com/test/oauth2/v2.0/authorize"
        openapi_token_url = "https://login.microsoftonline.com/test/oauth2/v2.0/token"

        # Act & Assert - should raise TypeError for missing required argument
        with pytest.raises(TypeError, match="missing.*required.*positional.*argument.*openid_config_url"):
            create_azure_scheme(
                app_client_id=app_client_id,
                tenant_id=tenant_id,
                scopes=scopes,
                openapi_authorization_url=openapi_authorization_url,
                openapi_token_url=openapi_token_url
            )

    def test_create_azure_scheme_always_returns_single_tenant(self):
        """Test that create_azure_scheme always returns SingleTenantAzureAuthorizationCodeBearer."""
        # Arrange
        app_client_id = "test-client-id"
        tenant_id = "12345678-1234-1234-1234-123456789abc"
        scopes = {"api://test": "test"}
        openapi_authorization_url = "https://login.microsoftonline.com/test/oauth2/v2.0/authorize"
        openapi_token_url = "https://login.microsoftonline.com/test/oauth2/v2.0/token"
        openid_config_url = "https://login.microsoftonline.com/test/v2.0/.well-known/openid-configuration"

        # Act
        result = create_azure_scheme(
            app_client_id=app_client_id,
            tenant_id=tenant_id,
            scopes=scopes,
            openapi_authorization_url=openapi_authorization_url,
            openapi_token_url=openapi_token_url,
            openid_config_url=openid_config_url
        )

        # Assert - should ALWAYS be SingleTenantAzureAuthorizationCodeBearer (not B2CMultiTenant)
        assert isinstance(result, SingleTenantAzureAuthorizationCodeBearer)
        assert result.app_client_id == app_client_id
