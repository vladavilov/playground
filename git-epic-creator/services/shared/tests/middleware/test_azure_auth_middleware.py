"""
Tests for Azure AD authentication middleware.
"""

import pytest
from unittest.mock import Mock, AsyncMock
from fastapi import HTTPException
from fastapi_azure_auth import SingleTenantAzureAuthorizationCodeBearer
from fastapi_azure_auth.user import User

from middleware.azure_auth_middleware import AzureAuthMiddleware, get_current_user, get_current_active_user, require_roles

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