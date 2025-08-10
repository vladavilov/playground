"""Azure AD authentication middleware for FastAPI applications."""

from fastapi import Depends, HTTPException, status, Request
from fastapi.security import SecurityScopes
from fastapi_azure_auth import SingleTenantAzureAuthorizationCodeBearer
from fastapi_azure_auth.user import User
from fastapi_azure_auth.openid_config import OpenIdConfig
import structlog

logger = structlog.get_logger(__name__)

class AzureAuthMiddleware:
    """Loads OpenID configuration and exposes auth utilities."""

    def __init__(self, azure_scheme):
        """
        Initialize the Azure auth middleware.
        
        Args:
            azure_scheme: Azure authentication scheme instance (Single or B2C Multi-tenant)
        """
        self.azure_scheme = azure_scheme
        logger.info("Azure auth middleware initialized")
    
    async def load_openid_config(self) -> None:
        """
        Load OpenID configuration on startup.
        This ensures the configuration is available immediately.
        """
        try:
            await self.azure_scheme.openid_config.load_config()
            logger.info("OpenID configuration loaded")
        except Exception as e:
            # Log detailed error information for debugging
            logger.error(
                "Failed to load OpenID configuration", 
                error=str(e),
                error_type=type(e).__name__,
                openid_url=getattr(self.azure_scheme.openid_config, 'openid_config_url', 'Unknown')
            )
            
            # Check if it's a tenant ID issue
            if "400 Bad Request" in str(e) and "openid-configuration" in str(e):
                logger.error(
                    "OpenID configuration failed - likely invalid tenant ID or network issue. "
                    "Please verify AZURE_TENANT_ID environment variable and network connectivity."
                )
            
            raise e

def create_azure_scheme(
    app_client_id: str,
    tenant_id: str,
    scopes: dict[str, str],
    openapi_authorization_url: str,
    openapi_token_url: str,
    openid_config_url: str
):
    """
    Create Azure authentication scheme using SingleTenantAzureAuthorizationCodeBearer with custom OpenID configuration.
    
    This approach works around the limitation that SingleTenantAzureAuthorizationCodeBearer doesn't
    support the openid_config_url parameter by creating the instance and then modifying its OpenID
    configuration object.
    
    Args:
        app_client_id: Azure application client ID
        tenant_id: Azure tenant ID
        scopes: Authentication scopes
        openapi_authorization_url: OpenAPI authorization URL
        openapi_token_url: OpenAPI token URL
        openid_config_url: Custom OpenID configuration URL
        
    Returns:
        SingleTenantAzureAuthorizationCodeBearer instance with custom OpenID config
    """
    logger.info("Creating Azure scheme with custom OpenID config",
                tenant_id=tenant_id, openid_config_url=openid_config_url)
    
    azure_scheme = SingleTenantAzureAuthorizationCodeBearer(
        app_client_id=app_client_id,
        tenant_id=tenant_id,
        scopes=scopes,
        openapi_authorization_url=openapi_authorization_url,
        openapi_token_url=openapi_token_url
    )
    
    # Override the OpenID configuration with our custom URL
    # This works because OpenIdConfig accepts config_url parameter
    azure_scheme.openid_config = OpenIdConfig(
        tenant_id=tenant_id,
        multi_tenant=False,  # Single tenant configuration
        app_id=None,  # Don't use app_id unless specifically needed
        config_url=openid_config_url
    )
    
    logger.info("Azure scheme created", config_url=azure_scheme.openid_config.config_url)
    
    return azure_scheme

# Global variable to store the azure scheme - will be set during app initialization
_azure_scheme: SingleTenantAzureAuthorizationCodeBearer = None

def get_azure_scheme() -> SingleTenantAzureAuthorizationCodeBearer:
    """
    Get the global Azure scheme instance.
    This should be set during application startup.
    """
    if _azure_scheme is None:
        raise RuntimeError("Azure scheme not initialized. Ensure it's set during app startup.")
    return _azure_scheme

def set_azure_scheme(scheme: SingleTenantAzureAuthorizationCodeBearer):
    """Set global Azure scheme instance."""
    global _azure_scheme
    _azure_scheme = scheme

async def get_current_user(
    request: Request,
    security_scopes: SecurityScopes = SecurityScopes(),
    azure_scheme: SingleTenantAzureAuthorizationCodeBearer = Depends(get_azure_scheme)
) -> User:
    """Return current authenticated user, validating scopes via scheme."""
    try:
        # The azure_scheme is callable and handles the full authentication flow
        user = await azure_scheme(request, security_scopes)
        
        if user is None:
            logger.warning("Authentication returned None - likely auto_error=False")
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Authentication required",
                headers={"WWW-Authenticate": "Bearer"},
            )
        
        logger.info("User authenticated successfully", 
            user_id=user.oid,
            username=user.preferred_username,
            scopes=security_scopes.scopes
        )
        return user
    except HTTPException as e:
        logger.warning("User authentication failed", status_code=e.status_code, detail=e.detail)
        raise
    except Exception as e:
        logger.error("Unexpected authentication error", error=str(e), error_type=type(e).__name__)
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Authentication failed",
            headers={"WWW-Authenticate": "Bearer"},
        ) from e

async def get_current_active_user(
    current_user: User = Depends(get_current_user)
) -> User:
    """
    Get the current authenticated and active user.
    
    Args:
        current_user: Current authenticated user
        
    Returns:
        User: Active authenticated user object
        
    Raises:
        HTTPException: If user is inactive
    """
    if current_user.is_guest:
        logger.warning("Guest user attempted to access protected resource", user_id=current_user.oid)
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Guest users are not allowed"
        )

    logger.info("Active user verified", user_id=current_user.oid)
    return current_user

def require_roles(required_roles: list[str]):
    """
    Create a dependency that requires specific roles.
    
    Args:
        required_roles: List of required role names
        
    Returns:
        Dependency function that validates user roles
    """
    async def validate_roles(current_user: User = Depends(get_current_active_user)) -> User:
        """
        Validate that the current user has required roles.
        
        Args:
            current_user: Current authenticated user
            
        Returns:
            User: User with validated roles
            
    Raises:
            HTTPException: If user doesn't have required roles
        """
        user_roles = set(current_user.roles or [])
        required_roles_set = set(required_roles)

        if not required_roles_set.intersection(user_roles):
            logger.warning(
                "User lacks required roles",
                user_id=current_user.oid,
                user_roles=list(user_roles),
                required_roles=required_roles
            )
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail=f"User must have one of the following roles: {', '.join(required_roles)}"
            )

        logger.info(
            "User role validation passed",
            user_id=current_user.oid,
            user_roles=list(user_roles),
            required_roles=required_roles
        )
        return current_user

    return validate_roles

def require_scopes(required_scopes: list[str]):  # deprecated, kept for compatibility
    def scoped_dependency(current_user: User = Depends(get_current_active_user)) -> User:
        return current_user
    return scoped_dependency
