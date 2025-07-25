"""
Azure AD authentication middleware for FastAPI applications.
"""

from fastapi import Depends, HTTPException, status
from fastapi_azure_auth import SingleTenantAzureAuthorizationCodeBearer
from fastapi_azure_auth.user import User
import structlog

logger = structlog.get_logger(__name__)

class AzureAuthMiddleware:
    """
    Azure AD authentication middleware for FastAPI applications.
    Handles OpenID configuration loading and provides authentication utilities.
    """

    def __init__(self, azure_scheme: SingleTenantAzureAuthorizationCodeBearer):
        """
        Initialize the Azure auth middleware.
        
        Args:
            azure_scheme: Azure authentication scheme instance
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
            logger.info("OpenID configuration loaded successfully")
        except Exception as e:
            logger.error("Failed to load OpenID configuration", error=str(e))
            raise

async def get_current_user(
    azure_scheme: SingleTenantAzureAuthorizationCodeBearer = Depends()
) -> User:
    """
    Get the current authenticated user.
    
    Args:
        azure_scheme: Azure authentication scheme dependency
        
    Returns:
        User: Authenticated user object
        
    Raises:
        HTTPException: If authentication fails
    """
    try:
        user = await azure_scheme()
        logger.info("User authenticated successfully", user_id=user.oid, username=user.preferred_username)
        return user
    except HTTPException as e:
        logger.warning("User authentication failed", status_code=e.status_code, detail=e.detail)
        raise
    except Exception as e:
        logger.error("Unexpected authentication error", error=str(e))
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Authentication failed",
            headers={"WWW-Authenticate": "Bearer"},
        )

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
    
    logger.info("Active user verified", user_id=current_user.oid, username=current_user.preferred_username)
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