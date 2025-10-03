"""Local auth dependencies built on jwt_utils.

Provides strict and relaxed dependencies that validate signature and (optionally)
expiration using a shared HMAC secret. No OIDC/OpenID discovery is used.
"""

from typing import Callable, List
from dataclasses import dataclass
import structlog

from fastapi import Request, HTTPException, status, Depends

from .jwt_utils import extract_bearer_token, verify_jwt

logger = structlog.get_logger(__name__)


@dataclass
class LocalUser:
    oid: str
    preferred_username: str | None
    roles: List[str]
    token: str


def _get_token_from_request(request: Request) -> str | None:
    return extract_bearer_token(request)


def _user_from_claims(claims: dict, token: str) -> LocalUser:
    oid = str(claims.get("oid") or claims.get("sub") or "").strip()
    if not oid:
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Invalid token: missing subject")
    username = claims.get("preferred_username") or claims.get("upn") or claims.get("email")
    roles = claims.get("roles") or []
    if isinstance(roles, str):
        roles = [r.strip() for r in roles.split(" ") if r.strip()]
    roles = roles if isinstance(roles, list) else []
    return LocalUser(oid=oid, preferred_username=(str(username) if username else None), roles=roles, token=token)


def get_local_user_verified(token: str | None = Depends(_get_token_from_request)) -> LocalUser:
    if not token:
        logger.warning("Authentication required: no token provided")
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Authentication required")
    try:
        claims = verify_jwt(token, verify_exp=True)
        logger.debug("Token verified successfully", oid=claims.get("oid"), username=claims.get("preferred_username"))
    except Exception as e:
        logger.error("Token verification failed", error=str(e), error_type=type(e).__name__)
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Invalid or expired token")
    return _user_from_claims(claims, token)


def get_local_user_allow_expired(token: str | None = Depends(_get_token_from_request)) -> LocalUser:
    if not token:
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Authentication required")
    try:
        claims = verify_jwt(token, verify_exp=False)
    except Exception:
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Invalid token")
    return _user_from_claims(claims, token)


def require_roles_local(required_roles: List[str]) -> Callable[[LocalUser], LocalUser]:
    def _dep(user: LocalUser = Depends(get_local_user_verified)) -> LocalUser:
        user_roles = set(user.roles or [])
        if not set(required_roles).intersection(user_roles):
            raise HTTPException(status_code=status.HTTP_403_FORBIDDEN, detail="Insufficient roles")
        return user
    return _dep

