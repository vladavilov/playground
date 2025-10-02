"""Local JWT utilities for signing and verifying tokens without OIDC.

This module centralizes JWT creation and validation to keep services DRY.
It uses an HMAC secret shared across services via environment variable
`LOCAL_JWT_SECRET` and the HS256 algorithm by default.
"""

from __future__ import annotations

import os
import time
from typing import Any, Dict, Optional
import structlog

from fastapi import Request
from jose import jwt

logger = structlog.get_logger(__name__)

_DEFAULT_ALGORITHM = os.getenv("LOCAL_JWT_ALG", "HS256")


def _get_secret() -> str:
    secret = os.getenv("LOCAL_JWT_SECRET", "").strip()
    if not secret:
        logger.error("LOCAL_JWT_SECRET environment variable is not set")
        raise RuntimeError(
            "LOCAL_JWT_SECRET is not set. Configure a strong shared secret for local JWTs."
        )
    logger.debug("LOCAL_JWT_SECRET loaded", secret_length=len(secret))
    return secret


def sign_jwt(
    claims: Dict[str, Any],
    expires_in_seconds: int = 3600,
    algorithm: Optional[str] = None,
) -> str:
    """Create a signed JWT with exp/iat set if not provided.

    Args:
        claims: base claims (e.g., {"oid": "...", "roles": [..], "preferred_username": "..."})
        expires_in_seconds: token lifetime if `exp` not already present
        algorithm: override algorithm (defaults to env or HS256)
    """
    now = int(time.time())
    payload = dict(claims)
    if "iat" not in payload:
        payload["iat"] = now
    if "exp" not in payload and expires_in_seconds > 0:
        payload["exp"] = now + int(expires_in_seconds)

    secret = _get_secret()
    alg = (algorithm or _DEFAULT_ALGORITHM).strip() or _DEFAULT_ALGORITHM
    return jwt.encode(payload, secret, algorithm=alg)


def verify_jwt(token: str, verify_exp: bool = True, algorithms: Optional[list[str]] = None) -> Dict[str, Any]:
    """Verify signature (and optionally expiration) and return claims.

    Args:
        token: bearer token without the "Bearer " prefix
        verify_exp: if False, do not fail on expired tokens
        algorithms: allowed algorithms; defaults to env algorithm
        
    Note:
        For internal S2S tokens, audience verification is disabled since all
        services within the system are trusted. Only signature and expiration
        are verified.
    """
    try:
        secret = _get_secret()
        algs = algorithms or [_DEFAULT_ALGORITHM]
        options = {
            "verify_exp": verify_exp,
            "verify_aud": False,  # Disable audience verification for internal S2S tokens
        }
        # Note: verify_aud: False in options is not enough for jose library
        claims = jwt.decode(token, secret, algorithms=algs, options=options, audience=None)
        logger.debug("JWT decoded successfully", verify_exp=verify_exp, algorithms=algs, aud=claims.get("aud"))
        return claims
    except Exception as e:
        logger.error("JWT verification failed", error=str(e), error_type=type(e).__name__, verify_exp=verify_exp, algorithms=algorithms or [_DEFAULT_ALGORITHM])
        raise


def extract_bearer_token(request: Request) -> Optional[str]:
    """Extract a bearer token from Authorization header if present and well-formed."""
    try:
        header_val = request.headers.get("authorization")
        if not isinstance(header_val, str) or not header_val:
            return None
        parts = header_val.split(" ", 1)
        if len(parts) != 2 or parts[0].lower() != "bearer":
            return None
        token = parts[1].strip()
        if token.count(".") != 2:
            return None
        return token
    except Exception:
        return None


