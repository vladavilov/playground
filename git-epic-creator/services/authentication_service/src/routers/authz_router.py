from __future__ import annotations

from dataclasses import dataclass

import structlog
from fastapi import APIRouter, Request
from fastapi.responses import JSONResponse

from services.gateway_policy import SERVICE_TOKEN_AUDIENCE, is_x_user_id_allowed
from services.token_service import TokenService

logger = structlog.get_logger(__name__)
router = APIRouter()


@dataclass(frozen=True)
class _PolicyDecision:
    allowed: bool
    required_roles: tuple[str, ...] = ()


def _get_original_path(request: Request) -> str:
    for key in ("x-envoy-original-path", "x-original-uri", "x-forwarded-uri"):
        val = request.headers.get(key)
        if isinstance(val, str) and val.strip():
            return val.split("?", 1)[0]
    return "/"


def _get_original_method(request: Request) -> str:
    for key in ("x-envoy-original-method", "x-forwarded-method"):
        val = request.headers.get(key)
        if isinstance(val, str) and val.strip():
            return val.strip().upper()
    return request.method.upper()


def _decide(method: str, path: str) -> _PolicyDecision:
    if path.startswith("/project/") and method in {"POST", "PUT", "PATCH", "DELETE"}:
        return _PolicyDecision(allowed=True, required_roles=("Admin",))
    return _PolicyDecision(allowed=True, required_roles=())


def _has_required_roles(user_roles: list[str] | None, required: tuple[str, ...]) -> bool:
    if not required:
        return True
    roles = set(user_roles or [])
    return bool(roles.intersection(required))


def _read_bearer_token(request: Request) -> str | None:
    raw = request.headers.get("authorization") or request.headers.get("Authorization") or ""
    raw = raw.strip()
    if not raw:
        return None
    if raw.lower().startswith("bearer "):
        token = raw[7:].strip()
        return token or None
    return None


async def _resolve_identity(request: Request) -> tuple[str | None, list[str]]:
    """
    Resolve user identity for `ext_authz`.

    Priority:
    1) Browser session cookie (oid + roles stored by MSAL callback)
    2) Bearer Azure AD access token (for MCP via Envoy)
    """
    session = request.session
    oid = session.get("oid")
    if isinstance(oid, str) and oid.strip():
        roles = session.get("roles") or []
        return oid.strip(), roles if isinstance(roles, list) else []

    bearer = _read_bearer_token(request)
    if not bearer:
        return None, []

    try:
        svc = TokenService()
        claims = await svc.validate_azure_access_token(bearer)
    except Exception as e:
        logger.warning("Azure bearer token validation failed", error=str(e))
        return None, []

    user_id = str(claims.get("oid") or claims.get("sub") or "").strip()
    roles = claims.get("roles") or []
    if isinstance(roles, str):
        roles = [r.strip() for r in roles.split() if r.strip()]
    return (user_id or None), roles if isinstance(roles, list) else []


@router.api_route("/authz", methods=["GET", "POST"])
async def authz_check(request: Request):
    """
    Envoy ext_authz endpoint.

    Returns 200 on allow (with injected headers) or 403 on deny.
    """
    oid, roles = await _resolve_identity(request)
    if not oid:
        return JSONResponse(status_code=403, content={"ok": False, "reason": "unauthenticated"})

    method = _get_original_method(request)
    path = _get_original_path(request)

    decision = _decide(method, path)
    if not decision.allowed or not _has_required_roles(roles, decision.required_roles):
        return JSONResponse(status_code=403, content={"ok": False, "reason": "forbidden"})

    svc = TokenService()
    s2s = svc.mint_gateway_service_token(aud=SERVICE_TOKEN_AUDIENCE)["access_token"]

    headers = {"authorization": f"Bearer {s2s}"}
    if is_x_user_id_allowed(path):
        headers["x-user-id"] = str(oid)

    return JSONResponse(status_code=200, content={"ok": True}, headers=headers)


