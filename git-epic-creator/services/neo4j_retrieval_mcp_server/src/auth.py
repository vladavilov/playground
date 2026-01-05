"""MCP Authentication handler for browser-based OAuth (Azure AD).

This MCP server receives an Azure AD access token from the MCP client (VS Code)
and forwards it to Envoy as a Bearer token. Envoy `ext_authz` validates the token
via authentication-service and injects S2S headers upstream.
"""

import asyncio
import time
from dataclasses import dataclass
from typing import Any
import httpx
import structlog

from fastmcp.server.dependencies import get_http_headers
from fastmcp.server.dependencies import get_http_request
from config import get_auth_service_url

logger = structlog.get_logger(__name__)


@dataclass
class MCPAuthContext:
    """Cached authentication context for this MCP session."""

    azure_access_token: str
    expires_at: float


class MCPAuthHandler:
    """
    Handles authentication for MCP sessions.
    
    Extracts Azure AD tokens from VS Code's HTTP Authorization header
    and exchanges them for LOCAL JWTs via authentication-service.
    
    This is the SAME flow as UI Service uses after browser login.
    Azure AD authentication is REQUIRED.
    """
    
    def __init__(self):
        self._cache: dict[str, MCPAuthContext] = {}
        self._lock = asyncio.Lock()
        self._http_client: httpx.AsyncClient | None = None
    
    async def _get_http_client(self) -> httpx.AsyncClient:
        """Get or create HTTP client."""
        if self._http_client is None or self._http_client.is_closed:
            self._http_client = httpx.AsyncClient(timeout=30.0)
        return self._http_client
    
    async def close(self) -> None:
        """Close HTTP client."""
        if self._http_client and not self._http_client.is_closed:
            await self._http_client.aclose()
            self._http_client = None
    
    async def get_auth_token(self, ctx: Any) -> str | None:
        """
        Get Azure AD access token for upstream calls (forwarded to Envoy).
        
        Extracts Azure AD token from:
        1. HTTP Authorization header (VS Code OAuth flow) - PREFERRED
        2. MCP context client_params (fallback for stdio transport)
        
        If no token is found, returns None which triggers HTTP 401
        to start the OAuth discovery flow.
        
        Args:
            ctx: MCP Context object (from FastMCP)
            
        Returns:
            Azure AD access token (without "Bearer " prefix), or None if unavailable
        """
        # Check for cached token
        session_id = self._get_session_id(ctx)
        if session_id:
            cached = await self._get_cached_token(session_id)
            if cached:
                logger.debug(
                    "Using cached Azure AD token",
                    session_id=session_id,
                )
                return cached.azure_access_token
        
        # Try to extract Azure AD token from HTTP Authorization header (VS Code OAuth)
        azure_token = self._extract_bearer_token_from_headers(ctx)
        
        if azure_token:
            logger.info("Found Azure AD token in HTTP Authorization header")
            await self._cache_token(
                session_id,
                MCPAuthContext(azure_access_token=azure_token, expires_at=time.time() + 300),
            )
            return azure_token
        
        # Fallback: Try MCP context client_params (for stdio transport)
        client_params = self._extract_client_params(ctx)
        if azure_token := client_params.get("access_token"):
            logger.info("Found Azure AD token in MCP context params")
            await self._cache_token(
                session_id,
                MCPAuthContext(azure_access_token=azure_token, expires_at=time.time() + 300),
            )
            return azure_token
        
        # No token found - return None to trigger HTTP 401
        logger.info("No Azure AD token found, authentication required")
        return None
    
    def _extract_bearer_token_from_headers(self, ctx: Any) -> str | None:
        """
        Extract Bearer token from HTTP Authorization header.
        
        This is how VS Code's MCP client passes the OAuth token
        after completing the browser-based OAuth flow.
        
        Args:
            ctx: MCP Context object
            
        Returns:
            Bearer token (without "Bearer " prefix) or None
        """
        # Method 1: Use FastMCP's get_http_headers dependency
        try:
            headers = get_http_headers()
            auth_header = headers.get("authorization", "")
            if auth_header.lower().startswith("bearer "):
                token = auth_header[7:]  # Strip "Bearer " prefix
                logger.debug("Extracted token from get_http_headers()")
                return token
        except Exception as e:
            logger.debug("get_http_headers() not available", error=str(e))
        
        # Method 2: Try get_http_request for full request access
        try:
            request = get_http_request()
            auth_header = request.headers.get("authorization", "")
            if auth_header.lower().startswith("bearer "):
                token = auth_header[7:]
                logger.debug("Extracted token from get_http_request()")
                return token
        except Exception as e:
            logger.debug("get_http_request() not available", error=str(e))
        
        # Method 3: Try accessing request_context directly from ctx
        if hasattr(ctx, 'request_context') and ctx.request_context:
            req_ctx = ctx.request_context
            # Try to get headers from request context
            if hasattr(req_ctx, 'request') and hasattr(req_ctx.request, 'headers'):
                auth_header = req_ctx.request.headers.get("authorization", "")
                if auth_header.lower().startswith("bearer "):
                    token = auth_header[7:]
                    logger.debug("Extracted token from ctx.request_context")
                    return token
        
        return None
    
    def _extract_client_params(self, ctx: Any) -> dict[str, Any]:
        """Extract client parameters from MCP context (for stdio transport)."""
        # Try direct attribute access
        if hasattr(ctx, 'client_params') and ctx.client_params:
            return ctx.client_params
        
        # Try request info (FastMCP)
        if hasattr(ctx, 'request_info'):
            req_info = ctx.request_info
            if isinstance(req_info, dict):
                return req_info.get('params', {})
        
        # Try meta attribute
        if hasattr(ctx, 'meta') and ctx.meta:
            if isinstance(ctx.meta, dict):
                return ctx.meta
        
        return {}
    
    def _get_session_id(self, ctx: Any) -> str | None:
        """Extract session identifier from MCP context."""
        if hasattr(ctx, 'session_id'):
            return ctx.session_id
        
        client_params = self._extract_client_params(ctx)
        if session_id := client_params.get('session_id'):
            return session_id
        
        if hasattr(ctx, 'request_info') and ctx.request_info:
            req_info = ctx.request_info
            if isinstance(req_info, dict) and 'id' in req_info:
                return f"mcp-{req_info['id']}"
        
        return None
    
    async def _get_cached_token(self, session_id: str) -> MCPAuthContext | None:
        """Get cached token if not expired."""
        async with self._lock:
            cached = self._cache.get(session_id)
            if cached and not self._is_expired(cached):
                return cached
            if cached:
                del self._cache[session_id]
        return None
    
    def _is_expired(self, auth_ctx: MCPAuthContext) -> bool:
        """Check if cached auth context is expired (with 60s buffer)."""
        return time.time() > (auth_ctx.expires_at - 60)
    
    async def _cache_token(self, session_id: str | None, auth_ctx: MCPAuthContext) -> None:
        """Cache authentication context."""
        if not session_id:
            return
        async with self._lock:
            self._cache[session_id] = auth_ctx
    
    async def get_userinfo(self, azure_token: str) -> dict[str, Any] | None:
        """
        Validate the Azure AD access token and return OIDC-like userinfo.

        NOTE: This call is intentionally made *via Envoy gateway* (AUTH_SERVICE_URL should point to Envoy)
        to match the system architecture: MCP behaves like a browser client, sending Azure Bearer tokens
        through the gateway. Envoy routes `/auth/*` to authentication-service (ext_authz disabled).
        """
        try:
            client = await self._get_http_client()
            gateway_url = get_auth_service_url().rstrip("/")
            resp = await client.get(
                f"{gateway_url}/auth/azure/userinfo",
                headers={"Authorization": f"Bearer {azure_token}"},
            )
            if resp.status_code != 200:
                return None
            data = resp.json()
            return data if isinstance(data, dict) else None
        except Exception as e:
            logger.warning("Failed to fetch userinfo", error=str(e), error_type=type(e).__name__)
            return None
