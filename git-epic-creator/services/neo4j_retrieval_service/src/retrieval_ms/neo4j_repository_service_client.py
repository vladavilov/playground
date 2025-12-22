from __future__ import annotations

from typing import Any, Dict

import httpx

from config import get_retrieval_settings

_CLIENT: httpx.AsyncClient | None = None


def get_client() -> httpx.AsyncClient:
    """Get a pooled AsyncClient for neo4j_repository_service."""
    global _CLIENT
    if _CLIENT is not None:
        return _CLIENT

    settings = get_retrieval_settings()
    base_url = settings.NEO4J_REPOSITORY_SERVICE_URL.rstrip("/")

    # Keep these resilient: some environments/services only define the timeout fields.
    connect_timeout = float(getattr(settings, "HTTP_CONNECTION_TIMEOUT", 30.0))
    read_timeout = float(getattr(settings, "HTTP_READ_TIMEOUT", 180.0))
    max_connections = int(getattr(settings, "HTTP_MAX_CONNECTIONS", 50))
    max_keepalive = int(getattr(settings, "HTTP_MAX_KEEPALIVE_CONNECTIONS", 20))

    timeout = httpx.Timeout(
        connect=connect_timeout,
        read=read_timeout,
        write=connect_timeout,
        pool=connect_timeout,
    )
    limits = httpx.Limits(max_connections=max_connections, max_keepalive_connections=max_keepalive)

    _CLIENT = httpx.AsyncClient(base_url=base_url, timeout=timeout, limits=limits)
    return _CLIENT


async def post_json(client: httpx.AsyncClient, path: str, payload: Dict[str, Any]) -> Dict[str, Any]:
    """POST JSON payload and return parsed JSON body (or empty dict)."""
    resp = await client.post(path, json=payload)
    resp.raise_for_status()
    return resp.json() or {}


async def get_json(client: httpx.AsyncClient, path: str) -> Dict[str, Any]:
    """GET JSON and return parsed JSON body (or empty dict)."""
    resp = await client.get(path)
    resp.raise_for_status()
    return resp.json() or {}

