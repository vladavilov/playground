from __future__ import annotations

import os
from typing import Any, Dict

import httpx

_CLIENT: httpx.Client | None = None


def _base_url() -> str:
    return os.getenv("NEO4J_REPOSITORY_SERVICE_URL", "http://neo4j-repository-service:8080").rstrip("/")


def get_client() -> httpx.Client:
    """
    Process-wide pooled HTTP client for neo4j-repository-service.

    This worker executes ingestion steps in sync code paths (same as the previous Neo4j driver usage),
    so we use httpx.Client intentionally.
    """
    global _CLIENT
    if _CLIENT is not None:
        return _CLIENT

    timeout_s = float(os.getenv("NEO4J_REPOSITORY_TIMEOUT_S", "120"))
    limits = httpx.Limits(
        max_connections=int(os.getenv("HTTP_MAX_CONNECTIONS", "50")),
        max_keepalive_connections=int(os.getenv("HTTP_MAX_KEEPALIVE_CONNECTIONS", "20")),
        keepalive_expiry=float(os.getenv("HTTP_KEEPALIVE_EXPIRY_S", "30")),
    )
    _CLIENT = httpx.Client(
        base_url=_base_url(),
        timeout=httpx.Timeout(timeout_s),
        limits=limits,
        headers={"Accept": "application/json", "Content-Type": "application/json"},
    )
    return _CLIENT


def post_json(client: httpx.Client, path: str, payload: Dict[str, Any]) -> Dict[str, Any]:
    resp = client.post(path, json=payload)
    resp.raise_for_status()
    data = resp.json()
    if not isinstance(data, dict):
        raise RuntimeError(f"neo4j-repository-service returned non-object JSON for {path}")
    return data


