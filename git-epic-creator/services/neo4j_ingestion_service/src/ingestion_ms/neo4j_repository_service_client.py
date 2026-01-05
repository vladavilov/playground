from __future__ import annotations

import os
from datetime import date, datetime
from decimal import Decimal
from typing import Any, Dict
from uuid import UUID

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


def _to_jsonable(value: Any, *, path: str = "$") -> Any:
    """
    Convert a Python object into a structure that the stdlib `json` module can serialize.

    This is a defensive boundary for httpx's `json=...` parameter. It prevents runtime failures
    when upstream data (e.g., pandas/pyarrow/numpy) introduces non-JSON-native types such as
    numpy arrays or numpy scalars.

    We intentionally keep this local to the transport layer so every outbound request benefits.
    """
    # Fast path for already-JSON-native scalars
    if value is None or isinstance(value, (str, int, float, bool)):
        return value

    # Common scalar wrappers
    if isinstance(value, (UUID, Decimal)):
        return str(value)
    if isinstance(value, (datetime, date)):
        return value.isoformat()

    # Mapping types: ensure keys are strings for JSON
    if isinstance(value, dict):
        out: Dict[str, Any] = {}
        for k, v in value.items():
            key = k if isinstance(k, str) else str(k)
            out[key] = _to_jsonable(v, path=f"{path}.{key}")
        return out

    # Sequence types
    if isinstance(value, (list, tuple)):
        return [_to_jsonable(v, path=f"{path}[{i}]") for i, v in enumerate(value)]

    # Sets are not JSON-serializable; represent as list (order not guaranteed)
    if isinstance(value, set):
        return [_to_jsonable(v, path=f"{path}[{i}]") for i, v in enumerate(value)]

    # Bytes: best-effort decode to UTF-8 for logging/transport
    if isinstance(value, (bytes, bytearray)):
        try:
            return value.decode("utf-8")
        except Exception:
            return bytes(value).decode("utf-8", errors="replace")

    # Numpy/pandas/pyarrow objects (or any array-like) often implement .tolist() / .item()
    # We avoid importing numpy globally; rely on duck-typing and fall back to strict errors.
    if hasattr(value, "tolist"):
        try:
            return _to_jsonable(value.tolist(), path=path)
        except Exception:
            # fall through to .item() or type error
            pass

    if hasattr(value, "item"):
        try:
            return _to_jsonable(value.item(), path=path)
        except Exception:
            pass

    raise TypeError(f"Object of type {type(value).__name__} at {path} is not JSON serializable")


def post_json(
    client: httpx.Client,
    path: str,
    payload: Dict[str, Any],
    *,
    auth_header: str | None = None,
) -> Dict[str, Any]:
    json_payload = _to_jsonable(payload, path="$")
    headers = {"Authorization": auth_header} if auth_header else None
    resp = client.post(path, json=json_payload, headers=headers)
    resp.raise_for_status()
    data = resp.json()
    if not isinstance(data, dict):
        raise RuntimeError(f"neo4j-repository-service returned non-object JSON for {path}")
    return data




