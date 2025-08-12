"""
Tests for centralized error handling registration and formatting.
"""

from fastapi import FastAPI, HTTPException
from fastapi.testclient import TestClient
from pydantic import BaseModel

from utils.error_handler import ErrorHandler


class Item(BaseModel):
    name: str


def _create_test_app_with_handlers() -> FastAPI:
    app = FastAPI()
    # Register global exception handlers (to be implemented in ErrorHandler)
    ErrorHandler().register_exception_handlers(app)  # type: ignore[attr-defined]
    return app


def test_http_exception_is_wrapped_and_sanitized():
    app = _create_test_app_with_handlers()

    @app.get("/boom-http")
    def boom_http():
        # Intentionally include a token to verify redaction
        raise HTTPException(status_code=403, detail="Forbidden, token=secret-token")

    client = TestClient(app, raise_server_exceptions=False)
    resp = client.get("/boom-http")

    assert resp.status_code == 403
    data = resp.json()
    assert data["status"] == "error"
    assert "Forbidden" in data["detail"]
    assert "secret-token" not in data["detail"]


def test_validation_exception_is_wrapped():
    app = _create_test_app_with_handlers()

    @app.post("/items")
    def create_item(item: Item):  # Missing field should trigger validation error
        return item

    client = TestClient(app, raise_server_exceptions=False)
    # Missing required field "name"
    resp = client.post("/items", json={})

    assert resp.status_code == 422
    data = resp.json()
    assert data["status"] == "error"
    assert "validation" in data["detail"].lower() or "error" in data["detail"].lower()


def test_generic_exception_is_wrapped_and_sanitized():
    app = _create_test_app_with_handlers()

    @app.get("/boom-generic")
    def boom_generic():
        # Include password to verify redaction
        raise RuntimeError("Something bad happened; password=hunter2")

    client = TestClient(app, raise_server_exceptions=False)
    resp = client.get("/boom-generic")

    assert resp.status_code == 500
    data = resp.json()
    assert data["status"] == "error"
    assert "Something bad happened" in data["detail"]
    assert "hunter2" not in data["detail"]


def test_direct_generic_formatter_sanitizes():
    handler = ErrorHandler()
    response = handler.format_generic_error(Exception("token=abc123"))
    assert response.status_code == 500
    payload = response.body.decode("utf-8")
    assert "abc123" not in payload
    assert "[REDACTED]" in payload


