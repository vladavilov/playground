from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient


@pytest.fixture(autouse=True)
def _session_env(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.setenv("SESSION_SECRET_KEY", "test-secret")
    monkeypatch.setenv("ALLOW_INSECURE_SESSION", "true")
    monkeypatch.setenv("LOCAL_JWT_SECRET", "dev-local-jwt-secret")


@pytest.fixture
def client():
    import main

    return TestClient(main.app)


def test_authz_denies_when_unauthenticated(client: TestClient):
    resp = client.post("/authz", headers={"x-envoy-original-path": "/project/123"})
    assert resp.status_code == 403
    assert resp.json()["ok"] is False


def test_authz_allows_with_session_and_injects_headers(client: TestClient):
    import routers.authz_router as router_mod

    async def fake_resolve_identity(_request):
        return ("user-oid", ["Admin"])

    with patch.object(router_mod, "_resolve_identity", side_effect=fake_resolve_identity):
        resp = client.post(
            "/authz",
            headers={
                "x-envoy-original-path": "/project/123",
                "x-envoy-original-method": "POST",
            },
        )

    assert resp.status_code == 200
    assert resp.headers.get("authorization", "").startswith("Bearer ")
    assert resp.headers.get("x-user-id") == "user-oid"


def test_authz_bearer_mode_uses_azure_claims(client: TestClient):
    import routers.authz_router as router_mod

    async def fake_validate(_token):
        return {"oid": "oid-123", "roles": ["User"]}

    with patch.object(router_mod, "TokenService", autospec=True) as svc_cls:
        svc = svc_cls.return_value
        svc.validate_azure_access_token.side_effect = fake_validate
        svc.mint_gateway_service_token.return_value = {"access_token": "svc.jwt", "expires_in": 600}

        resp = client.post(
            "/authz",
            headers={
                "authorization": "Bearer azure.token",
                "x-envoy-original-path": "/project/abc",
                "x-envoy-original-method": "GET",
            },
        )

    assert resp.status_code == 200
    assert resp.headers.get("authorization") == "Bearer svc.jwt"
    assert resp.headers.get("x-user-id") == "oid-123"


def test_authz_enforces_admin_for_project_mutations(client: TestClient):
    import routers.authz_router as router_mod

    async def fake_resolve_identity(_request):
        return ("user-oid", ["User"])

    with patch.object(router_mod, "_resolve_identity", side_effect=fake_resolve_identity):
        resp = client.post(
            "/authz",
            headers={"x-envoy-original-path": "/project/123", "x-envoy-original-method": "POST"},
        )

    assert resp.status_code == 403
    assert resp.json()["reason"] == "forbidden"


