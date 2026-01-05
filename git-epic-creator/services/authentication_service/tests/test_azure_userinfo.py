from unittest.mock import patch

import pytest
from fastapi.testclient import TestClient


@pytest.fixture(autouse=True)
def _env(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.setenv("SESSION_SECRET_KEY", "test-secret")
    monkeypatch.setenv("ALLOW_INSECURE_SESSION", "true")
    monkeypatch.setenv("LOCAL_JWT_SECRET", "dev-local-jwt-secret")


@pytest.fixture
def client() -> TestClient:
    import main

    return TestClient(main.app)


def test_azure_userinfo_requires_bearer_token(client: TestClient):
    resp = client.get("/auth/azure/userinfo")
    assert resp.status_code == 401


def test_azure_userinfo_returns_oidc_shape(client: TestClient):
    import routers.token_router as router_mod

    async def fake_validate(_token: str):
        return {"oid": "oid-123", "preferred_username": "user@example.com", "roles": ["User"]}

    with patch.object(router_mod, "TokenService", autospec=True) as svc_cls:
        svc = svc_cls.return_value
        svc.validate_azure_access_token.side_effect = fake_validate

        resp = client.get("/auth/azure/userinfo", headers={"Authorization": "Bearer azure.token"})

    assert resp.status_code == 200
    body = resp.json()
    assert body["sub"] == "oid-123"
    assert body["preferred_username"] == "user@example.com"
    assert body["email"] == "user@example.com"
    assert body["roles"] == ["User"]


