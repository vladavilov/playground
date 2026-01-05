import importlib

from fastapi.testclient import TestClient
from utils.jwt_utils import verify_jwt


def _build_client(monkeypatch) -> TestClient:
    # Required by shared AzureAuthSettings validators used by TokenService init
    monkeypatch.setenv("AZURE_TENANT_ID", "00000000-0000-0000-0000-000000000000")
    monkeypatch.setenv("AZURE_CLIENT_ID", "11111111-1111-1111-1111-111111111111")
    monkeypatch.setenv("AZURE_CLIENT_SECRET", "test-secret")

    # Required by JWT signing
    monkeypatch.setenv("LOCAL_JWT_SECRET", "local-jwt-secret-for-tests")

    # Required by /auth/s2s/mint caller authentication
    monkeypatch.setenv("API_GATEWAY_MINT_SECRET", "mint-secret-for-tests")

    import main as auth_main

    importlib.reload(auth_main)
    return TestClient(auth_main.app)


def test_s2s_mint_rejects_missing_secret(monkeypatch):
    client = _build_client(monkeypatch)

    resp = client.post("/auth/s2s/mint", json={})

    assert resp.status_code == 401


def test_s2s_mint_rejects_wrong_secret(monkeypatch):
    client = _build_client(monkeypatch)

    resp = client.post(
        "/auth/s2s/mint",
        json={},
        headers={"X-Api-Gateway-Secret": "wrong"},
    )

    assert resp.status_code == 401


def test_s2s_mint_returns_service_token(monkeypatch):
    client = _build_client(monkeypatch)

    resp = client.post(
        "/auth/s2s/mint",
        json={},
        headers={"X-Api-Gateway-Secret": "mint-secret-for-tests"},
    )

    assert resp.status_code == 200
    body = resp.json()

    assert body["token_type"] == "Bearer"
    assert body["expires_in"] == 600
    assert isinstance(body["access_token"], str)

    claims = verify_jwt(body["access_token"], verify_exp=True)
    assert claims["sub"] == "api-gateway"
    assert claims["iss"] == "authentication-service"
    assert claims["aud"] == "internal-services"

    # No user-bound claims are allowed on service tokens
    assert "oid" not in claims
    assert "roles" not in claims


