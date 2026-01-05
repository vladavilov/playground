from fastapi import Depends, FastAPI
from fastapi.testclient import TestClient

from utils.jwt_utils import sign_jwt
from utils.local_auth import get_gateway_service_verified


def test_get_gateway_service_verified_rejects_missing_token():
    app = FastAPI()

    @app.get("/ok")
    def ok(_caller=Depends(get_gateway_service_verified())):
        return {"ok": True}

    client = TestClient(app)
    resp = client.get("/ok")
    assert resp.status_code == 401


def test_get_gateway_service_verified_rejects_wrong_sub(monkeypatch):
    monkeypatch.setenv("LOCAL_JWT_SECRET", "shared-secret-for-tests")
    token = sign_jwt(
        {"sub": "not-gateway", "iss": "authentication-service", "aud": "internal-services"},
        expires_in_seconds=600,
    )

    app = FastAPI()

    @app.get("/ok")
    def ok(_caller=Depends(get_gateway_service_verified())):
        return {"ok": True}

    client = TestClient(app)
    resp = client.get("/ok", headers={"Authorization": f"Bearer {token}"})
    assert resp.status_code == 401


def test_get_gateway_service_verified_rejects_wrong_issuer(monkeypatch):
    monkeypatch.setenv("LOCAL_JWT_SECRET", "shared-secret-for-tests")
    token = sign_jwt(
        {"sub": "api-gateway", "iss": "wrong-issuer", "aud": "internal-services"},
        expires_in_seconds=600,
    )

    app = FastAPI()

    @app.get("/ok")
    def ok(_caller=Depends(get_gateway_service_verified())):
        return {"ok": True}

    client = TestClient(app)
    resp = client.get("/ok", headers={"Authorization": f"Bearer {token}"})
    assert resp.status_code == 401


def test_get_gateway_service_verified_accepts_valid_token(monkeypatch):
    monkeypatch.setenv("LOCAL_JWT_SECRET", "shared-secret-for-tests")
    token = sign_jwt(
        {"sub": "api-gateway", "iss": "authentication-service", "aud": "internal-services"},
        expires_in_seconds=600,
    )

    app = FastAPI()

    @app.get("/ok")
    def ok(_caller=Depends(get_gateway_service_verified())):
        return {"ok": True}

    client = TestClient(app)
    resp = client.get("/ok", headers={"Authorization": f"Bearer {token}"})
    assert resp.status_code == 200
    assert resp.json() == {"ok": True}


