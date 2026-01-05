from unittest.mock import Mock, patch

import pytest
from fastapi.testclient import TestClient


@pytest.fixture
def client():
    import main

    # Avoid env-heavy azure settings in tests; router will prefer app.state if present.
    main.app.state.azure_auth_settings = Mock(SCOPE_NAME="api://test/user_impersonation")
    return TestClient(main.app)


@pytest.fixture(autouse=True)
def _set_session_secret(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.setenv("SESSION_SECRET_KEY", "test-secret")
    monkeypatch.setenv("ALLOW_INSECURE_SESSION", "true")


def _mock_msal_app():
    app = Mock()

    def mock_get_auth_url(scopes, state, redirect_uri):
        return (
            "https://login.microsoftonline.com/authorize"
            f"?client_id=test&state={state}&redirect_uri={redirect_uri}"
        )

    app.get_authorization_request_url = Mock(side_effect=mock_get_auth_url)
    app.acquire_token_by_authorization_code = Mock(
        return_value={
            "id_token_claims": {
                "oid": "user123",
                "preferred_username": "test@example.com",
                "roles": ["User"],
                "tid": "tenant123",
            }
        }
    )
    return app


def test_login_redirects_to_azure_ad(client: TestClient):
    from routers import browser_auth_router as router_mod

    with patch.object(router_mod, "_get_msal_app", return_value=_mock_msal_app()):
        resp = client.get("/auth/login", follow_redirects=False)

    assert resp.status_code in (302, 307)
    assert "login.microsoftonline.com" in (resp.headers.get("location") or "")


def test_callback_sets_session_and_redirects(client: TestClient):
    from routers import browser_auth_router as router_mod

    msal_app = _mock_msal_app()

    with patch.object(router_mod, "_get_msal_app", return_value=msal_app):
        login = client.get("/auth/login", follow_redirects=False)
        assert login.status_code in (302, 307)
        location = login.headers["location"]
        state_param = location.split("state=")[1].split("&")[0]

        cb = client.get(f"/auth/callback?code=test_code&state={state_param}", follow_redirects=False)

    assert cb.status_code in (302, 307)
    assert cb.headers["location"].startswith("/")

    me = client.get("/auth/me")
    assert me.status_code == 200
    assert me.json()["authenticated"] is True


def test_me_unauthenticated(client: TestClient):
    resp = client.get("/auth/me")
    assert resp.status_code == 200
    assert resp.json()["authenticated"] is False


