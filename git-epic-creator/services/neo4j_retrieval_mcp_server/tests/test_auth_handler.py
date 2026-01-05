from unittest.mock import AsyncMock, MagicMock, patch

import pytest


@pytest.mark.asyncio
async def test_get_auth_token_returns_azure_bearer_and_caches():
    from auth import MCPAuthHandler

    handler = MCPAuthHandler()

    class Ctx:
        session_id = "sess-1"

    with patch("auth.get_http_headers", return_value={"authorization": "Bearer azure.token"}):
        token = await handler.get_auth_token(Ctx())

    assert token == "azure.token"

    # Second call should hit cache (no header dependency required)
    with patch("auth.get_http_headers", return_value={}):
        token2 = await handler.get_auth_token(Ctx())

    assert token2 == "azure.token"

    await handler.close()


@pytest.mark.asyncio
async def test_get_userinfo_calls_auth_service_endpoint():
    from auth import MCPAuthHandler

    handler = MCPAuthHandler()

    fake_client = MagicMock()
    fake_resp = MagicMock()
    fake_resp.status_code = 200
    fake_resp.json.return_value = {"sub": "oid-1", "preferred_username": "u@example.com", "roles": []}
    fake_client.get = AsyncMock(return_value=fake_resp)

    with patch.object(handler, "_get_http_client", return_value=fake_client):
        with patch("auth.get_auth_service_url", return_value="http://envoy-gateway:8000"):
            out = await handler.get_userinfo("azure.token")

    assert out == {"sub": "oid-1", "preferred_username": "u@example.com", "roles": []}
    fake_client.get.assert_called_once()

    await handler.close()


