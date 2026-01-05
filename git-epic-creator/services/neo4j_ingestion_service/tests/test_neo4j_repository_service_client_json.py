import pytest


class _FakeResponse:
    def raise_for_status(self) -> None:
        return None

    def json(self):
        return {"ok": True}


def test_post_json_sanitizes_numpy_ndarray(monkeypatch):
    np = pytest.importorskip("numpy")

    from ingestion_ms.neo4j_repository_service_client import post_json

    captured = {}

    class _FakeClient:
        def post(self, path, json, headers=None):  # noqa: A002 - match httpx signature style
            captured["path"] = path
            captured["json"] = json
            captured["headers"] = headers
            return _FakeResponse()

    payload = {
        "project_id": "p1",
        "rows": [{"embedding": np.array([1.0, 2.0, 3.0])}],
    }

    out = post_json(_FakeClient(), "/v1/test", payload, auth_header="Bearer svc.jwt.token")

    assert out["ok"] is True
    assert captured["path"] == "/v1/test"
    assert captured["json"]["rows"][0]["embedding"] == [1.0, 2.0, 3.0]
    assert captured["headers"] == {"Authorization": "Bearer svc.jwt.token"}


def test_post_json_raises_clear_error_for_unsupported_type():
    from ingestion_ms.neo4j_repository_service_client import post_json

    class _Unsupported:
        pass

    class _FakeClient:
        def post(self, path, json, headers=None):  # noqa: A002 - match httpx signature style
            return _FakeResponse()

    payload = {"x": {"y": _Unsupported()}}

    with pytest.raises(TypeError) as exc:
        post_json(_FakeClient(), "/v1/test", payload, auth_header="Bearer svc.jwt.token")

    msg = str(exc.value)
    assert "not JSON serializable" in msg
    assert "$.x.y" in msg


