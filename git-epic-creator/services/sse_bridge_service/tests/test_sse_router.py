from unittest.mock import patch

from fastapi.testclient import TestClient


def test_events_endpoint_streams_without_session():
    import main

    client = TestClient(main.app)

    async def fake_stream(_request):
        yield "event: hello\ndata: {\"ok\": true}\n\n"

    with patch("routers.sse_router._redis_event_stream", side_effect=fake_stream):
        resp = client.get("/events")

    assert resp.status_code == 200
    assert resp.headers["content-type"].startswith("text/event-stream")

