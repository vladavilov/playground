"""FastAPI app serving static UI and SSE bridge to Redis pubsub."""

from fastapi.responses import StreamingResponse, JSONResponse
from fastapi.staticfiles import StaticFiles
from fastapi import Request
from typing import AsyncIterator
import asyncio
import json
import os
import structlog
import httpx
from configuration.common_config import get_app_settings
from utils.app_factory import FastAPIFactory
from utils.redis_client import get_redis_client
from constants.streams import UI_PROJECT_PROGRESS_CHANNEL

logger = structlog.get_logger(__name__)

app = FastAPIFactory.create_app(
    title="UI Service",
    description="Serves static UI and SSE for project progress",
    version="0.1.0",
    enable_azure_auth=False,
    enable_cors=True,
    enable_postgres=False,
    enable_redis=True,
)

def format_sse(data: dict, event: str | None = None) -> str:
    payload_lines = [f"data: {json.dumps(data)}"]
    if event:
        payload_lines.insert(0, f"event: {event}")
    payload_lines.append("\n")
    return "\n".join(payload_lines)


async def redis_event_stream(request: Request) -> AsyncIterator[str]:
    redis = get_redis_client()
    pubsub = redis.pubsub()
    await pubsub.subscribe(UI_PROJECT_PROGRESS_CHANNEL)
    logger.info("SSE subscriber connected", channel=UI_PROJECT_PROGRESS_CHANNEL)

    try:
        # Send a hello event
        yield format_sse({"connected": True, "channel": UI_PROJECT_PROGRESS_CHANNEL}, event="hello")
        while True:
            if await request.is_disconnected():
                break
            message = await pubsub.get_message(ignore_subscribe_messages=True, timeout=1.0)
            if message and message.get("type") == "message":
                try:
                    data = json.loads(message["data"].decode("utf-8") if isinstance(message["data"], (bytes, bytearray)) else message["data"]) 
                except Exception:
                    data = {"raw": str(message.get("data"))}
                yield format_sse(data, event="project_progress")
            await asyncio.sleep(0.05)
    finally:
        try:
            await pubsub.unsubscribe(UI_PROJECT_PROGRESS_CHANNEL)
        except Exception:
            pass
        await pubsub.close()
        logger.info("SSE subscriber disconnected", channel=UI_PROJECT_PROGRESS_CHANNEL)


@app.get("/events")
async def sse_events(request: Request):
    return StreamingResponse(redis_event_stream(request), media_type="text/event-stream")


@app.get("/config")
async def get_ui_config():
    pm_api_base = os.getenv("PM_API_BASE", "http://localhost:8080")
    return JSONResponse({
        "projectManagementApiBase": pm_api_base,
        "progressChannel": UI_PROJECT_PROGRESS_CHANNEL,
    })


@app.post("/dev-token")
async def get_dev_token():
    """
    Development helper: fetch a mock access token from mock-auth-service.
    Requires MOCK_AUTH_BASE and AZURE_TENANT_ID env vars. Defaults to localhost ports.
    """
    base = os.getenv("MOCK_AUTH_BASE", "http://localhost:8005")
    tenant = os.getenv("AZURE_TENANT_ID", "e7963c3a-3b3a-43b6-9426-89e433d07e69")
    url = f"{base.rstrip('/')}/{tenant}/oauth2/v2.0/token"
    async with httpx.AsyncClient(timeout=10.0) as client:
        r = await client.post(url)
        r.raise_for_status()
        return JSONResponse(r.json())

static_dir = os.path.join(os.path.dirname(__file__), "static")
app.mount("/", StaticFiles(directory=static_dir, html=True), name="static")

if __name__ == "__main__":
    settings = get_app_settings()
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)


