"""FastAPI app serving static UI and SSE bridge to Redis pubsub."""

from fastapi.responses import StreamingResponse, JSONResponse, Response
from fastapi.staticfiles import StaticFiles
from fastapi import Request
from pydantic import BaseModel, Field
from uuid import UUID
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
    # Always instruct the browser to call our same-origin proxy.
    # The server will use PROJECT_MANAGEMENT_SERVICE_URL internally.
    return JSONResponse({
        "projectManagementApiBase": "/api",
        "progressChannel": UI_PROJECT_PROGRESS_CHANNEL,
    })


@app.post("/dev-token")
async def get_dev_token():
    """
    Development helper: fetch a mock access token from mock-auth-service.
    Uses AZURE_AD_AUTHORITY and AZURE_TENANT_ID env vars. Defaults point to mock-auth-service in compose.
    """
    authority = os.getenv("AZURE_AD_AUTHORITY", "http://mock-auth-service:8000")
    tenant = os.getenv("AZURE_TENANT_ID", "e7963c3a-3b3a-43b6-9426-89e433d07e69")
    url = f"{authority.rstrip('/')}/{tenant}/oauth2/v2.0/token"
    async with httpx.AsyncClient(timeout=10.0) as client:
        r = await client.post(url)
        r.raise_for_status()
        return JSONResponse(r.json())


@app.api_route("/api/{path:path}", methods=["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"])
async def proxy_to_project_management(path: str, request: Request):
    """
    Reverse proxy endpoint that forwards browser requests to the
    Project Management Service using Docker-internal networking.
    Only the bearer token (Authorization header) is forwarded from the
    incoming request; all other incoming headers are dropped.
    """
    upstream_base = os.getenv("PROJECT_MANAGEMENT_SERVICE_URL", "http://project-management-service:8000").rstrip("/")
    # Preserve original query string
    target_url = f"{upstream_base}/{path}"
    if request.url.query:
        target_url = f"{target_url}?{request.url.query}"

    # Forward only Authorization header (bearer token). All other incoming
    # headers are intentionally not forwarded.
    forward_headers = {}
    auth_header = request.headers.get("authorization")
    if auth_header:
        forward_headers["Authorization"] = auth_header
    # Ensure upstream can parse payloads (JSON, multipart, etc.) by
    # explicitly setting Content-Type to match the incoming request.
    ct = request.headers.get("content-type")
    if ct:
        forward_headers["Content-Type"] = ct

    body = await request.body()

    try:
        async with httpx.AsyncClient(timeout=30.0) as client:
            resp = await client.request(
                method=request.method,
                url=target_url,
                content=body if body else None,
                headers=forward_headers,
            )
    except httpx.RequestError as exc:
        return JSONResponse({"detail": f"Upstream request failed: {str(exc)}"}, status_code=502)

    # Build response without passing through upstream headers.
    content_type = resp.headers.get("content-type", "")
    if "application/json" in content_type:
        try:
            return JSONResponse(resp.json(), status_code=resp.status_code)
        except Exception:
            # Fallback if body isn't valid JSON
            return Response(content=resp.content, status_code=resp.status_code)
    return Response(content=resp.content, status_code=resp.status_code)


class ChatMessageRequest(BaseModel):
    project_id: UUID = Field(..., description="Project ID for scoping the chat")
    prompt: str = Field(..., min_length=1, description="User prompt text")


class ChatMessageResponse(BaseModel):
    role: str = "assistant"
    content: str


async def _publish_agentic_updates(project_id: UUID):
    """Publish mock agentic workflow updates to UI channel via Redis."""
    try:
        redis = get_redis_client()
        async def pub(payload: dict):
            await redis.publish(UI_PROJECT_PROGRESS_CHANNEL, json.dumps(payload))

        # Start processing
        await pub({
            "project_id": str(project_id),
            "status": "rag_processing",
            "step": "Analyze prompt",
            "score": 0.2
        })
        await asyncio.sleep(0.6)

        await pub({
            "project_id": str(project_id),
            "status": "rag_processing",
            "step": "Search knowledge graph",
            "score": 0.5
        })
        await asyncio.sleep(0.6)

        await pub({
            "project_id": str(project_id),
            "status": "rag_ready",
            "step": "Synthesize answer",
            "score": 1.0
        })
    except Exception as e:
        logger.warning("Agentic mock publish failed", error=str(e))


@app.post("/chat/message")
async def chat_message(req: ChatMessageRequest):
    """
    Mock chat endpoint. Returns a synthetic markdown answer and publishes
    mock agentic workflow step/score updates via Redis to the UI SSE channel.
    """
    # Kick off background agentic updates (non-blocking)
    asyncio.create_task(_publish_agentic_updates(req.project_id))

    # Produce a simple markdown response based on the prompt
    content = f"""## Proposed Approach\n\nYou asked: `{req.prompt}`\n\n- We will analyze existing documents and project context.\n- Then we will derive epics and user stories suitable for GitLab.\n- Finally, we will validate acceptance criteria.\n\n> This is a mocked response until the agentic workflow is available.\n"""
    return JSONResponse(ChatMessageResponse(content=content).model_dump())

static_dir = os.path.join(os.path.dirname(__file__), "static")
app.mount("/", StaticFiles(directory=static_dir, html=True), name="static")

if __name__ == "__main__":
    settings = get_app_settings()
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)


