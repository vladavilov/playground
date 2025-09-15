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
from constants.streams import UI_PROJECT_PROGRESS_CHANNEL, UI_AI_WORKFLOW_PROGRESS_CHANNEL

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

# Ensure broker stops cleanly with the app
@app.on_event("shutdown")
async def _shutdown_broker_event():
    try:
        await _broker.stop()
    except Exception:
        pass

def format_sse(data: dict, event: str | None = None) -> str:
    payload_lines = [f"data: {json.dumps(data)}"]
    if event:
        payload_lines.insert(0, f"event: {event}")
    payload_lines.append("\n")
    return "\n".join(payload_lines)


class _SSEBroker:
    """Single-process Redis pub/sub bridge to multiplex messages to many SSE clients."""

    def __init__(self) -> None:
        self._started: bool = False
        self._pubsub = None
        self._task: asyncio.Task | None = None
        self._subscribers: set[asyncio.Queue[str]] = set()
        self._start_lock: asyncio.Lock = asyncio.Lock()

    async def start(self, redis_client) -> None:
        async with self._start_lock:
            if self._started:
                return
            self._pubsub = redis_client.pubsub()
            await self._pubsub.subscribe(UI_PROJECT_PROGRESS_CHANNEL, UI_AI_WORKFLOW_PROGRESS_CHANNEL)
            self._started = True
            self._task = asyncio.create_task(self._read_loop())
            logger.info(
                "SSE broker started",
                project_channel=UI_PROJECT_PROGRESS_CHANNEL,
                ai_channel=UI_AI_WORKFLOW_PROGRESS_CHANNEL,
            )

    async def stop(self) -> None:
        try:
            if self._task is not None:
                try:
                    self._task.cancel()
                except Exception:
                    pass
            if self._pubsub is not None:
                try:
                    await self._pubsub.unsubscribe(UI_PROJECT_PROGRESS_CHANNEL, UI_AI_WORKFLOW_PROGRESS_CHANNEL)
                except Exception:
                    pass
                try:
                    await self._pubsub.close()
                except Exception:
                    pass
        finally:
            self._started = False
            self._pubsub = None
            self._task = None
            self._subscribers.clear()
            logger.info("SSE broker stopped")

    def _broadcast(self, payload: str) -> None:
        for q in list(self._subscribers):
            try:
                q.put_nowait(payload)
            except asyncio.QueueFull:
                # Drop slow subscriber
                self._subscribers.discard(q)
            except Exception:
                self._subscribers.discard(q)

    async def _read_loop(self) -> None:
        try:
            while True:
                msg = await self._pubsub.get_message(ignore_subscribe_messages=True, timeout=1.0)
                if msg and msg.get("type") == "message":
                    channel = msg.get("channel")
                    if isinstance(channel, (bytes, bytearray)):
                        try:
                            channel = channel.decode("utf-8")
                        except Exception:
                            channel = str(channel)

                    data_field = msg.get("data")
                    if isinstance(data_field, (bytes, bytearray)):
                        try:
                            data_field = data_field.decode("utf-8")
                        except Exception:
                            data_field = str(data_field)

                    try:
                        parsed = json.loads(data_field) if isinstance(data_field, str) else data_field
                    except Exception:
                        parsed = {"raw": str(data_field)}

                    event_name = (
                        "ai_workflow_progress"
                        if channel == UI_AI_WORKFLOW_PROGRESS_CHANNEL
                        else "project_progress"
                    )
                    self._broadcast(format_sse(parsed, event=event_name))
                await asyncio.sleep(0.01)
        except asyncio.CancelledError:
            return
        except Exception as e:
            logger.warning("SSE broker loop error", error=str(e))

    def register(self) -> asyncio.Queue[str]:
        q: asyncio.Queue[str] = asyncio.Queue(maxsize=100)
        self._subscribers.add(q)
        return q

    def unregister(self, q: asyncio.Queue[str]) -> None:
        self._subscribers.discard(q)


_broker = _SSEBroker()


async def redis_event_stream(request: Request) -> AsyncIterator[str]:
    redis = getattr(request.app.state, "redis_client", None) or get_redis_client()
    await _broker.start(redis)
    queue = _broker.register()
    logger.info("SSE subscriber connected")
    try:
        yield format_sse({"connected": True, "channel": UI_PROJECT_PROGRESS_CHANNEL}, event="hello")
        while True:
            if await request.is_disconnected():
                break
            try:
                payload = await asyncio.wait_for(queue.get(), timeout=1.0)
                yield payload
            except asyncio.TimeoutError:
                continue
    finally:
        _broker.unregister(queue)
        logger.info("SSE subscriber disconnected")


@app.get("/events")
async def sse_events(request: Request):
    return StreamingResponse(redis_event_stream(request), media_type="text/event-stream")


@app.get("/config")
async def get_ui_config():
    # Always instruct the browser to call our same-origin proxies.
    # The server will use service URLs internally.
    return JSONResponse({
        "projectManagementApiBase": "/project",
        "aiWorkflowApiBase": "/workflow",
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


@app.api_route("/project/{path:path}", methods=["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"])
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


@app.api_route("/workflow/{path:path}", methods=["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"])
async def proxy_to_ai_workflow(path: str, request: Request):
    """
    Reverse proxy endpoint that forwards browser requests to the
    AI Workflow Service using Docker-internal networking.
    Only the bearer token (Authorization header) is forwarded from the
    incoming request; all other incoming headers are dropped.
    """
    upstream_base = os.getenv("AI_WORKFLOW_SERVICE_URL", "http://ai-workflow-service:8000").rstrip("/")
    target_url = f"{upstream_base}/workflow/{path}"
    if request.url.query:
        target_url = f"{target_url}?{request.url.query}"

    forward_headers = {}
    auth_header = request.headers.get("authorization")
    if auth_header:
        forward_headers["Authorization"] = auth_header
    ct = request.headers.get("content-type")
    if ct:
        forward_headers["Content-Type"] = ct

    body = await request.body()

    try:
        async with httpx.AsyncClient(timeout=60.0) as client:
            resp = await client.request(
                method=request.method,
                url=target_url,
                content=body if body else None,
                headers=forward_headers,
            )
    except httpx.RequestError as exc:
        return JSONResponse({"detail": f"Upstream request failed: {str(exc)}"}, status_code=502)

    content_type = resp.headers.get("content-type", "")
    if "application/json" in content_type:
        try:
            return JSONResponse(resp.json(), status_code=resp.status_code)
        except Exception:
            return Response(content=resp.content, status_code=resp.status_code)
    return Response(content=resp.content, status_code=resp.status_code)


class ChatMessageRequest(BaseModel):
    project_id: UUID = Field(..., description="Project ID for scoping the chat")
    prompt: str = Field(..., min_length=1, description="User prompt text")

static_dir = os.path.join(os.path.dirname(__file__), "static")
app.mount("/", StaticFiles(directory=static_dir, html=True), name="static")

if __name__ == "__main__":
    settings = get_app_settings()
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)


