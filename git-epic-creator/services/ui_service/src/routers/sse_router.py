from __future__ import annotations

from typing import AsyncIterator
import asyncio
import json
import structlog
from fastapi import APIRouter, Request
from fastapi.responses import StreamingResponse

from constants.streams import (
    UI_PROJECT_PROGRESS_CHANNEL,
    UI_AI_REQUIREMENTS_PROGRESS_CHANNEL,
    UI_AI_TASKS_PROGRESS_CHANNEL,
    UI_RETRIEVAL_PROGRESS_CHANNEL,
)
from utils.redis_client import get_redis_client


logger = structlog.get_logger(__name__)
router = APIRouter()


def _format_sse(data: dict, event: str | None = None) -> str:
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
            await self._pubsub.subscribe(
                UI_PROJECT_PROGRESS_CHANNEL,
                UI_AI_REQUIREMENTS_PROGRESS_CHANNEL,
                UI_AI_TASKS_PROGRESS_CHANNEL,
                UI_RETRIEVAL_PROGRESS_CHANNEL,
            )
            self._started = True
            self._task = asyncio.create_task(self._read_loop())
            logger.info(
                "SSE broker started",
                project_channel=UI_PROJECT_PROGRESS_CHANNEL,
                workflow_channel=UI_AI_REQUIREMENTS_PROGRESS_CHANNEL,
                tasks_channel=UI_AI_TASKS_PROGRESS_CHANNEL,
                retrieval_channel=UI_RETRIEVAL_PROGRESS_CHANNEL,
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
                    await self._pubsub.unsubscribe(
                        UI_PROJECT_PROGRESS_CHANNEL,
                        UI_AI_REQUIREMENTS_PROGRESS_CHANNEL,
                        UI_AI_TASKS_PROGRESS_CHANNEL,
                        UI_RETRIEVAL_PROGRESS_CHANNEL,
                    )
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
                self._subscribers.discard(q)
            except Exception:
                self._subscribers.discard(q)

    async def _read_loop(self) -> None:
        try:
            while True:
                msg = await self._pubsub.get_message(
                    ignore_subscribe_messages=True, timeout=1.0
                )
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
                        parsed = (
                            json.loads(data_field) if isinstance(data_field, str) else data_field
                        )
                    except Exception:
                        parsed = {"raw": str(data_field)}

                    # Map channel to SSE event name
                    if channel == UI_AI_REQUIREMENTS_PROGRESS_CHANNEL:
                        event_name = "ai_requirements_progress"
                    elif channel == UI_AI_TASKS_PROGRESS_CHANNEL:
                        event_name = "ai_tasks_progress"
                    elif channel == UI_PROJECT_PROGRESS_CHANNEL:
                        event_name = "project_progress"
                    elif channel == UI_RETRIEVAL_PROGRESS_CHANNEL:
                        event_name = "retrieval_progress"
                    else:
                        event_name = "unknown"
                    
                    self._broadcast(_format_sse(parsed, event=event_name))
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


async def _redis_event_stream(request: Request) -> AsyncIterator[str]:
    redis = getattr(request.app.state, "redis_client", None) or get_redis_client(is_pubsub_client=True)
    await _broker.start(redis)
    queue = _broker.register()
    logger.info("SSE subscriber connected")
    try:
        yield _format_sse({"connected": True, "channel": UI_PROJECT_PROGRESS_CHANNEL}, event="hello")
        while True:
            if await request.is_disconnected():
                logger.info("Client disconnected, closing SSE stream")
                break
            try:
                payload = await asyncio.wait_for(queue.get(), timeout=1.0)
                yield payload
            except asyncio.TimeoutError:
                yield ": heartbeat\n\n"
    except Exception as e:
        logger.error("SSE stream error", error=str(e), error_type=type(e).__name__)
        raise
    finally:
        _broker.unregister(queue)
        logger.info("SSE subscriber disconnected")


@router.get("/events")
async def sse_events(request: Request):
    return StreamingResponse(
        _redis_event_stream(request),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "X-Accel-Buffering": "no",
        },
    )


