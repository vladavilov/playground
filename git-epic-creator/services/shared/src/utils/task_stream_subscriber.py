"""
Generalized Redis Streams subscriber that enqueues Celery tasks and XACKs on success.

Designed to be reused by multiple services.
"""

from __future__ import annotations

import asyncio
import json
from dataclasses import dataclass
from typing import Any, Callable, Dict, List, Optional, Tuple

import structlog

logger = structlog.get_logger(__name__)


@dataclass
class ProcessResult:
    processed: int = 0
    failed: int = 0


class TaskStreamSubscriber:
    """
    Consume Redis Streams entries using a consumer group and enqueue a Celery task per message.
    """

    def __init__(
        self,
        *,
        redis_client,
        stream_key: str,
        consumer_group: str,
        consumer_name: str,
        task,
        queue: str,
        build_apply_kwargs: Optional[Callable[[Dict[str, Any]], Dict[str, Any]]] = None,
        block_ms: int = 1000,
        read_count: int = 10,
        claim_idle_ms: int = 60_000,
    ) -> None:
        self.redis_client = redis_client
        self.stream_key = stream_key
        self.consumer_group = consumer_group
        self.consumer_name = consumer_name
        self.task = task
        self.queue = queue
        self.block_ms = block_ms
        self.read_count = read_count
        self.claim_idle_ms = claim_idle_ms
        self.build_apply_kwargs = build_apply_kwargs
        self._running = False

    async def start_listening(self) -> None:
        """Block and continuously read messages, enqueuing and acking on success."""
        await self._ensure_group()
        self._running = True
        while self._running:
            try:
                messages = await self.redis_client.xreadgroup(
                    self.consumer_group,
                    self.consumer_name,
                    {self.stream_key: ">"},
                    count=self.read_count,
                    block=self.block_ms,
                )
                if messages:
                    await self.process_messages(messages)
            except Exception as e:
                logger.error("Streams read error", error=str(e))
                await asyncio.sleep(1)

    async def stop_listening(self) -> None:
        self._running = False

    async def _ensure_group(self) -> None:
        try:
            # Ensure stream exists
            try:
                await self.redis_client.xinfo_stream(self.stream_key)
            except Exception:
                dummy_id = await self.redis_client.xadd(self.stream_key, {"_init": "1"})
                await self.redis_client.xdel(self.stream_key, dummy_id)
            # Create consumer group
            await self.redis_client.xgroup_create(
                self.stream_key, self.consumer_group, id="0", mkstream=True
            )
        except Exception as e:
            if "BUSYGROUP" in str(e).upper() or "ALREADY" in str(e).upper():
                return
            logger.debug("xgroup create error (ignored if exists)", error=str(e))

    async def claim_idle_messages(self) -> Tuple[str, List[Any]]:
        """Claim idle messages for this consumer via XAUTOCLAIM."""
        try:
            return await self.redis_client.xautoclaim(
                self.stream_key,
                self.consumer_group,
                self.consumer_name,
                min_idle_time=self.claim_idle_ms,
                start_id="0-0",
                count=self.read_count,
            )
        except Exception as e:
            logger.error("xautoclaim failed", error=str(e))
            return "0-0", []

    async def process_messages(self, stream_messages) -> ProcessResult:
        processed = 0
        failed = 0
        for stream, entries in stream_messages:
            if stream != self.stream_key:
                continue
            for message_id, fields in entries:
                try:
                    decoded = self._decode_fields(fields)
                    apply_kwargs = self._build_apply_kwargs(decoded)
                    result = self.task.apply_async(**apply_kwargs)
                    if result is None:
                        raise RuntimeError("apply_async returned None")
                    await self.redis_client.xack(
                        self.stream_key, self.consumer_group, message_id
                    )
                    processed += 1
                except Exception as e:
                    failed += 1
                    logger.error("Failed to process message", message_id=message_id, error=str(e))
        return ProcessResult(processed=processed, failed=failed)

    def _decode_fields(self, fields: Dict[Any, Any]) -> Dict[str, Any]:
        decoded: Dict[str, Any] = {}
        for k, v in fields.items():
            key = k.decode("utf-8") if isinstance(k, (bytes, bytearray)) else k
            val = v.decode("utf-8") if isinstance(v, (bytes, bytearray)) else v
            if key in {"documents", "parameters"} and isinstance(val, str):
                try:
                    decoded[key] = json.loads(val)
                    continue
                except Exception:
                    pass
            decoded[key] = val
        return decoded

    def _build_apply_kwargs(self, fields: Dict[str, Any]) -> Dict[str, Any]:
        if self.build_apply_kwargs is not None:
            kwargs = dict(self.build_apply_kwargs(fields))
            # Ensure queue is set if not provided
            kwargs.setdefault("queue", self.queue)
            return kwargs
        # Default mapping for ingestion jobs
        args = [
            fields.get("job_id"),
            fields.get("project_id"),
            fields.get("documents"),
            int(fields.get("attempts", 0) or 0),
        ]
        return {"args": args, "queue": self.queue}
