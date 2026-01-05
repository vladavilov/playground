"""
Persistent event loop runner for sync contexts (e.g., Celery tasks).

This module provides a single, long-lived event loop that runs for the entire
worker process lifetime, preventing cross-event-loop reuse issues with asyncio
primitives and async HTTP clients.
"""

from __future__ import annotations

import asyncio
import threading
from concurrent.futures import Future
from typing import Any, Coroutine, TypeVar

import structlog

logger = structlog.get_logger(__name__)

T = TypeVar("T")


class PersistentEventLoopRunner:
    """Manages a single persistent event loop for the current process."""

    _instance: "PersistentEventLoopRunner | None" = None
    _lock = threading.Lock()

    def __init__(self) -> None:
        self._loop: asyncio.AbstractEventLoop | None = None
        self._thread: threading.Thread | None = None
        self._shutdown_event = threading.Event()
        self._ready_event = threading.Event()

    @classmethod
    def get_instance(cls) -> "PersistentEventLoopRunner":
        if cls._instance is None:
            with cls._lock:
                if cls._instance is None:
                    cls._instance = cls()
        return cls._instance

    def _run_loop(self) -> None:
        try:
            self._loop = asyncio.new_event_loop()
            asyncio.set_event_loop(self._loop)
            self._ready_event.set()
            logger.info("Persistent event loop started", thread_id=threading.get_ident())
            self._loop.run_until_complete(self._wait_for_shutdown())
        except Exception as exc:
            logger.error("Event loop thread failed", error=str(exc))
            raise
        finally:
            if self._loop and not self._loop.is_closed():
                self._loop.close()
            logger.info("Persistent event loop stopped")

    async def _wait_for_shutdown(self) -> None:
        while not self._shutdown_event.is_set():
            await asyncio.sleep(0.1)

    def start(self) -> None:
        if self._thread is not None and self._thread.is_alive():
            return

        self._shutdown_event.clear()
        self._ready_event.clear()

        self._thread = threading.Thread(
            target=self._run_loop,
            name="PersistentEventLoop",
            daemon=True,
        )
        self._thread.start()

        self._ready_event.wait(timeout=10.0)
        if not self._ready_event.is_set():
            raise RuntimeError("Failed to start persistent event loop within timeout")

    def stop(self) -> None:
        if self._loop is None or self._loop.is_closed():
            return

        self._shutdown_event.set()
        if self._thread is not None:
            self._thread.join(timeout=5.0)
            if self._thread.is_alive():
                logger.warning("Event loop thread did not stop within timeout")

    def run(self, coro: Coroutine[Any, Any, T]) -> T:
        if self._loop is None or self._loop.is_closed():
            self.start()

        if self._loop is None or self._loop.is_closed():
            raise RuntimeError("Event loop is not available")

        future: Future[T] = asyncio.run_coroutine_threadsafe(coro, self._loop)
        try:
            return future.result()
        except Exception as exc:
            logger.error("Coroutine execution failed", error=str(exc))
            raise

    def is_running(self) -> bool:
        return (
            self._loop is not None
            and not self._loop.is_closed()
            and self._thread is not None
            and self._thread.is_alive()
        )


def run_async(coro: Coroutine[Any, Any, T]) -> T:
    """Run a coroutine on the persistent event loop."""

    return PersistentEventLoopRunner.get_instance().run(coro)



