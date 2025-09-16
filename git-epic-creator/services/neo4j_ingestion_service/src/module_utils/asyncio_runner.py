"""
Persistent event loop runner for Celery tasks.

This module provides a single, long-lived event loop that runs for the entire
worker process lifetime, preventing cross-event-loop semaphore reuse issues
with fnllm rate limiters and other asyncio primitives.
"""

import asyncio
import threading
import structlog
from typing import Any, Coroutine, TypeVar
from concurrent.futures import Future

logger = structlog.get_logger(__name__)

T = TypeVar('T')

class PersistentEventLoopRunner:
    """Manages a single persistent event loop for the worker process."""
    
    _instance: 'PersistentEventLoopRunner | None' = None
    _lock = threading.Lock()
    
    def __init__(self):
        self._loop: asyncio.AbstractEventLoop | None = None
        self._thread: threading.Thread | None = None
        self._shutdown_event = threading.Event()
        self._ready_event = threading.Event()
    
    @classmethod
    def get_instance(cls) -> 'PersistentEventLoopRunner':
        """Get the singleton instance of the event loop runner."""
        if cls._instance is None:
            with cls._lock:
                if cls._instance is None:
                    cls._instance = cls()
        return cls._instance
    
    def _run_loop(self) -> None:
        """Run the event loop in a dedicated thread."""
        try:
            # Create a new event loop for this thread
            self._loop = asyncio.new_event_loop()
            asyncio.set_event_loop(self._loop)
            
            # Signal that the loop is ready
            self._ready_event.set()
            
            logger.info("Persistent event loop started", thread_id=threading.get_ident())
            
            # Run the loop until shutdown is requested
            self._loop.run_until_complete(self._wait_for_shutdown())
            
        except Exception as e:
            logger.error("Event loop thread failed", error=str(e))
            raise
        finally:
            if self._loop and not self._loop.is_closed():
                self._loop.close()
            logger.info("Persistent event loop stopped")
    
    async def _wait_for_shutdown(self) -> None:
        """Wait for shutdown signal."""
        while not self._shutdown_event.is_set():
            await asyncio.sleep(0.1)
    
    def start(self) -> None:
        """Start the persistent event loop in a background thread."""
        if self._thread is not None and self._thread.is_alive():
            return
        
        self._shutdown_event.clear()
        self._ready_event.clear()
        
        self._thread = threading.Thread(
            target=self._run_loop,
            name="PersistentEventLoop",
            daemon=True
        )
        self._thread.start()
        
        # Wait for the loop to be ready
        self._ready_event.wait(timeout=10.0)
        if not self._ready_event.is_set():
            raise RuntimeError("Failed to start persistent event loop within timeout")
    
    def stop(self) -> None:
        """Stop the persistent event loop."""
        if self._loop is None or self._loop.is_closed():
            return
        
        self._shutdown_event.set()
        
        if self._thread is not None:
            self._thread.join(timeout=5.0)
            if self._thread.is_alive():
                logger.warning("Event loop thread did not stop within timeout")
    
    def run(self, coro: Coroutine[Any, Any, T]) -> T:
        """
        Run a coroutine on the persistent event loop.
        
        Args:
            coro: The coroutine to run
            
        Returns:
            The result of the coroutine
            
        Raises:
            RuntimeError: If the event loop is not running
            Exception: Any exception raised by the coroutine
        """
        if self._loop is None or self._loop.is_closed():
            self.start()
        
        if self._loop is None or self._loop.is_closed():
            raise RuntimeError("Event loop is not available")
        
        # Use run_coroutine_threadsafe to execute the coroutine on the loop
        future: Future[T] = asyncio.run_coroutine_threadsafe(coro, self._loop)
        
        try:
            return future.result()
        except Exception as e:
            logger.error("Coroutine execution failed", error=str(e))
            raise
    
    def is_running(self) -> bool:
        """Check if the event loop is running."""
        return (
            self._loop is not None 
            and not self._loop.is_closed() 
            and self._thread is not None 
            and self._thread.is_alive()
        )


# Convenience function for easy usage
def run_async(coro: Coroutine[Any, Any, T]) -> T:
    """
    Run a coroutine on the persistent event loop.
    
    This is a convenience function that uses the singleton runner.
    
    Args:
        coro: The coroutine to run
        
    Returns:
        The result of the coroutine
    """
    runner = PersistentEventLoopRunner.get_instance()
    return runner.run(coro)


# Backwards-compatible helper name preferred in task code
def run_task(coro: Coroutine[Any, Any, T]) -> T:
    """Alias to run_async for readability in task code."""
    return run_async(coro)
