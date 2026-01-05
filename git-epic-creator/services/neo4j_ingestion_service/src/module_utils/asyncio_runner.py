"""
Thin re-export of the shared persistent event loop runner.

This service historically imported the runner via `module_utils.asyncio_runner`.
We keep that import path to avoid churn, but the implementation lives in the
shared library (`utils.asyncio_runner`) to prevent duplication.
"""

from utils.asyncio_runner import PersistentEventLoopRunner, run_async

__all__ = ["PersistentEventLoopRunner", "run_async"]


