"""Abstract and in-memory implementations of a minimal Service Bus client."""
from __future__ import annotations

from abc import ABC, abstractmethod
from collections import deque
from typing import Deque, List, Any


class ServiceBusClient(ABC):
    """Interface exposing only the operations required by our services."""

    @abstractmethod
    def send_message(self, message: Any) -> None:
        """Send *message* to the queue."""

    @abstractmethod
    def receive_messages(self, max_messages: int = 1) -> List[Any]:
        """Retrieve up to *max_messages* from the queue (FIFO). Returns list which may be empty."""


class InMemoryServiceBusClient(ServiceBusClient):
    """In-memory FIFO queue implementation suitable for tests and local runs."""

    def __init__(self) -> None:
        self._queue: Deque[Any] = deque()

    # ------------------------------------------------------------------
    # Interface implementations
    # ------------------------------------------------------------------
    def send_message(self, message: Any) -> None:  # noqa: D401
        self._queue.append(message)

    def receive_messages(self, max_messages: int = 1) -> List[Any]:  # noqa: D401
        if max_messages <= 0:
            raise ValueError("max_messages must be positive")
        msgs: List[Any] = []
        for _ in range(min(max_messages, len(self._queue))):
            msgs.append(self._queue.popleft())
        return msgs

    # Convenience for tests
    def clear(self) -> None:
        self._queue.clear() 