from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Any, Dict, List, Optional, Sequence


class BaseHandler(ABC):
    """Abstract base class for prompt handlers."""

    @abstractmethod
    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        """
        Determine if this handler can process the given messages.
        
        Args:
            messages: List of message dictionaries from the chat request
            combined_text: All message content concatenated with newlines
            lower_text: Lowercased combined_text for case-insensitive matching
            
        Returns:
            True if this handler should process the request
        """
        pass

    @abstractmethod
    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        """
        Generate the response content for this handler.
        
        Args:
            messages: List of message dictionaries from the chat request
            combined_text: All message content concatenated with newlines
            model: The model name from the request
            
        Returns:
            Generated response string
        """
        pass

    def is_fallback(self) -> bool:
        """Whether this handler is the fallback.

        Fallback handlers should not participate in ambiguity detection. They are used
        only when no other handler matches.
        """
        return False


@dataclass(frozen=True)
class AmbiguousHandlerMatch(Exception):
    """Raised when more than one non-fallback handler matches a request."""

    matches: Sequence[str]


class HandlerRegistry:
    """Registry for managing and routing to handlers."""

    def __init__(self):
        self._handlers: List[BaseHandler] = []

    def register(self, handler: BaseHandler) -> None:
        """Register a handler. Handlers are checked in registration order."""
        self._handlers.append(handler)

    def _all_matching_handlers(
        self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str
    ) -> List[BaseHandler]:
        matches: List[BaseHandler] = []
        for handler in self._handlers:
            if handler.is_fallback():
                continue
            if handler.can_handle(messages, combined_text, lower_text):
                matches.append(handler)
        return matches

    def fallback_handler(self) -> Optional[BaseHandler]:
        for handler in self._handlers:
            if handler.is_fallback():
                return handler
        return None

    def select_handler(
        self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str
    ) -> BaseHandler:
        """Select exactly one handler for the request.

        Rules:
        - If exactly one non-fallback handler matches -> use it.
        - If none match -> use fallback (must exist).
        - If more than one matches -> raise AmbiguousHandlerMatch.
        """
        matches = self._all_matching_handlers(messages, combined_text, lower_text)
        if len(matches) == 1:
            return matches[0]
        if len(matches) == 0:
            fb = self.fallback_handler()
            if fb is None:
                raise RuntimeError("No fallback handler registered")
            return fb

        raise AmbiguousHandlerMatch(matches=[type(h).__name__ for h in matches])

