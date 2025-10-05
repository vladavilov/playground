from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional


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


class HandlerRegistry:
    """Registry for managing and routing to handlers."""

    def __init__(self):
        self._handlers: List[BaseHandler] = []

    def register(self, handler: BaseHandler) -> None:
        """Register a handler. Handlers are checked in registration order."""
        self._handlers.append(handler)

    def find_handler(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> Optional[BaseHandler]:
        """Find the first handler that can process the messages."""
        for handler in self._handlers:
            if handler.can_handle(messages, combined_text, lower_text):
                return handler
        return None

