"""Base node class for DRIFT retrieval pipeline.

Provides common functionality for all graph nodes:
- State type definitions
- Dependency management
- Logging utilities
"""

from typing import Any, Callable, Dict
from abc import ABC, abstractmethod
import structlog

logger = structlog.get_logger(__name__)


class BaseNode(ABC):
    """Abstract base class for all retrieval pipeline nodes.
    
    Provides dependency injection and common utilities for node implementations.
    Each node should implement the execute() method with their specific logic.
    """
    
    def __init__(
        self,
        get_session: Callable[[], Any],
        get_llm: Callable[[], Any],
        get_embedder: Callable[[], Any],
        publisher: Any = None,
    ):
        """Initialize base node with dependencies.
        
        Args:
            get_session: Factory function for Neo4j session
            get_llm: Factory function for LLM client
            get_embedder: Factory function for embedder client
            publisher: Optional progress publisher for UI updates
        """
        self._get_session = get_session
        self._get_llm = get_llm
        self._get_embedder = get_embedder
        self._publisher = publisher
        self._logger = logger.bind(node=self.__class__.__name__)
    
    @abstractmethod
    async def execute(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """Execute node logic and return state updates.
        
        Args:
            state: Current graph state
            
        Returns:
            Dictionary with state updates to merge into graph state
        """
        raise NotImplementedError
    
    async def __call__(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """Make node callable for LangGraph integration.
        
        Args:
            state: Current graph state
            
        Returns:
            Dictionary with state updates
        """
        self._logger.debug("node_execution_start")
        result = await self.execute(state)
        self._logger.debug("node_execution_complete", updates=list(result.keys()))
        return result
    
    def _ensure_top_k(self, state: Dict[str, Any], default: int = 5) -> int:
        """Ensure top_k is valid positive integer.
        
        Args:
            state: Graph state
            default: Default value if missing/invalid
            
        Returns:
            Validated top_k value
        """
        try:
            val = int(state.get("top_k") or default)
            return max(1, val)
        except Exception:
            self._logger.warning("top_k_parse_failed", raw_value=state.get("top_k"), fallback=default)
            return default

