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
        get_repo: Callable[[], Any],
        get_llm: Callable[[], Any],
        get_embedder: Callable[[], Any],
    ):
        """Initialize base node with dependencies.
        
        Args:
            get_repo: Factory function for Neo4j repository client (HTTP -> neo4j_repository_service)
            get_llm: Factory function for LLM client
            get_embedder: Factory function for embedder client
        """
        self._get_repo = get_repo
        self._get_llm = get_llm
        self._get_embedder = get_embedder
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

    @staticmethod
    def _publisher_from_state(state: Dict[str, Any]) -> Any:
        """Get per-request progress publisher from state (if present).
        
        The retrieval graph is cached (singleton) for performance. Storing per-request
        objects (like Redis publishers) on node instances is unsafe and causes cross-request
        leakage. Publisher must be passed via state instead.
        """
        return state.get("publisher")

