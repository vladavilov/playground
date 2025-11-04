"""Graph factory for creating DRIFT retrieval pipeline.

Creates LangGraph StateGraph with nodes for:
- init: Initialize session and caches
- hyde: HyDE query expansion
- primer: Community retrieval and primer
- followups: Parallel followup execution
- aggregate: Final aggregation
"""

from typing import Any, Callable, Dict, Optional, TypedDict
import structlog
from langgraph.graph import StateGraph, START, END
from langgraph.checkpoint.memory import InMemorySaver

from retrieval_ms.nodes import (
    InitNode,
    HydeNode,
    PrimerNode,
    FollowupsNode,
    AggregateNode,
)

logger = structlog.get_logger(__name__)


class GraphState(TypedDict, total=False):
    """State dictionary for DRIFT retrieval graph."""
    question: str
    top_k: int
    project_id: str
    prompt_id: Optional[str]  # Optional parent workflow prompt_id for UI tracking
    retrieval_id: Any  # UUID
    qvec: list[float]
    communities: list[int]
    community_brief: list[Dict[str, Any]]
    primer_json: Dict[str, Any]
    followups: list[Dict[str, Any]]
    followup_results: list[Dict[str, Any]]
    tree: Dict[str, Any]
    result_json: Dict[str, Any]
    no_data_found: bool  # Flag indicating no communities/chunks found
    # Request-level caches
    _cache_embeddings: Dict[str, list[float]]  # text -> embedding vector
    _cache_neighborhoods: Dict[str, Dict[str, Any]]  # chunk_id -> expanded data
    _seen_chunk_ids: set[str]  # Track seen chunks for novelty ordering


def create_retrieval_graph(
    get_session: Callable[[], Any],
    get_llm: Callable[[], Any],
    get_embedder: Callable[[], Any],
    publisher: Optional[Any] = None,
):
    """Create DRIFT retrieval graph with configured nodes.
    
    Args:
        get_session: Factory function for Neo4j session
        get_llm: Factory function for LLM client
        get_embedder: Factory function for embedder client
        publisher: Optional progress publisher for UI updates
        
    Returns:
        Compiled LangGraph StateGraph ready for execution
    """
    logger.info("graph_creation_start", message="Creating DRIFT retrieval graph")
    
    # Initialize nodes with dependencies
    init_node = InitNode(get_session, get_llm, get_embedder, publisher)
    hyde_node = HydeNode(get_session, get_llm, get_embedder, publisher)
    primer_node = PrimerNode(get_session, get_llm, get_embedder, publisher)
    followups_node = FollowupsNode(get_session, get_llm, get_embedder, publisher)
    aggregate_node = AggregateNode(get_session, get_llm, get_embedder, publisher)
    
    # Build graph
    builder = StateGraph(GraphState)
    
    # Add nodes (nodes are callable via __call__)
    builder.add_node("init", init_node)
    builder.add_node("hyde", hyde_node)
    builder.add_node("primer", primer_node)
    builder.add_node("followups", followups_node)
    builder.add_node("aggregate", aggregate_node)
    
    # Define edges (linear flow for DRIFT pipeline)
    builder.add_edge(START, "init")
    builder.add_edge("init", "hyde")
    builder.add_edge("hyde", "primer")
    builder.add_edge("primer", "followups")
    builder.add_edge("followups", "aggregate")
    builder.add_edge("aggregate", END)
    
    # Compile with in-memory checkpointer
    graph = builder.compile(checkpointer=InMemorySaver())
    
    logger.info("graph_creation_complete", message="DRIFT retrieval graph compiled successfully")
    
    return graph

