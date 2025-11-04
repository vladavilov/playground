"""Initialization node for retrieval pipeline.

Handles:
- Retrieval session ID generation
- Cache initialization (embeddings, neighborhoods)
- Progress publishing (initialization phase)
"""

from typing import Any, Dict
from uuid import UUID, uuid4
import structlog

from retrieval_ms.nodes.base_node import BaseNode
from models.progress_messages import RetrievalStatus

logger = structlog.get_logger(__name__)


class InitNode(BaseNode):
    """Initialize retrieval session with caches and progress tracking."""
    
    async def execute(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """Initialize retrieval session.
        
        Args:
            state: Graph state with question, project_id
            
        Returns:
            State updates with retrieval_id and initialized caches
        """
        logger.info("retrieval.init", question_len=len(state.get("question") or ""))
        
        # Generate retrieval session ID if not provided
        retrieval_id = state.get("retrieval_id") or uuid4()
        
        # Publish initialization status
        if self._publisher:
            try:
                prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                await self._publisher.publish_retrieval_update(
                    project_id=UUID(state["project_id"]),
                    retrieval_id=retrieval_id,
                    phase=RetrievalStatus.INITIALIZING,
                    thought_summary="Agentic retrieval...",
                    details_md="**Agentic retrieval**  \nTo find relevant information from the knowledge graph by provided question and intentions...  \n",
                    progress_pct=0.0,
                    prompt_id=prompt_id_uuid,
                )
            except Exception as exc:
                logger.debug("publish_failed", phase="init", error=str(exc))
        
        return {
            "retrieval_id": retrieval_id,
            "_cache_embeddings": {},
            "_cache_neighborhoods": {},
            "_seen_chunk_ids": set(),
        }

