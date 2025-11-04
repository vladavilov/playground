"""HyDE (Hypothetical Document Embeddings) node for query expansion.

Implements query expansion using HyDE technique:
1. Generate hypothetical answer to query
2. Combine query + answer for embedding
3. Embed for vector search
"""

from typing import Any, Dict
from uuid import UUID
import structlog

from retrieval_ms.nodes.base_node import BaseNode
from retrieval_ms.prompts import hyde_prompt, build_hyde_embed_text
from models.progress_messages import RetrievalStatus
from utils.json_utils import extract_string_content
from utils.embedding_service import EmbeddingService
from config import get_retrieval_settings

logger = structlog.get_logger(__name__)


class HydeNode(BaseNode):
    """Generate query expansion using HyDE for improved vector search."""
    
    async def execute(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """Execute HyDE query expansion.
        
        Args:
            state: Graph state with question
            
        Returns:
            State updates with qvec (query embedding vector)
        """
        llm = self._get_llm()
        embedder = self._get_embedder()
        settings = get_retrieval_settings()
        
        # Generate hypothetical answer
        prompt = hyde_prompt()
        msg = prompt.format_messages(question=state["question"])
        hyde_response = await llm.ainvoke(msg)
        hyde_answer = extract_string_content(hyde_response)
        
        # Build combined text for embedding
        hyde_text = build_hyde_embed_text(state["question"], hyde_answer)
        
        # Generate embedding with validation
        embedding_service = EmbeddingService(
            embedder,
            expected_dimensions=settings.vector_index.VECTOR_INDEX_DIMENSIONS
        )
        
        # Use request-level cache from state
        cache = state.get("_cache_embeddings", {})
        qvec = await embedding_service.embed_with_cache(hyde_text, cache)
        
        # Validate dimension
        embedding_service.validate_dimension(qvec, "HyDE")
        
        logger.info(
            "retrieval.hyde.done",
            hyde_text_len=len(hyde_text),
            qvec_len=len(qvec)
        )
        
        # Publish query expansion status
        if self._publisher:
            try:
                prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                await self._publisher.publish_retrieval_update(
                    project_id=UUID(state["project_id"]),
                    retrieval_id=state["retrieval_id"],
                    phase=RetrievalStatus.EXPANDING_QUERY,
                    thought_summary="📝 **Expanding Query**",
                    details_md=f"**Using HyDE (Hypothetical Document Embeddings) to improve search precision**\n\nHyDE answer: {hyde_answer[:300]}...",
                    progress_pct=20.0,
                    prompt_id=prompt_id_uuid,
                )
            except Exception as exc:
                logger.debug("publish_failed", phase="hyde", error=str(exc))
        
        return {
            "qvec": qvec,
            "_cache_embeddings": cache,
        }

