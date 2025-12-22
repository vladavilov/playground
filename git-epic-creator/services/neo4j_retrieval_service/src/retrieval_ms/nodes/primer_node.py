"""Primer node for community retrieval and initial response generation.

Implements DRIFT primer phase:
1. Fetch relevant communities using vector search
2. Get community summaries/briefs
3. Generate initial answer and followup questions
"""

from typing import Any, Dict, List
from uuid import UUID
import json
import structlog

from retrieval_ms.nodes.base_node import BaseNode
from retrieval_ms.prompts import primer_prompt
import httpx
from retrieval_ms.neo4j_repository_service_client import post_json
from retrieval_ms.response_models import PrimerResponse
from models.progress_messages import RetrievalStatus
from utils.json_utils import parse_and_validate
from config import get_retrieval_settings

logger = structlog.get_logger(__name__)


class PrimerNode(BaseNode):
    """Retrieve communities and generate primer response with followup questions."""
    
    async def _fetch_primer_context(
        self,
        client: httpx.AsyncClient,
        index_name: str,
        k: int,
        qvec: List[float],
        project_id: str,
    ) -> tuple[List[int], List[Dict[str, Any]]]:
        """Fetch primer context (communities + brief) in one backend call."""
        data = await post_json(
            client,
            "/v1/retrieval/primer-context",
            {
                "project_id": project_id,
                "community_index_name": index_name,
                "k": int(k),
                "qvec": qvec,
            },
        )
        communities = [int(x) for x in (data.get("communities") or []) if x is not None]
        brief = list(data.get("community_brief") or [])
        return communities, brief
    
    async def execute(self, state: Dict[str, Any]) -> Dict[str, Any]:
        """Execute primer phase: fetch communities and generate followups.
        
        Args:
            state: Graph state with question, qvec, project_id, top_k
            
        Returns:
            State updates with communities, community_brief, primer_json, followups
        """
        settings = get_retrieval_settings()
        k = self._ensure_top_k(state)
        
        # Fetch communities and briefs
        client = self._get_repo()
        communities, community_brief = await self._fetch_primer_context(
            client,
            settings.vector_index.COMMUNITY_VECTOR_INDEX_NAME,
            k,
            state["qvec"],
            state["project_id"],
        )

        # Early exit: without communities, followup chunk retrieval cannot proceed.
        # This avoids spending tokens on primer LLM when graph has no relevant data.
        if not communities:
            logger.warning(
                "retrieval.primer.no_communities",
                project_id=state.get("project_id"),
                top_k=k,
                message="No relevant communities found; short-circuiting retrieval",
            )
            return {
                "communities": [],
                "community_brief": [],
                "primer_json": {"initial_answer": "", "followups": [], "rationale": ""},
                "followups": [],
                "no_data_found": True,
            }
        
        # Generate primer response with LLM
        llm = self._get_llm()
        prompt = primer_prompt()
        msg = prompt.format_messages(
            question=state["question"],
            community_details=json.dumps(community_brief),
        )
        
        primer_response = await llm.ainvoke(msg)
        
        # Validate and normalize primer response
        primer_validated = parse_and_validate(
            primer_response,
            PrimerResponse,
            "Primer",
        )
        
        # Convert followups to dict format for state
        followups = [f.model_dump() for f in primer_validated.followups]
        
        logger.info(
            "retrieval.primer.done",
            communities=len(communities),
            community_brief=len(community_brief),
            followups=len(followups),
        )
        
        # Publish community retrieval status
        publisher = self._publisher_from_state(state)
        if publisher:
            try:
                prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                await publisher.publish_retrieval_update(
                    project_id=UUID(state["project_id"]),
                    retrieval_id=state["retrieval_id"],
                    phase=RetrievalStatus.RETRIEVING_COMMUNITIES,
                    thought_summary=f"🌐 **Retrieved {len(communities)} Knowledge Communities**",
                    details_md=(
                        f"*Communities found:* {len(communities)}  \n"
                        + "*Follow-up questions:*  \n"
                        + "\n".join([f"- {f.get('question','')}" for f in followups])
                        if followups else ""
                    ),
                    progress_pct=40.0,
                    prompt_id=prompt_id_uuid,
                )
            except Exception as exc:
                logger.debug("publish_failed", phase="primer", error=str(exc))
        
        return {
            "communities": communities,
            "community_brief": community_brief,
            "primer_json": primer_validated.model_dump(),
            "followups": followups,
        }

