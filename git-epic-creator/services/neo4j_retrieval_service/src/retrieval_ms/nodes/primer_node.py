"""Primer node for community retrieval and initial response generation.

Implements DRIFT primer phase:
1. Fetch relevant communities using vector search
2. Get community summaries/briefs
3. Generate initial answer and followup questions
"""

from typing import Any, Dict, List
from uuid import UUID
import json
from contextlib import contextmanager
import structlog

from retrieval_ms.nodes.base_node import BaseNode
from retrieval_ms.prompts import primer_prompt
from retrieval_ms.repositories.neo4j_repository import Neo4jRepository
from retrieval_ms.response_models import PrimerResponse
from models.progress_messages import RetrievalStatus
from utils.json_utils import parse_and_validate
from config import get_retrieval_settings

logger = structlog.get_logger(__name__)


class PrimerNode(BaseNode):
    """Retrieve communities and generate primer response with followup questions."""
    
    @contextmanager
    def _repo_ctx(self):
        """Context manager for Neo4j repository."""
        with self._get_session() as session:
            yield Neo4jRepository(session)
    
    def _fetch_communities(
        self,
        repo: Neo4jRepository,
        index_name: str,
        k: int,
        qvec: List[float],
        project_id: str,
    ) -> List[int]:
        """Fetch communities using hierarchical level filtering.
        
        Strategy:
        1. Query highest-level communities first (global summaries)
        2. Fall back to lower levels if insufficient results
        3. Ensures DRIFT algorithm starts with aggregate context
        """
        # Get max hierarchy level for project
        max_level = repo.get_max_community_level(project_id)
        
        if max_level > 0:
            # Query at highest level first (aggregate communities)
            logger.info("fetching_communities_by_level", level=max_level, k=k, project_id=project_id)
            rows = repo.vector_query_communities_by_level(index_name, k, qvec, project_id, level=max_level)
            
            # If insufficient results, fall back to next level
            if len(rows) < k // 2 and max_level > 0:
                logger.info(
                    "insufficient_top_level_communities",
                    found=len(rows),
                    needed=k,
                    falling_back_to_level=max_level - 1
                )
                additional_rows = repo.vector_query_communities_by_level(
                    index_name,
                    k - len(rows),
                    qvec,
                    project_id,
                    level=max_level - 1
                )
                rows.extend(additional_rows)
        else:
            # No hierarchy or flat structure - use all levels
            logger.info("fetching_communities_all_levels", project_id=project_id)
            rows = repo.vector_query_nodes(index_name, k, qvec, project_id)
        
        communities: List[int] = []
        for r in rows:
            node = r.get("node")
            if node is not None:
                communities.append(int(node["community"]))
        
        logger.info("communities_fetched", count=len(communities), max_level=max_level, project_id=project_id)
        return communities
    
    def _fetch_community_brief(
        self,
        repo: Neo4jRepository,
        communities: List[int],
        project_id: str,
    ) -> List[Dict[str, Any]]:
        """Fetch community summaries and briefs."""
        if not communities:
            return []
        
        summaries: Dict[int, str] = repo.fetch_community_summaries(communities, project_id)
        brief = repo.fetch_communities_brief(communities, project_id)
        
        if brief:
            return brief
        
        return [{"id": cid, "summary": summaries.get(cid, "")} for cid in communities]
    
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
        with self._repo_ctx() as repo:
            communities = self._fetch_communities(
                repo,
                settings.vector_index.COMMUNITY_VECTOR_INDEX_NAME,
                k,
                state["qvec"],
                state["project_id"],
            )
            community_brief = self._fetch_community_brief(repo, communities, state["project_id"])
        
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
        if self._publisher:
            try:
                prompt_id_uuid = UUID(state["prompt_id"]) if state.get("prompt_id") else None
                await self._publisher.publish_retrieval_update(
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

