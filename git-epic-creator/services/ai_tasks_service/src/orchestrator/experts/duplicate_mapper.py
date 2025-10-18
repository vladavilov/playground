"""DuplicateMapper expert - identifies similar work items in GitLab backlog."""

from typing import List, Dict, Any
import structlog
import numpy as np
from sklearn.metrics.pairwise import cosine_similarity
from openai import AzureOpenAI

from task_models.agent_models import BacklogDraft, DuplicateMappings
from task_models.backlog_models import Epic, Task, SimilarMatch
from config import get_ai_tasks_settings

logger = structlog.get_logger(__name__)


class DuplicateMapper:
    """Expert that maps generated work items to similar GitLab epics/issues using embeddings."""

    def __init__(self) -> None:
        settings = get_ai_tasks_settings()
        self.similarity_threshold = settings.SIMILARITY_THRESHOLD
        # Use deployment name for Azure API calls
        self.embed_model = settings.llm.embedding_deployment_name
        
        self.client = AzureOpenAI(
            api_key=settings.llm.OAI_KEY,
            api_version=settings.llm.OAI_API_VERSION,
            azure_endpoint=settings.llm.OAI_BASE_URL,
        )

    async def map_duplicates(
        self,
        draft: BacklogDraft,
        gitlab_backlog: Dict[str, List[Dict[str, Any]]],
    ) -> DuplicateMappings:
        """Map generated epics/tasks to similar GitLab work items.
        
        Args:
            draft: Generated backlog draft
            gitlab_backlog: Dict with 'epics' and 'issues' lists from GitLab
                           (items should have 'title_embedding' from gitlab_client_service)
            
        Returns:
            DuplicateMappings with enriched epics containing similar matches
        """
        gitlab_epics = gitlab_backlog.get("epics", [])
        gitlab_issues = gitlab_backlog.get("issues", [])
        
        # Early return if no GitLab items
        if not gitlab_epics and not gitlab_issues:
            return DuplicateMappings(
                enriched_epics=draft.epics,
                stats={"total_matches": 0, "avg_similarity": 0.0},
            )
        
        # Compute embeddings ONLY for generated items (titles only, to match GitLab backlog)
        generated_texts = []
        generated_refs = []  # (epic_idx, task_idx or None)
        
        for epic_idx, epic in enumerate(draft.epics):
            generated_texts.append(epic.title)
            generated_refs.append((epic_idx, None))
            
            for task_idx, task in enumerate(epic.tasks):
                generated_texts.append(task.title)
                generated_refs.append((epic_idx, task_idx))
        
        # Extract embeddings from GitLab items (already computed by gitlab_client_service)
        gitlab_embeddings = []
        gitlab_refs = []  # (kind, item_dict)
        
        for item in gitlab_epics:
            # Reuse embedding from gitlab_client_service if available
            embedding = item.get("title_embedding", [])
            if embedding:
                gitlab_embeddings.append(embedding)
                gitlab_refs.append(("epic", item))
        
        for item in gitlab_issues:
            # Reuse embedding from gitlab_client_service if available
            embedding = item.get("title_embedding", [])
            if embedding:
                gitlab_embeddings.append(embedding)
                gitlab_refs.append(("issue", item))
        
        # Early return if no GitLab embeddings available
        if not gitlab_embeddings:
            logger.warning(
                "No embeddings found in GitLab backlog",
                epics_count=len(gitlab_epics),
                issues_count=len(gitlab_issues),
            )
            return DuplicateMappings(
                enriched_epics=draft.epics,
                stats={"total_matches": 0, "no_gitlab_embeddings": True},
            )
        
        # Get embeddings for generated items only
        try:
            generated_embeddings = self._get_embeddings(generated_texts)
            logger.info(
                "Generated embeddings for new items",
                generated_count=len(generated_embeddings),
                gitlab_reused_count=len(gitlab_embeddings),
            )
        except Exception as e:
            logger.warning("Embedding generation failed for new items", error=str(e))
            return DuplicateMappings(
                enriched_epics=draft.epics,
                stats={"total_matches": 0, "error": str(e)},
            )
        
        # Compute all similarities at once using sklearn (vectorized)
        generated_matrix = np.array(generated_embeddings)
        gitlab_matrix = np.array(gitlab_embeddings)
        similarity_matrix = cosine_similarity(generated_matrix, gitlab_matrix)
        
        # Build enriched epics with similar matches
        enriched_epics = []
        total_matches = 0
        total_similarity = 0.0
        
        for epic_idx, epic in enumerate(draft.epics):
            enriched_epic = epic.model_copy(deep=True)
            enriched_tasks = []
            
            # Find similar epics for this epic
            gen_ref = (epic_idx, None)
            gen_idx = generated_refs.index(gen_ref)
            
            similar_epics = self._find_similar_from_matrix(
                gen_idx,
                similarity_matrix,
                gitlab_refs,
                self.similarity_threshold,
            )
            
            if similar_epics:
                enriched_epic.similar = similar_epics
                total_matches += len(similar_epics)
                total_similarity += sum(m.similarity for m in similar_epics)
            
            # Find similar issues for each task
            for task_idx, task in enumerate(epic.tasks):
                gen_ref = (epic_idx, task_idx)
                gen_idx = generated_refs.index(gen_ref)
                
                similar_tasks = self._find_similar_from_matrix(
                    gen_idx,
                    similarity_matrix,
                    gitlab_refs,
                    self.similarity_threshold,
                )
                
                enriched_task = task.model_copy(deep=True)
                if similar_tasks:
                    enriched_task.similar = similar_tasks
                    total_matches += len(similar_tasks)
                    total_similarity += sum(m.similarity for m in similar_tasks)
                
                enriched_tasks.append(enriched_task)
            
            enriched_epic.tasks = enriched_tasks
            enriched_epics.append(enriched_epic)
        
        avg_similarity = (total_similarity / total_matches) if total_matches > 0 else 0.0
        
        return DuplicateMappings(
            enriched_epics=enriched_epics,
            stats={
                "total_matches": total_matches,
                "avg_similarity": avg_similarity,
            },
        )

    def _get_embeddings(self, texts: List[str]) -> List[List[float]]:
        """Get embeddings for list of texts.
        
        Note: Texts should be titles only to match GitLab's embedding strategy.
        """
        response = self.client.embeddings.create(
            model=self.embed_model,
            input=texts,
        )
        return [item.embedding for item in response.data]

    def _find_similar_from_matrix(
        self,
        query_idx: int,
        similarity_matrix: np.ndarray,
        gitlab_refs: List[tuple],
        threshold: float,
    ) -> List[SimilarMatch]:
        """Find similar items above threshold from precomputed similarity matrix."""
        similarities = similarity_matrix[query_idx]
        
        # Get indices above threshold
        above_threshold = np.where(similarities >= threshold)[0]
        
        # Create matches
        matches = [
            SimilarMatch(
                kind=gitlab_refs[idx][0],
                id=str(gitlab_refs[idx][1].get("id", "")),
                status=gitlab_refs[idx][1].get("state") or gitlab_refs[idx][1].get("status"),
                similarity=float(similarities[idx]),
                url=gitlab_refs[idx][1].get("web_url"),
            )
            for idx in above_threshold
        ]
        
        # Sort by similarity descending, return top 3
        return sorted(matches, key=lambda m: m.similarity, reverse=True)[:3]


