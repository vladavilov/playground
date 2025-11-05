"""DuplicateMapper expert - identifies similar work items in GitLab backlog."""

from typing import List, Dict, Any
import structlog
import numpy as np
from sklearn.metrics.pairwise import cosine_similarity

from task_models.agent_models import BacklogDraft, DuplicateMappings
from task_models.backlog_models import Epic, Task, SimilarMatch
from config import get_ai_tasks_settings
from utils.llm_client_factory import create_embedder
from utils.embedding_service import EmbeddingService

logger = structlog.get_logger(__name__)


class DuplicateMapper:
    """Expert that maps generated work items to similar GitLab epics/issues using embeddings."""

    def __init__(self) -> None:
        settings = get_ai_tasks_settings()
        self.similarity_threshold = settings.SIMILARITY_THRESHOLD
        
        # Use shared embedding service for error handling and logging
        embedder = create_embedder()
        self.embedding_service = EmbeddingService(embedder)

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
            embedding = item.get("title_embedding", [])
            if embedding:
                gitlab_embeddings.append(embedding)
                gitlab_refs.append(("epic", item))
        
        for item in gitlab_issues:
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
        
        # Get embeddings for generated items using shared embedding service
        # EmbeddingService handles error logging and HTTPException conversion
        try:
            generated_embeddings = await self.embedding_service.embed_batch(generated_texts)
            logger.info(
                "duplicate_mapping_embeddings_generated",
                count=len(generated_texts),
                embedding_dim=len(generated_embeddings[0]) if generated_embeddings else 0,
            )
        except Exception as exc:
            # EmbeddingService already logged the error, just return gracefully
            return DuplicateMappings(
                enriched_epics=draft.epics,
                stats={"total_matches": 0, "embedding_error": str(exc)},
            )
        
        # Compute similarity matrix
        try:
            similarity_matrix = cosine_similarity(generated_embeddings, gitlab_embeddings)
            logger.debug(
                "Computed similarity matrix",
                shape=similarity_matrix.shape,
                max_similarity=float(similarity_matrix.max()) if similarity_matrix.size > 0 else 0.0,
            )
        except Exception as exc:
            logger.error(
                "Similarity computation failed",
                error=str(exc),
                error_type=type(exc).__name__,
            )
            return DuplicateMappings(
                enriched_epics=draft.epics,
                stats={"total_matches": 0, "similarity_error": str(exc)},
            )
        
        # Build enriched epics with similar matches
        enriched_epics = []
        total_matches = 0
        similarity_scores = []
        
        for epic_idx, epic in enumerate(draft.epics):
            # Get generated item indices for this epic
            epic_item_indices = [i for i, (ei, ti) in enumerate(generated_refs) if ei == epic_idx and ti is None]
            task_item_indices = [i for i, (ei, ti) in enumerate(generated_refs) if ei == epic_idx and ti is not None]
            
            # Find similar matches for epic
            epic_similar = []
            if epic_item_indices:
                epic_item_idx = epic_item_indices[0]
                similarities = similarity_matrix[epic_item_idx]
                for gitlab_idx, score in enumerate(similarities):
                    if score >= self.similarity_threshold:
                        kind, gitlab_item = gitlab_refs[gitlab_idx]
                        epic_similar.append(
                            SimilarMatch(
                                id=str(gitlab_item.get("id", "")),
                                iid=str(gitlab_item.get("iid", "")),
                                kind=kind,
                                title=gitlab_item.get("title", ""),
                                project_id=str(gitlab_item.get("project_id", "")),
                                status=gitlab_item.get("state"),
                                similarity=float(score),
                                url=gitlab_item.get("web_url", ""),
                            )
                        )
                        total_matches += 1
                        similarity_scores.append(float(score))
            
            # Sort epic matches by similarity
            epic_similar.sort(key=lambda x: x.similarity, reverse=True)
            
            # Find similar matches for tasks
            enriched_tasks = []
            for task_idx, task in enumerate(epic.tasks):
                task_item_indices_for_this_task = [
                    i for i, (ei, ti) in enumerate(generated_refs)
                    if ei == epic_idx and ti == task_idx
                ]
                
                task_similar = []
                if task_item_indices_for_this_task:
                    task_item_idx = task_item_indices_for_this_task[0]
                    similarities = similarity_matrix[task_item_idx]
                    for gitlab_idx, score in enumerate(similarities):
                        if score >= self.similarity_threshold:
                            kind, gitlab_item = gitlab_refs[gitlab_idx]
                            task_similar.append(
                                SimilarMatch(
                                    id=str(gitlab_item.get("id", "")),
                                    iid=str(gitlab_item.get("iid", "")),
                                    kind=kind,
                                    title=gitlab_item.get("title", ""),
                                    project_id=str(gitlab_item.get("project_id", "")),
                                    status=gitlab_item.get("state"),
                                    similarity=float(score),
                                    url=gitlab_item.get("web_url", ""),
                                )
                            )
                            total_matches += 1
                            similarity_scores.append(float(score))
                
                # Sort task matches by similarity
                task_similar.sort(key=lambda x: x.similarity, reverse=True)
                
                # Create enriched task
                enriched_tasks.append(
                    Task(
                        id=task.id,
                        title=task.title,
                        description=task.description,
                        acceptance_criteria=task.acceptance_criteria,
                        dependencies=task.dependencies,
                        similar=task_similar,
                    )
                )
            
            # Create enriched epic
            enriched_epics.append(
                Epic(
                    id=epic.id,
                    title=epic.title,
                    description=epic.description,
                    tasks=enriched_tasks,
                    similar=epic_similar,
                )
            )
        
        avg_similarity = float(np.mean(similarity_scores)) if similarity_scores else 0.0
        
        logger.info(
            "Duplicate mapping complete",
            total_matches=total_matches,
            avg_similarity=avg_similarity,
            threshold=self.similarity_threshold,
        )
        
        return DuplicateMappings(
            enriched_epics=enriched_epics,
            stats={
                "total_matches": total_matches,
                "avg_similarity": avg_similarity,
                "threshold": self.similarity_threshold,
            },
        )
