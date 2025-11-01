"""Redis pub/sub progress notifier for embedding caching tasks."""

from typing import Optional, List
import structlog
import redis.asyncio as redis
from pydantic import BaseModel

from constants.streams import UI_PROJECT_PROGRESS_NAME
from utils.redis_progress_publisher import RedisProgressPublisher

logger = structlog.get_logger(__name__)


class EmbeddingProgressMessage(BaseModel):
    """Progress message for embedding caching operations."""
    project_id: str
    status: str  # 'processing', 'completed', 'error'
    process_step: str
    processed_pct: Optional[float] = None
    project_index: Optional[int] = None
    total_projects: Optional[int] = None
    error_message: Optional[str] = None
    error_tip: Optional[str] = None
    scanned: Optional[int] = None
    total: Optional[int] = None
    cached: Optional[int] = None


class ProgressNotifier(RedisProgressPublisher):
    """Publishes embedding caching progress events to Redis pub/sub.
    
    Publishes to channel: ui:project_progress
    Inherits common publish infrastructure from RedisProgressPublisher.
    """
    
    def __init__(self, redis_client: redis.Redis):
        """
        Initialize progress notifier.
        
        Args:
            redis_client: Configured Redis client
        """
        super().__init__(
            redis_client=redis_client,
            default_channel_name=UI_PROJECT_PROGRESS_NAME
        )
    
    async def notify_started(
        self,
        project_id: str,
        project_index: Optional[int] = None,
        total_projects: Optional[int] = None
    ) -> bool:
        """Notify that embedding caching has started for a project."""
        step = "Caching embeddings: Starting..."
        if project_index and total_projects:
            step = f"Caching embeddings (Project {project_index}/{total_projects}): Starting..."
        
        message = EmbeddingProgressMessage(
            project_id=project_id,
            status="processing",
            process_step=step,
            processed_pct=0.0,
            project_index=project_index,
            total_projects=total_projects
        )
        return await self._publish_message(message)
    
    async def notify_progress(
        self,
        project_id: str,
        scanned: int,
        total: int,
        project_index: Optional[int] = None,
        total_projects: Optional[int] = None
    ) -> bool:
        """Notify progress in scanning work items."""
        pct = (scanned / total * 33.0) if total > 0 else 0.0  # 0-33% for scanning
        
        step = f"Caching embeddings: Fetching backlog items ({scanned}/{total})..."
        if project_index and total_projects:
            step = f"Caching embeddings (Project {project_index}/{total_projects}): Fetching items ({scanned}/{total})..."
        
        message = EmbeddingProgressMessage(
            project_id=project_id,
            status="processing",
            process_step=step,
            processed_pct=pct,
            project_index=project_index,
            total_projects=total_projects,
            scanned=scanned,
            total=total
        )
        return await self._publish_message(message)
    
    async def notify_embedded(
        self,
        project_id: str,
        completed: int,
        total: int,
        project_index: Optional[int] = None,
        total_projects: Optional[int] = None
    ) -> bool:
        """Notify progress in embedding generation."""
        pct = 33.0 + (completed / total * 34.0) if total > 0 else 33.0  # 33-67% for embedding
        
        step = f"Caching embeddings: Generating embeddings ({completed}/{total})..."
        if project_index and total_projects:
            step = f"Caching embeddings (Project {project_index}/{total_projects}): Generating embeddings ({completed}/{total})..."
        
        message = EmbeddingProgressMessage(
            project_id=project_id,
            status="processing",
            process_step=step,
            processed_pct=pct,
            project_index=project_index,
            total_projects=total_projects
        )
        return await self._publish_message(message)
    
    async def notify_cached(
        self,
        project_id: str,
        cached: int,
        project_index: Optional[int] = None,
        total_projects: Optional[int] = None
    ) -> bool:
        """Notify that embeddings have been cached."""
        step = f"Caching embeddings: Storing in cache ({cached} items)..."
        if project_index and total_projects:
            step = f"Caching embeddings (Project {project_index}/{total_projects}): Stored {cached} items"
        
        message = EmbeddingProgressMessage(
            project_id=project_id,
            status="processing",
            process_step=step,
            processed_pct=90.0,  # 67-90% for caching
            project_index=project_index,
            total_projects=total_projects,
            cached=cached
        )
        return await self._publish_message(message)
    
    async def notify_completed(
        self,
        project_id: str,
        project_index: Optional[int] = None,
        total_projects: Optional[int] = None
    ) -> bool:
        """Notify that embedding caching is complete for a project."""
        step = "Caching embeddings: Completed"
        if project_index and total_projects:
            step = f"Caching embeddings (Project {project_index}/{total_projects}): Completed"
        
        message = EmbeddingProgressMessage(
            project_id=project_id,
            status="completed",
            process_step=step,
            processed_pct=100.0,
            project_index=project_index,
            total_projects=total_projects
        )
        return await self._publish_message(message)
    
    async def notify_all_completed(
        self,
        project_ids: List[str],
        success_count: int,
        error_count: int
    ) -> bool:
        """Notify that all projects have been processed."""
        total = len(project_ids)
        step = f"Embedding caching completed: {success_count} successful, {error_count} failed out of {total} total"
        
        # Use first project ID as representative (UI filters by project anyway)
        message = EmbeddingProgressMessage(
            project_id=project_ids[0] if project_ids else "multi",
            status="completed" if error_count == 0 else "completed_with_errors",
            process_step=step,
            processed_pct=100.0,
            total_projects=total
        )
        return await self._publish_message(message)
    
    async def notify_error(
        self,
        project_id: str,
        error_message: str,
        error_tip: Optional[str] = None,
        project_index: Optional[int] = None,
        total_projects: Optional[int] = None
    ) -> bool:
        """Notify that an error occurred."""
        step = f"Caching embeddings: Error - {error_message}"
        if project_index and total_projects:
            step = f"Caching embeddings (Project {project_index}/{total_projects}): Error"
        
        message = EmbeddingProgressMessage(
            project_id=project_id,
            status="error",
            process_step=step,
            error_message=error_message,
            error_tip=error_tip,
            project_index=project_index,
            total_projects=total_projects
        )
        return await self._publish_message(message)


