"""GitLab API router with normalized endpoints."""

from typing import Optional
import structlog
from fastapi import APIRouter, Depends, Query, HTTPException, status, BackgroundTasks
import gitlab

from models import (
    ListResponse, 
    ApplyBacklogRequest, 
    ApplyBacklogResponse, 
    ApplyBacklogResults,
    ApplyBacklogItemResult,
    ApplyBacklogError,
    ResolveProjectRequest,
    ResolveProjectResponse
)
from config import GitLabClientSettings, get_gitlab_client_settings
from dependencies import get_gitlab_client_dep, get_redis_client_dep
from services.gitlab_client_service import GitLabClientService
from services.embedding_client import EmbeddingClient
from services.redis_cache_client import RedisCacheClient
from services.progress_notifier import ProgressNotifier
from services.idempotency_store import IdempotencyStore
from services.error_mapper import map_gitlab_error
from services.error_tips import get_error_tip_for_gitlab_exception
import redis.asyncio as redis

logger = structlog.get_logger(__name__)
router = APIRouter(prefix="/gitlab", tags=["GitLab"])

# Global idempotency store (in-memory)
_idempotency_store: Optional[IdempotencyStore] = None


def get_idempotency_store(
    settings: GitLabClientSettings = Depends(get_gitlab_client_settings)
) -> IdempotencyStore:
    """Get or create idempotency store."""
    global _idempotency_store
    if _idempotency_store is None:
        _idempotency_store = IdempotencyStore(settings)
    return _idempotency_store


@router.get("/projects/{project_id}/backlog", response_model=ListResponse)
async def get_project_backlog(
    project_id: str,
    gitlab_client: gitlab.Gitlab = Depends(get_gitlab_client_dep),
    redis_client: redis.Redis = Depends(get_redis_client_dep),
    settings: GitLabClientSettings = Depends(get_gitlab_client_settings),
    labels: Optional[str] = Query(None, description="Comma-separated labels to filter by"),
    state: str = Query("opened", description="Filter by state: opened, closed, all"),
    page: int = Query(1, ge=1, description="Page number"),
    per_page: Optional[int] = Query(None, ge=1, le=1000, description="Items per page")
):
    """
    Get project backlog (epics + issues) with cached embeddings.
    
    Returns normalized work items with title embeddings from cache if available.
    """
    try:
        gitlab_service = GitLabClientService(gitlab_client, settings)
        cache_client = RedisCacheClient(redis_client)
        
        # Parse labels
        label_list = [l.strip() for l in labels.split(",")] if labels else None
        
        # Get backlog from GitLab
        response = gitlab_service.list_project_backlog(
            project_id=project_id,
            labels=label_list,
            state=state,
            page=page,
            per_page=per_page
        )
        
        # Enrich with cached embeddings and titles
        if response.items:
            work_item_ids = [item.id for item in response.items]
            cached_data = await cache_client.get_embeddings_bulk(
                project_id=project_id,
                work_item_ids=work_item_ids
            )
            
            for item in response.items:
                data = cached_data.get(item.id)
                if data:
                    # Update title from cache if available (useful for stale GitLab data)
                    cached_title = data.get("title", "")
                    if cached_title:
                        item.title = cached_title
                    # Add embedding
                    item.title_embedding = data.get("embedding", [])
        
        logger.info(
            "Backlog retrieved",
            project_id=project_id,
            item_count=len(response.items),
            embeddings_found=sum(1 for item in response.items if item.title_embedding)
        )
        
        return response
        
    except Exception as e:
        logger.error(
            "Failed to retrieve backlog",
            project_id=project_id,
            error=str(e),
            error_type=type(e).__name__
        )
        error_response = map_gitlab_error(e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=error_response["error"]
        )


@router.post("/projects/{project_id}/apply-backlog", response_model=ApplyBacklogResponse)
async def apply_backlog(
    project_id: str,
    request: ApplyBacklogRequest,
    gitlab_client: gitlab.Gitlab = Depends(get_gitlab_client_dep),
    settings: GitLabClientSettings = Depends(get_gitlab_client_settings),
    idempotency_store: IdempotencyStore = Depends(get_idempotency_store)
):
    """
    Apply a generated backlog to GitLab with idempotent create/update.
    
    Creates or updates epics and issues based on provided data.
    Returns results indicating which items were created, updated, or unchanged.
    
    Args:
        project_id: GitLab project ID (numeric or namespace/project path) - used in URL
        request: Backlog application request with epics, issues, and tracking IDs
    
    Note: request.internal_project_id can be used for tracking and logging operations
    back to the internal project management system.
    """
    logger.info(
        "Applying backlog to GitLab",
        gitlab_project_id=project_id,
        internal_project_id=request.internal_project_id,
        prompt_id=request.prompt_id,
        epics_count=len(request.epics),
        issues_count=len(request.issues)
    )
    
    try:
        # Check idempotency
        if request.prompt_id:
            cached_result = idempotency_store.get(project_id, request.prompt_id)
            if cached_result is not None:
                logger.info(
                    "Returning cached apply-backlog result",
                    project_id=project_id,
                    prompt_id=request.prompt_id
                )
                return cached_result
        
        gitlab_service = GitLabClientService(gitlab_client, settings)
        
        results = ApplyBacklogResults()
        errors = []
        
        # Resolve group for epics
        group = gitlab_service._get_group_for_project(project_id)
        
        # Track created epic IIDs for linking issues to parent epics
        epic_iid_map = {}  # input_index -> created_epic_iid
        
        # Process epics - always create new ones
        for idx, epic_data in enumerate(request.epics):
            try:
                if not group:
                    errors.append(ApplyBacklogError(
                        scope="epic",
                        input_index=idx,
                        message="Project has no group for epics"
                    ))
                    continue
                
                # Create new epic
                work_item = gitlab_service.create_epic(
                    group_id=str(group.id),
                    title=epic_data.title,
                    description=epic_data.description,
                    labels=epic_data.labels
                )
                
                # Store IID for linking issues to this epic
                epic_iid_map[idx] = work_item.id
                
                results.epics.append(ApplyBacklogItemResult(
                    input_index=idx,
                    action="created",
                    id=work_item.id,
                    web_url=work_item.web_url
                ))
                
                # If user accepted a similar epic, create link to it
                if epic_data.related_to_iid:
                    try:
                        gitlab_service.create_epic_link(
                            group_id=str(group.id),
                            source_epic_iid=work_item.id,
                            target_epic_iid=epic_data.related_to_iid,
                            link_type="relates_to"
                        )
                        logger.info(
                            "Epic linked to similar item",
                            source_epic_iid=work_item.id,
                            target_epic_iid=epic_data.related_to_iid
                        )
                    except Exception as link_error:
                        logger.warning(
                            "Failed to link epic to similar item",
                            source_epic_iid=work_item.id,
                            target_epic_iid=epic_data.related_to_iid,
                            error=str(link_error)
                        )
                        # Don't fail the whole operation, just log warning
            
            except Exception as e:
                logger.error(
                    "Failed to process epic",
                    project_id=project_id,
                    epic_index=idx,
                    error=str(e)
                )
                errors.append(ApplyBacklogError(
                    scope="epic",
                    input_index=idx,
                    message=str(e)
                ))
        
        # Process issues - always create new ones
        for idx, issue_data in enumerate(request.issues):
            try:
                # Create new issue
                work_item = gitlab_service.create_issue(
                    project_id=project_id,
                    title=issue_data.title,
                    description=issue_data.description,
                    labels=issue_data.labels
                )
                
                results.issues.append(ApplyBacklogItemResult(
                    input_index=idx,
                    action="created",
                    id=work_item.id,
                    web_url=work_item.web_url
                ))
                
                # Link to parent epic if specified
                if issue_data.parent_epic_index is not None and group:
                    parent_epic_iid = epic_iid_map.get(issue_data.parent_epic_index)
                    if parent_epic_iid:
                        try:
                            gitlab_service.link_issue_to_epic(
                                group_id=str(group.id),
                                epic_iid=parent_epic_iid,
                                issue_id=work_item.id  # Use issue.id (not iid)
                            )
                            logger.info(
                                "Issue linked to parent epic",
                                issue_id=work_item.id,
                                epic_iid=parent_epic_iid
                            )
                        except Exception as link_error:
                            logger.warning(
                                "Failed to link issue to parent epic",
                                issue_id=work_item.id,
                                epic_iid=parent_epic_iid,
                                error=str(link_error)
                            )
                
                # If user accepted a similar issue, create related link to it
                if issue_data.related_to_iid:
                    try:
                        gitlab_service.create_issue_link(
                            project_id=project_id,
                            source_issue_iid=work_item.id,
                            target_issue_iid=issue_data.related_to_iid,
                            link_type="relates_to"
                        )
                        logger.info(
                            "Issue linked to similar item",
                            source_issue_iid=work_item.id,
                            target_issue_iid=issue_data.related_to_iid
                        )
                    except Exception as link_error:
                        logger.warning(
                            "Failed to link issue to similar item",
                            source_issue_iid=work_item.id,
                            target_issue_iid=issue_data.related_to_iid,
                            error=str(link_error)
                        )
                        # Don't fail the whole operation, just log warning
            
            except Exception as e:
                logger.error(
                    "Failed to process issue",
                    project_id=project_id,
                    issue_index=idx,
                    error=str(e)
                )
                errors.append(ApplyBacklogError(
                    scope="issue",
                    input_index=idx,
                    message=str(e)
                ))
        
        # Check if any errors indicate authentication failure (401)
        has_auth_error = any(
            "401" in str(error.message) or 
            "Unauthorized" in str(error.message)
            for error in errors
        )
        
        if has_auth_error:
            logger.warning(
                "Authentication error detected in backlog application",
                project_id=project_id,
                error_count=len(errors)
            )
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="GitLab authentication failed. Please reconnect your GitLab account."
            )
        
        response = ApplyBacklogResponse(results=results, errors=errors)
        
        # Cache result for idempotency
        if request.prompt_id:
            idempotency_store.set(project_id, request.prompt_id, response)
        
        logger.info(
            "Backlog applied successfully",
            gitlab_project_id=project_id,
            internal_project_id=request.internal_project_id,
            prompt_id=request.prompt_id,
            epics_created=sum(1 for e in results.epics if e.action == "created"),
            epics_updated=sum(1 for e in results.epics if e.action == "updated"),
            issues_created=sum(1 for i in results.issues if i.action == "created"),
            issues_updated=sum(1 for i in results.issues if i.action == "updated"),
            errors=len(errors)
        )
        
        return response
        
    except Exception as e:
        logger.error(
            "Failed to apply backlog",
            project_id=project_id,
            error=str(e),
            error_type=type(e).__name__
        )
        error_response = map_gitlab_error(e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=error_response["error"]
        )


@router.post("/projects/resolve", response_model=ResolveProjectResponse)
async def resolve_project(
    request: ResolveProjectRequest,
    gitlab_client: gitlab.Gitlab = Depends(get_gitlab_client_dep),
    settings: GitLabClientSettings = Depends(get_gitlab_client_settings)
):
    """
    Resolve GitLab project path to numeric project ID.
    
    Accepts gitlab_path in namespace/project format.
    Returns project details including numeric ID.
    
    Example request:
    ```json
    {
      "gitlab_path": "my-group/my-project"
    }
    ```
    
    Example response:
    ```json
    {
      "project_id": "123",
      "path": "my-group/my-project",
      "name": "My Project",
      "web_url": "https://gitlab.com/my-group/my-project"
    }
    ```
    """
    try:
        gitlab_service = GitLabClientService(gitlab_client, settings)
        result = gitlab_service.resolve_project(request.gitlab_path)
        
        return ResolveProjectResponse(**result)
        
    except ValueError as e:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=str(e)
        )
    except gitlab.exceptions.GitlabAuthenticationError:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="GitLab authentication failed"
        )
    except gitlab.exceptions.GitlabGetError as e:
        if e.response_code == 404:
            raise HTTPException(
                status_code=status.HTTP_404_NOT_FOUND,
                detail=f"GitLab project not found: {request.gitlab_path}"
            )
        else:
            raise HTTPException(
                status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
                detail=f"GitLab API error: {e.response_code}"
            )
    except Exception as e:
        logger.error("Failed to resolve project", error=str(e), error_type=type(e).__name__)
        error_response = map_gitlab_error(e)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=error_response["error"]
        )


@router.post("/projects/multi/cache-embeddings", status_code=status.HTTP_202_ACCEPTED)
async def cache_multiple_projects_embeddings(
    project_ids: str = Query(..., description="Comma-separated list of GitLab project IDs"),
    background_tasks: BackgroundTasks = BackgroundTasks(),
    gitlab_client: gitlab.Gitlab = Depends(get_gitlab_client_dep),
    redis_client: redis.Redis = Depends(get_redis_client_dep),
    settings: GitLabClientSettings = Depends(get_gitlab_client_settings)
):
    """
    Precompute and cache title embeddings for multiple GitLab projects.
    
    This endpoint starts a background task that processes all specified projects
    and returns immediately. Each project is cached independently.
    
    Args:
        project_ids: Comma-separated GitLab project IDs (e.g., "123,456,789")
        
    Returns:
        Status message with project IDs being processed
    """
    # Parse project IDs
    id_list = [pid.strip() for pid in project_ids.split(",") if pid.strip()]
    
    if not id_list:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="No project IDs provided"
        )
    
    logger.info(
        "Starting multi-project embedding caching",
        project_ids=id_list,
        project_count=len(id_list)
    )
    
    async def cache_multi_embeddings_task():
        """Background task to cache embeddings for multiple projects."""
        gitlab_service = GitLabClientService(gitlab_client, settings)
        embedding_client = EmbeddingClient(settings)
        cache_client = RedisCacheClient(redis_client)
        notifier = ProgressNotifier(redis_client)
        
        total_projects = len(id_list)
        success_count = 0
        error_count = 0
        
        for idx, project_id in enumerate(id_list, start=1):
            try:
                await notifier.notify_started(
                    project_id=project_id,
                    project_index=idx,
                    total_projects=total_projects
                )
                
                # Fetch all work items (paginate through all pages)
                all_items = []
                page = 1
                
                while True:
                    response = gitlab_service.list_project_backlog(
                        project_id=project_id,
                        state="all",
                        page=page,
                        per_page=settings.DEFAULT_PAGE_SIZE
                    )
                    
                    all_items.extend(response.items)
                    
                    await notifier.notify_progress(
                        project_id=project_id,
                        scanned=len(all_items),
                        total=len(all_items),
                        project_index=idx,
                        total_projects=total_projects
                    )
                    
                    if response.pagination.next_page is None:
                        break
                    
                    page = response.pagination.next_page
                
                if not all_items:
                    logger.warning("No work items found for embedding", project_id=project_id)
                    await notifier.notify_completed(
                        project_id=project_id,
                        project_index=idx,
                        total_projects=total_projects
                    )
                    success_count += 1
                    continue
                
                # Generate embeddings for all titles
                titles = [item.title for item in all_items]
                embeddings = await embedding_client.embed_texts_batched(titles)
                
                await notifier.notify_embedded(
                    project_id=project_id,
                    completed=len(embeddings),
                    total=len(all_items),
                    project_index=idx,
                    total_projects=total_projects
                )
                
                # Clear old cache and store new embeddings with titles
                await cache_client.clear_project_embeddings(project_id)
                
                embeddings_dict = {
                    item.id: {"title": item.title, "embedding": embedding}
                    for item, embedding in zip(all_items, embeddings)
                }
                
                await cache_client.set_embeddings_bulk(project_id, embeddings_dict)
                
                await notifier.notify_cached(
                    project_id=project_id,
                    cached=len(embeddings_dict),
                    project_index=idx,
                    total_projects=total_projects
                )
                
                await notifier.notify_completed(
                    project_id=project_id,
                    project_index=idx,
                    total_projects=total_projects
                )
                
                success_count += 1
                
                logger.info(
                    "Embedding caching completed for project",
                    project_id=project_id,
                    project_index=idx,
                    total_projects=total_projects,
                    total_cached=len(embeddings_dict)
                )
                
            except Exception as e:
                error_count += 1
                error_message, error_tip = get_error_tip_for_gitlab_exception(e)
                
                logger.error(
                    "Embedding caching failed for project",
                    project_id=project_id,
                    project_index=idx,
                    total_projects=total_projects,
                    error=str(e),
                    error_type=type(e).__name__,
                    error_message=error_message,
                    error_tip=error_tip
                )
                
                await notifier.notify_error(
                    project_id=project_id,
                    error_message=error_message,
                    error_tip=error_tip,
                    project_index=idx,
                    total_projects=total_projects
                )
        
        # Send overall completion notification
        await notifier.notify_all_completed(
            project_ids=id_list,
            success_count=success_count,
            error_count=error_count
        )
        
        logger.info(
            "Multi-project embedding caching completed",
            total_projects=total_projects,
            success_count=success_count,
            error_count=error_count
        )
    
    # Add to background tasks
    background_tasks.add_task(cache_multi_embeddings_task)
    
    return {
        "message": "Multi-project embedding caching started",
        "project_ids": id_list,
        "project_count": len(id_list)
    }


