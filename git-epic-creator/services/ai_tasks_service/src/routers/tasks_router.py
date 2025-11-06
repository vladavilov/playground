"""Tasks router for backlog generation API."""

from typing import Optional
import redis.asyncio as redis
from fastapi import APIRouter, Depends, Header
from utils.local_auth import get_local_user_verified, LocalUser
from utils.app_factory import get_redis_client_from_state

from task_models.request_models import (
    TasksChatRequest,
    GeneratedBacklogBundle,
    EnhanceTaskRequest,
    EnhancedTask,
)
from services.ai_tasks_status_publisher import AiTasksStatusPublisher
from orchestrator.orchestrator import run_backlog_workflow, run_single_task_enhancement


router = APIRouter()


@router.post("/generate", response_model=GeneratedBacklogBundle)
async def generate_backlog(
    request: TasksChatRequest,
    redis_client: redis.Redis = Depends(get_redis_client_from_state),
    current_user: LocalUser = Depends(get_local_user_verified),
    x_gitlab_access_token: Optional[str] = Header(None, alias="X-GitLab-Access-Token"),
) -> GeneratedBacklogBundle:
    """Generate or refine backlog (epics and tasks) from requirements.
    
    This is a single chat-style endpoint:
    - If prompt_id is omitted, a new conversation is created
    - If prompt_id is provided, the conversation continues with refinement
    
    Args:
        request: TasksChatRequest with project_id, optional prompt_id, and message
        redis_client: Redis client (injected)
        current_user: Authenticated user (injected)
        x_gitlab_access_token: GitLab access token from UI proxy (optional header)
        
    Returns:
        GeneratedBacklogBundle with epics, tasks, score, and optional clarification questions
    """
    publisher = AiTasksStatusPublisher(redis_client)
    
    bundle = await run_backlog_workflow(
        project_id=request.project_id,
        requirements=request.message,
        publisher=publisher,
        prompt_id_opt=request.prompt_id,
        auth_header=f"Bearer {current_user.token}",
        gitlab_token=x_gitlab_access_token,
    )
    
    return bundle


@router.post("/enhance", response_model=EnhancedTask)
async def enhance_task(
    request: EnhanceTaskRequest,
    redis_client: redis.Redis = Depends(get_redis_client_from_state),
    current_user: LocalUser = Depends(get_local_user_verified),
    x_gitlab_access_token: Optional[str] = Header(None, alias="X-GitLab-Access-Token"),
) -> EnhancedTask:
    """Enhance a single epic/task with AI-generated expansions.
    
    This endpoint provides focused enhancement of individual backlog items with:
    - Detailed technical descriptions with mermaid diagrams
    - Enhanced acceptance criteria
    - Validated diagram syntax
    - Context-grounded improvements
    
    Args:
        request: Enhancement request with item ID and current content
        redis_client: Redis client for progress publishing
        current_user: Authenticated user
        x_gitlab_access_token: Optional GitLab access token from UI proxy
        
    Returns:
        Enhanced task/epic with improved content
        
    Raises:
        HTTPException: 500 if enhancement workflow fails
    """
    
    publisher = AiTasksStatusPublisher(redis_client)
    enhanced_dict = await run_single_task_enhancement(
        project_id=request.project_id,
        item_id=request.item_id,
        item_type=request.item_type,
        current_content=request.current_content,
        publisher=publisher,
        parent_epic_content=request.parent_epic_content,
        auth_header=f"Bearer {current_user.token}",
    )
    return EnhancedTask(**enhanced_dict)


