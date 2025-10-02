"""Tasks router for backlog generation API."""

from typing import Optional
import redis.asyncio as redis
from fastapi import APIRouter, Depends, Header
from utils.local_auth import get_local_user_verified, LocalUser
from utils.app_factory import get_redis_client_from_state

from models.request_models import TasksChatRequest, GeneratedBacklogBundle
from services.ai_tasks_status_publisher import AiTasksStatusPublisher
from orchestrator.orchestrator import run_backlog_workflow


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


