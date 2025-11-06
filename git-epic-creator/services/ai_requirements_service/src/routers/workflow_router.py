from typing import List
from uuid import UUID

import redis.asyncio as redis
from fastapi import APIRouter, Depends, HTTPException
from utils.local_auth import get_local_user_verified, LocalUser
from pydantic import BaseModel, Field
import structlog

from utils.app_factory import get_redis_client_from_state
from workflow_models.requirements_models import (
    RequirementsBundle,
    QuestionAnswer,
    EnhanceRequirementRequest,
    Requirement,
)
from services.ai_workflow_status_publisher import AiWorkflowStatusPublisher
from orchestrator.orchestrator import (
    run_requirements_workflow,
    run_answers_workflow,
    run_single_requirement_enhancement,
)

logger = structlog.get_logger(__name__)


router = APIRouter()


class RequirementsRequest(BaseModel):
    project_id: UUID = Field(..., description="Project identifier")
    prompt: str = Field(..., description="User prompt describing the project/feature")
    prompt_id: UUID | None = Field(None, description="Optional prompt identifier for conversation continuity")


class AnswersRequest(BaseModel):
    project_id: UUID = Field(..., description="Project identifier")
    prompt_id: UUID = Field(..., description="Prompt identifier")
    prompt: str = Field(..., description="Original user prompt to rerun with answers")
    answers: List[QuestionAnswer] = Field(..., description="Answered clarifications")


@router.post("/requirements", response_model=RequirementsBundle)
async def create_requirements_bundle(
    request: RequirementsRequest,
    redis_client: redis.Redis = Depends(get_redis_client_from_state),
    current_user: LocalUser = Depends(get_local_user_verified),
) -> RequirementsBundle:
    try:
        publisher = AiWorkflowStatusPublisher(redis_client)
        bundle = await run_requirements_workflow(
            project_id=request.project_id,
            prompt=request.prompt,
            publisher=publisher,
            prompt_id_opt=request.prompt_id,
            auth_header=(f"Bearer {current_user.token}"),
        )
        return bundle
    except RuntimeError as e:
        logger.error("workflow_execution_failed", error=str(e), project_id=str(request.project_id))
        raise HTTPException(status_code=500, detail=str(e)) from e


@router.post("/answers", response_model=RequirementsBundle)
async def answer_clarifications(
    request: AnswersRequest,
    redis_client: redis.Redis = Depends(get_redis_client_from_state),
    current_user: LocalUser = Depends(get_local_user_verified),
) -> RequirementsBundle:
    try:
        publisher = AiWorkflowStatusPublisher(redis_client)
        bundle = await run_answers_workflow(
            project_id=request.project_id,
            prompt_id=request.prompt_id,
            prompt=request.prompt,
            answers=request.answers,
            publisher=publisher,
            auth_header=(f"Bearer {current_user.token}"),
        )
        return bundle
    except RuntimeError as e:
        logger.error("workflow_execution_failed", error=str(e), project_id=str(request.project_id))
        raise HTTPException(status_code=500, detail=str(e)) from e


@router.post("/enhance", response_model=Requirement)
async def enhance_requirement(
    request: EnhanceRequirementRequest,
    redis_client: redis.Redis = Depends(get_redis_client_from_state),
    current_user: LocalUser = Depends(get_local_user_verified),
) -> Requirement:
    """Enhance a single requirement with AI-generated expansions.
    
    This endpoint provides focused enhancement of individual BR/FR items with:
    - Detailed technical descriptions
    - Enhanced acceptance criteria
    - Validated mermaid diagrams
    - Context-grounded improvements
    
    Args:
        request: Enhancement request with requirement ID and current content
        redis_client: Redis client for progress publishing
        current_user: Authenticated user
        
    Returns:
        Enhanced requirement with improved content
        
    Raises:
        HTTPException: 500 if enhancement workflow fails
    """
    try:
        publisher = AiWorkflowStatusPublisher(redis_client)
        enhanced_dict = await run_single_requirement_enhancement(
            project_id=request.project_id,
            requirement_id=request.requirement_id,
            requirement_type=request.requirement_type,
            current_content=request.current_content,
            publisher=publisher,
            auth_header=(f"Bearer {current_user.token}"),
        )
        return Requirement(**enhanced_dict)
    except RuntimeError as e:
        logger.error(
            "enhancement_workflow_failed",
            error=str(e),
            project_id=str(request.project_id),
            requirement_id=request.requirement_id,
        )
        raise HTTPException(status_code=500, detail=str(e)) from e


