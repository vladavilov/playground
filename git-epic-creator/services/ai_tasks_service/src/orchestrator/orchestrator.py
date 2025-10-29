"""Entry point functions for backlog generation orchestration."""

from uuid import UUID, uuid4
from typing import List
import structlog

from task_models.request_models import GeneratedBacklogBundle
from services.ai_tasks_status_publisher import AiTasksStatusPublisher
from config import get_ai_tasks_settings
from orchestrator import graph_pipeline as lg_pipeline
from orchestrator.experts.clients.project_client import ProjectClient

logger = structlog.get_logger(__name__)


async def run_backlog_workflow(
    project_id: UUID,
    requirements: str,
    publisher: AiTasksStatusPublisher,
    prompt_id_opt: UUID | None = None,
    auth_header: str | None = None,
    gitlab_token: str | None = None,
) -> GeneratedBacklogBundle:
    """Run the backlog generation workflow.
    
    Args:
        project_id: Project identifier (internal PostgreSQL UUID)
        requirements: Requirements text (markdown or plain)
        publisher: Redis publisher for progress updates
        prompt_id_opt: Optional conversation thread ID (creates new if None)
        auth_header: Optional JWT auth header for project and GraphRAG services
        gitlab_token: Optional GitLab access token
        
    Returns:
        GeneratedBacklogBundle with epics, tasks, and metadata
    """
    settings = get_ai_tasks_settings()
    target = float(settings.CLARIFICATION_SCORE_TARGET)
    max_iters = int(settings.MAX_AGENT_ITERS)

    # Create/resolve prompt_id
    prompt_id_local = prompt_id_opt or uuid4()
    
    # Fetch gitlab_project_id from project_management_service
    project_client = ProjectClient(
        base_url=settings.http.PROJECT_MANAGEMENT_SERVICE_URL,
        timeout_sec=settings.http.CONNECTION_TIMEOUT,
    )
    
    gitlab_project_id = None
    try:
        gitlab_project_id = await project_client.get_gitlab_project_id(
            project_id=project_id,
            auth_header=auth_header,
        )
        
        if gitlab_project_id:
            logger.info(
                "Retrieved GitLab project ID for workflow",
                project_id=str(project_id),
                gitlab_project_id=gitlab_project_id,
            )
        else:
            logger.warning(
                "Project has no GitLab integration; duplicate detection will be skipped",
                project_id=str(project_id),
            )
    except Exception as e:
        logger.error(
            "Failed to fetch GitLab project ID; continuing with empty backlog",
            project_id=str(project_id),
            error=str(e),
            error_type=type(e).__name__,
        )
        # Continue workflow without GitLab integration rather than failing
    
    # Build initial message
    msgs: List[dict] = [{"role": "user", "content": requirements}]
    
    # Run LangGraph pipeline
    graph = await lg_pipeline.create_backlog_graph(publisher, target=target, max_iters=max_iters)
    result_state = await graph.ainvoke(
        {
            "project_id": project_id,
            "prompt_id": prompt_id_local,
            "requirements": requirements,
            "messages": msgs,
            "auth_header": auth_header,
            "gitlab_token": gitlab_token,
            "gitlab_project_id": gitlab_project_id,
        },
        {"configurable": {"thread_id": f"{project_id}:{prompt_id_local}"}},
    )
    
    bundle = result_state.get("result")
    if isinstance(bundle, GeneratedBacklogBundle):
        return bundle
    
    # Safety fallback (should not happen)
    raise RuntimeError("Graph pipeline did not produce a GeneratedBacklogBundle")


