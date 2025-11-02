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
    thread_id = f"{project_id}:{prompt_id_local}"
    
    # Load previous conversation if prompt_id exists (for conversation continuity)
    previous_backlog = None
    existing_messages = []
    
    if prompt_id_opt:
        # Create graph temporarily to access checkpointer
        graph = await lg_pipeline.create_backlog_graph(publisher, target=target, max_iters=max_iters)
        config_dict = {"configurable": {"thread_id": thread_id}}
        
        try:
            state_snapshot = await graph.aget_state(config_dict)
            if state_snapshot and state_snapshot.values:
                existing_messages = state_snapshot.values.get("messages", [])
                previous_result = state_snapshot.values.get("result")
                if isinstance(previous_result, GeneratedBacklogBundle):
                    previous_backlog = previous_result
                logger.info(
                    "loaded_previous_state",
                    thread_id=thread_id,
                    message_count=len(existing_messages),
                    has_previous_backlog=previous_backlog is not None,
                )
        except Exception as e:
            logger.warning("failed_to_load_previous_state", error=str(e), thread_id=thread_id)
    
    # Fetch gitlab_project_ids from project_management_service
    project_client = ProjectClient(
        base_url=settings.http.PROJECT_MANAGEMENT_SERVICE_URL,
        timeout_sec=settings.http.CONNECTION_TIMEOUT,
    )
    
    gitlab_project_ids = []
    try:
        gitlab_project_ids = await project_client.get_gitlab_project_ids(
            project_id=project_id,
            auth_header=auth_header,
        )
        
        if gitlab_project_ids:
            logger.info(
                "Retrieved GitLab project IDs for workflow",
                project_id=str(project_id),
                gitlab_project_count=len(gitlab_project_ids),
                gitlab_project_ids=gitlab_project_ids,
            )
        else:
            logger.warning(
                "Project has no GitLab integration; duplicate detection will be skipped",
                project_id=str(project_id),
            )
    except Exception as e:
        logger.error(
            "Failed to fetch GitLab project IDs; continuing with empty backlog",
            project_id=str(project_id),
            error=str(e),
            error_type=type(e).__name__,
        )
        # Continue workflow without GitLab integration rather than failing
    
    # Build messages list: previous messages + new message
    msgs: List[dict] = existing_messages.copy() if existing_messages else []
    msgs.append({"role": "user", "content": requirements})
    
    # Run LangGraph pipeline with conversation context
    graph = await lg_pipeline.create_backlog_graph(publisher, target=target, max_iters=max_iters)
    result_state = await graph.ainvoke(
        {
            "project_id": project_id,
            "prompt_id": prompt_id_local,
            "requirements": requirements,
            "messages": msgs,
            "auth_header": auth_header,
            "gitlab_token": gitlab_token,
            "gitlab_project_ids": gitlab_project_ids,
            "previous_backlog": previous_backlog,
        },
        {"configurable": {"thread_id": thread_id}},
    )
    
    bundle = result_state.get("result")
    if isinstance(bundle, GeneratedBacklogBundle):
        return bundle
    
    # Safety fallback (should not happen)
    raise RuntimeError("Graph pipeline did not produce a GeneratedBacklogBundle")


