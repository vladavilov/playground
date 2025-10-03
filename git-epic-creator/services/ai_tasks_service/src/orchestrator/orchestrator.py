"""Entry point functions for backlog generation orchestration."""

from uuid import UUID, uuid4
from typing import List

from models.request_models import GeneratedBacklogBundle
from services.ai_tasks_status_publisher import AiTasksStatusPublisher
from config import get_ai_tasks_settings
from orchestrator import graph_pipeline as lg_pipeline


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
        project_id: Project identifier
        requirements: Requirements text (markdown or plain)
        publisher: Redis publisher for progress updates
        prompt_id_opt: Optional conversation thread ID (creates new if None)
        auth_header: Optional JWT auth header for GraphRAG service
        gitlab_token: Optional GitLab access token
        
    Returns:
        GeneratedBacklogBundle with epics, tasks, and metadata
    """
    settings = get_ai_tasks_settings()
    target = float(settings.CLARIFICATION_SCORE_TARGET)
    max_iters = int(settings.MAX_AGENT_ITERS)

    # Create/resolve prompt_id
    prompt_id_local = prompt_id_opt or uuid4()
    
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
        },
        {"configurable": {"thread_id": f"{project_id}:{prompt_id_local}"}},
    )
    
    bundle = result_state.get("result")
    if isinstance(bundle, GeneratedBacklogBundle):
        return bundle
    
    # Safety fallback (should not happen)
    raise RuntimeError("Graph pipeline did not produce a GeneratedBacklogBundle")


