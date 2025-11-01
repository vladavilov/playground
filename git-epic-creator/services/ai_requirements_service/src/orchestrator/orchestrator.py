from typing import List
from uuid import UUID, uuid4

from workflow_models.requirements_models import RequirementsBundle, QuestionAnswer
from services.ai_workflow_status_publisher import AiWorkflowStatusPublisher
import config
from orchestrator import graph_pipeline as lg_pipeline
import structlog

logger = structlog.get_logger(__name__)


async def run_requirements_workflow(
    project_id: UUID,
    prompt: str,
    publisher: AiWorkflowStatusPublisher,
    answers: List[QuestionAnswer] | None = None,
    prompt_id_opt: UUID | None = None,
    auth_header: str | None = None,
) -> RequirementsBundle:
    settings = config.get_ai_requirements_settings()
    target = float(settings.CLARIFICATION_SCORE_TARGET)
    max_iters = int(settings.MAX_AGENT_ITERS)

    # Create/resolve prompt_id
    prompt_id_local = prompt_id_opt or uuid4()
    thread_id = f"{project_id}:{prompt_id_local}"
    
    # Load previous conversation if prompt_id exists (for conversation continuity)
    previous_requirements = None
    existing_messages = []
    
    if prompt_id_opt:
        # Create graph temporarily to access checkpointer
        graph = await lg_pipeline.create_requirements_graph(publisher, target=target, max_iters=max_iters)
        config_dict = {"configurable": {"thread_id": thread_id}}
        
        try:
            state_snapshot = await graph.aget_state(config_dict)
            if state_snapshot and state_snapshot.values:
                existing_messages = state_snapshot.values.get("messages", [])
                previous_result = state_snapshot.values.get("result")
                if isinstance(previous_result, RequirementsBundle):
                    previous_requirements = previous_result
                logger.info("loaded_previous_state", thread_id=thread_id, 
                           message_count=len(existing_messages), 
                           has_previous_requirements=previous_requirements is not None)
        except Exception as e:
            logger.warning("failed_to_load_previous_state", error=str(e), thread_id=thread_id)
    
    # Build messages list: previous messages + new message + answers
    msgs: List[dict] = existing_messages.copy() if existing_messages else []
    msgs.append({"role": "user", "content": prompt})
    
    if answers:
        for a in answers:
            msgs.append({"role": "user", "content": f"Q{a.id}: {a.answer}"})
    
    # Run LangGraph pipeline with conversation context
    graph = await lg_pipeline.create_requirements_graph(publisher, target=target, max_iters=max_iters)
    result_state = await graph.ainvoke({
        "project_id": project_id,
        "prompt": prompt,
        "prompt_id": prompt_id_local,
        "messages": msgs,
        "auth_header": auth_header,
        "previous_requirements": previous_requirements,
    }, {"configurable": {"thread_id": thread_id}})
    
    bundle = result_state.get("result")
    if isinstance(bundle, RequirementsBundle):
        return bundle
    # Safety fallback (should not happen)
    raise RuntimeError("Graph pipeline did not produce a RequirementsBundle")


async def run_answers_workflow(
    project_id: UUID,
    prompt_id: UUID,
    prompt: str,
    answers: List[QuestionAnswer],
    publisher: AiWorkflowStatusPublisher,
    auth_header: str | None = None,
) -> RequirementsBundle:
    # Re-run the full requirements workflow with prompt augmented by answers
    rebundled = await run_requirements_workflow(
        project_id=project_id,
        prompt=prompt,
        publisher=publisher,
        answers=answers,
        prompt_id_opt=prompt_id,
        auth_header=auth_header,
    )

    # Preserve original prompt_id for the Q/A thread
    return RequirementsBundle(
        prompt_id=prompt_id,
        project_id=project_id,
        business_requirements=rebundled.business_requirements,
        functional_requirements=rebundled.functional_requirements,
        assumptions=rebundled.assumptions,
        risks=rebundled.risks,
        score=rebundled.score,
        clarification_questions=rebundled.clarification_questions,
    )

