from typing import Any, Dict, Annotated, TypedDict, Literal
from uuid import uuid4
from operator import add

from langgraph.graph import StateGraph, START, END
from langgraph.graph.message import add_messages
from langgraph.checkpoint.memory import InMemorySaver
from langgraph.types import Command
from langchain_core.messages.utils import trim_messages, count_tokens_approximately

from workflow_models.workflow_models import RequirementsBundle
from workflow_models.agent_models import AuditFindings
from orchestrator.experts.prompt_analyst import PromptAnalyst
from orchestrator.experts.context_retriever import ContextRetriever
from orchestrator.experts.requirements_engineer import RequirementsEngineer
from orchestrator.experts.consistency_auditor import ConsistencyAuditor
from orchestrator.experts.evaluator import Evaluator
from orchestrator.experts.question_strategist import QuestionStrategist


async def create_requirements_graph(publisher: Any, *, target: float, max_iters: int):
    """
    Build a LangGraph StateGraph for the requirements workflow using dynamic imports.
    State keys:
      - project_id: UUID
      - prompt: str
      - analysis, context, draft, findings, report: objects from experts
      - iteration: int
      - target: float
      - max_iters: int
      - citations: List[str]
      - result: RequirementsBundle | None
    """
    class State(TypedDict, total=False):
        # Chat history with reducer so messages are appended across turns
        messages: Annotated[list[Any], add_messages]
        # Workflow data
        project_id: Any
        prompt: str
        prompt_id: Any
        analysis: Any
        context: Any
        draft: Any
        findings: Any
        report: Any
        iteration: Annotated[list[int], add]
        target: float
        max_iters: int
        citations: Annotated[list[str], add]
        result: Any

    analyst = PromptAnalyst()
    retriever = ContextRetriever()
    engineer = RequirementsEngineer()
    auditor = ConsistencyAuditor()
    evaluator = Evaluator()
    strategist = QuestionStrategist()

    async def init_node(state: Dict[str, Any]) -> Dict[str, Any]:
        await publisher.publish_workflow_update(
            project_id=state["project_id"],
            prompt_id=state.get("prompt_id"),
            stage="prompt_decomposition",
            status="analyzing_prompt",
            thought_summary="Analyzing prompt and outlining initial intents.",
        )
        incoming_msgs = state.get("messages") or []
        if incoming_msgs:
            trimmed = trim_messages(
                incoming_msgs,
                strategy="last",
                token_counter=count_tokens_approximately,
                max_tokens=512,
                start_on="human",
                end_on=("human", "tool"),
            )
        else:
            trimmed = incoming_msgs
        return {"iteration": [0], "target": target, "max_iters": max_iters, "result": None, "messages": trimmed}

    async def analyze_node(state: Dict[str, Any]) -> Dict[str, Any]:
        analysis = await analyst.analyze(state["prompt"])
        await publisher.publish_workflow_update(
            project_id=state["project_id"],
            prompt_id=state.get("prompt_id"),
            stage="retrieve_context",
            status="retrieving_context",
            thought_summary=f"Preparing retrieval plan and fetching project context. Intents found: {analysis.intents}.",
        )
        return {"analysis": analysis}

    async def retrieve_node(state: Dict[str, Any]) -> Dict[str, Any]:
        context = await retriever.retrieve(state["analysis"])
        return {"context": context, "citations": list(context.citations or [])}

    async def synthesize_node(state: Dict[str, Any]) -> Dict[str, Any]:
        draft = await engineer.synthesize(state["analysis"], state["context"], state.get("findings") or AuditFindings())
        return {"draft": draft}

    async def audit_node(state: Dict[str, Any]) -> Dict[str, Any]:
        findings = await auditor.audit(state["draft"], state["context"], state["prompt"])
        return {"findings": findings}

    async def supervisor_node(state: Dict[str, Any]) -> Command[Literal["finalize", "clarify", "synthesize"]]:
        # Compute evaluation here (sequentially after audit) and publish progress
        report = await evaluator.evaluate(state["draft"], state["findings"], state["prompt"], state["context"])
        score = float(getattr(report, "score", 0.0) or 0.0)
        current_iter = int((state.get("iteration") or [0])[-1]) + 1
        await publisher.publish_workflow_update(
            project_id=state["project_id"],
            prompt_id=state.get("prompt_id"),
            stage="draft_requirements",
            status="drafting_requirements",
            thought_summary="Drafted requirements, audited for issues, and evaluated against rubric.",
            iteration=current_iter,
            score=score,
            citations=state.get("citations") or None,
        )
        if score >= state.get("target", 1.0):
            return Command(goto="finalize", update={"report": report, "iteration": [current_iter]})
        if current_iter >= int(state.get("max_iters", 1)):
            return Command(goto="clarify", update={"report": report, "iteration": [current_iter]})
        return Command(goto="synthesize", update={"report": report, "iteration": [current_iter]})

    async def finalize_node(state: Dict[str, Any]) -> Dict[str, Any]:
        prompt_id = state.get("prompt_id") or uuid4()
        bundle = RequirementsBundle(
            prompt_id=prompt_id,
            project_id=state["project_id"],
            business_requirements=state["draft"].business_requirements,
            functional_requirements=state["draft"].functional_requirements,
            assumptions=state["draft"].assumptions,
            risks=state["draft"].risks,
            score=state["report"].score,
            clarification_questions=None,
        )
        return {"result": bundle}

    async def clarify_node(state: Dict[str, Any]) -> Dict[str, Any]:
        plan = await strategist.propose(
            draft=state["draft"],
            user_prompt=state["prompt"],
            component_scores=state["report"].component_scores,
            target_score=float(state.get("target", 1.0)),
        )
        prompt_id = state.get("prompt_id") or uuid4()
        bundle = RequirementsBundle(
            prompt_id=prompt_id,
            project_id=state["project_id"],
            business_requirements=state["draft"].business_requirements,
            functional_requirements=state["draft"].functional_requirements,
            assumptions=state["draft"].assumptions,
            risks=state["draft"].risks,
            score=state["report"].score,
            clarification_questions=plan.questions,
        )
        return {"result": bundle}

    builder = StateGraph(State)
    builder.add_node("init", init_node)
    builder.add_node("analyze", analyze_node)
    builder.add_node("retrieve", retrieve_node)
    builder.add_node("synthesize", synthesize_node)
    builder.add_node("audit", audit_node)
    builder.add_node("supervisor", supervisor_node)
    builder.add_node("finalize", finalize_node)
    builder.add_node("clarify", clarify_node)

    builder.add_edge(START, "init")
    builder.add_edge("init", "analyze")
    builder.add_edge("analyze", "retrieve")
    builder.add_edge("retrieve", "synthesize")
    # after synthesize, run audit; evaluation occurs inside supervisor after audit
    builder.add_edge("synthesize", "audit")

    builder.add_edge("audit", "supervisor")
    # decide returns Command to goto next
    builder.add_edge("finalize", END)
    builder.add_edge("clarify", END)

    checkpointer = InMemorySaver()
    
    graph = builder.compile(checkpointer=checkpointer)
    return graph


