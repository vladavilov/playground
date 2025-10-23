from typing import Any, Dict, Annotated, TypedDict, Literal
from uuid import uuid4
from operator import add
import time

from langgraph.graph import StateGraph, START, END
from langgraph.graph.message import add_messages
from langgraph.checkpoint.memory import InMemorySaver
from langgraph.types import Command
from langchain_core.messages.utils import trim_messages, count_tokens_approximately
import structlog
from config import get_ai_requirements_settings

from workflow_models.requirements_models import RequirementsBundle
from workflow_models.agent_models import AuditFindings
from orchestrator.experts.prompt_analyst import PromptAnalyst
from orchestrator.experts.context_retriever import ContextRetriever
from orchestrator.experts.requirements_engineer import RequirementsEngineer
from orchestrator.experts.consistency_auditor import ConsistencyAuditor
from orchestrator.experts.evaluator import Evaluator
from orchestrator.experts.question_strategist import QuestionStrategist

logger = structlog.get_logger(__name__)


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
        auth_header: str
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
        workflow_start_time: float  # Track workflow start for timeout detection

    analyst = PromptAnalyst()
    retriever = ContextRetriever()
    engineer = RequirementsEngineer()
    auditor = ConsistencyAuditor()
    evaluator = Evaluator()
    strategist = QuestionStrategist()

    async def init_node(state: Dict[str, Any]) -> Dict[str, Any]:
        # Record workflow start time for timeout tracking
        workflow_start = time.time()
        
        # Build details_md with prompt preview
        prompt_text = state.get("prompt", "")
        prompt_preview = prompt_text[:200] + "..." if len(prompt_text) > 200 else prompt_text
        details_md = f"### User Prompt\n\n{prompt_preview}\n\n### Workflow Configuration\n- Target Score: **{target:.2f}**\n- Max Iterations: **{max_iters}**"
        
        await publisher.publish_workflow_update(
            project_id=state["project_id"],
            prompt_id=state.get("prompt_id"),
            status="analyzing_prompt",
            thought_summary="Analyzing prompt and outlining initial intents.",
            details_md=details_md,
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
        return {
            "iteration": [0], 
            "target": target, 
            "max_iters": max_iters, 
            "result": None, 
            "messages": trimmed,
            "workflow_start_time": workflow_start
        }

    async def analyze_node(state: Dict[str, Any]) -> Dict[str, Any]:
        analysis = await analyst.analyze(state["prompt"])
        
        # Build details_md with analysis breakdown
        md_lines = ["### Analysis Results"]
        if analysis.intents:
            intents_list = analysis.intents if isinstance(analysis.intents, list) else [analysis.intents]
            md_lines.append(f"\n**Intents:** {', '.join(intents_list)}")
        details_md = "\n".join(md_lines)
        
        await publisher.publish_workflow_update(
            project_id=state["project_id"],
            prompt_id=state.get("prompt_id"),
            status="analyzing_prompt",
            thought_summary=f"Preparing retrieval plan and fetching project context. Intents found: {analysis.intents}.",
            details_md=details_md,
        )
        return {"analysis": analysis}

    async def retrieve_node(state: Dict[str, Any]) -> Dict[str, Any]:
        auth_header = state.get("auth_header")
        prompt_id = state.get("prompt_id")
        context = await retriever.retrieve(state["analysis"], state["project_id"], auth_header=auth_header, prompt_id=prompt_id)
        citations = list(context.citations or [])
        # Publish interim retrieval results
        try:
            total = len(citations)
            top = citations[:5]
            md_lines: list[str] = [
                "### Retrieved context",
                f"- Items: **{total}**",
            ]
            if top:
                md_lines.append("- Top refs:")
                # Format citations as: [Document Name] "text preview..."
                for cit in top:
                    citation_display = f"[{cit.document_name}] \"{cit.text_preview}\""
                    md_lines.append(f"  - {citation_display}")
            details_md = "\n".join(md_lines)
            await publisher.publish_workflow_update(
                project_id=state["project_id"],
                prompt_id=state.get("prompt_id"),
                status="retrieving_context",
                thought_summary=f"Retrieved {total} context items.",
                details_md=details_md,
            )
        except Exception:
            # Best-effort progress update; do not fail node on publish error
            pass
        return {"context": context, "citations": citations}

    async def synthesize_node(state: Dict[str, Any]) -> Dict[str, Any]:
        draft = await engineer.synthesize(state["analysis"], state["context"], state.get("findings") or AuditFindings())
        # Publish interim synthesis results
        try:
            br = list(draft.business_requirements or [])
            fr = list(draft.functional_requirements or [])
            br_count = len(br)
            fr_count = len(fr)
            def _titles(items):
                out = []
                for r in items[:3]:
                    try:
                        out.append(f"- {r.id}: {r.title}")
                    except Exception:
                        continue
                return out
            md_lines: list[str] = [
                "### Draft synthesis",
                f"- Business requirements: **{br_count}**",
                f"- Functional requirements: **{fr_count}**",
            ]
            titles = _titles(br) + _titles(fr)
            if titles:
                md_lines.append("- Examples:")
                md_lines.extend([f"  {t}" for t in titles])
            details_md = "\n".join(md_lines)
            await publisher.publish_workflow_update(
                project_id=state["project_id"],
                prompt_id=state.get("prompt_id"),
                status="drafting_requirements",
                thought_summary=f"Drafted {br_count} business and {fr_count} functional requirements.",
                details_md=details_md,
            )
        except Exception:
            pass
        return {"draft": draft}

    async def audit_node(state: Dict[str, Any]) -> Dict[str, Any]:
        findings = await auditor.audit(state["draft"], state["context"], state["prompt"])
        # Publish interim audit results
        try:
            issues = list(findings.issues or [])
            suggestions = list(findings.suggestions or [])
            md_lines: list[str] = [
                "### Audit findings",
                f"- Issues: **{len(issues)}**",
                f"- Suggestions: **{len(suggestions)}**",
            ]
            for label, items in (("Top issues", issues[:3]), ("Top suggestions", suggestions[:3])):
                if items:
                    md_lines.append(f"- {label}:")
                    md_lines.extend([f"  - {str(it)}" for it in items])
            details_md = "\n".join(md_lines)
            await publisher.publish_workflow_update(
                project_id=state["project_id"],
                prompt_id=state.get("prompt_id"),
                status="evaluating",
                thought_summary=f"Audited draft; identified {len(issues)} issues.",
                details_md=details_md,
            )
        except Exception:
            pass
        return {"findings": findings}

    async def supervisor_node(state: Dict[str, Any]) -> Command[Literal["finalize", "clarify", "synthesize"]]:
        settings = get_ai_requirements_settings()
        
        # Check workflow timeout (90% threshold for graceful abort)
        workflow_start = state.get("workflow_start_time", time.time())
        elapsed = time.time() - workflow_start
        timeout_threshold = settings.WORKFLOW_TIMEOUT_SEC * 0.9  # 90% of timeout (135s default)
        
        # Log warnings at key percentages
        percent_elapsed = (elapsed / settings.WORKFLOW_TIMEOUT_SEC) * 100
        if percent_elapsed >= 90:
            logger.warning(
                "workflow_timeout_imminent",
                elapsed_sec=round(elapsed, 1),
                timeout_sec=settings.WORKFLOW_TIMEOUT_SEC,
                percent=round(percent_elapsed, 1),
                message="Workflow at 90%+ of timeout, will abort at current iteration"
            )
        elif percent_elapsed >= 75:
            logger.warning(
                "workflow_timeout_approaching",
                elapsed_sec=round(elapsed, 1),
                timeout_sec=settings.WORKFLOW_TIMEOUT_SEC,
                percent=round(percent_elapsed, 1),
                message="Workflow at 75%+ of timeout"
            )
        elif percent_elapsed >= 50:
            logger.info(
                "workflow_timeout_halfway",
                elapsed_sec=round(elapsed, 1),
                timeout_sec=settings.WORKFLOW_TIMEOUT_SEC,
                percent=round(percent_elapsed, 1),
                message="Workflow at 50%+ of timeout"
            )
        
        # Compute evaluation here (sequentially after audit) and publish progress
        report = await evaluator.evaluate(state["draft"], state["findings"], state["prompt"], state["context"])
        score = float(getattr(report, "score", 0.0) or 0.0)
        current_iter = int((state.get("iteration") or [0])[-1]) + 1
        await publisher.publish_workflow_update(
            project_id=state["project_id"],
            prompt_id=state.get("prompt_id"),
            status="evaluating",
            thought_summary="Drafted requirements, audited for issues, and evaluated against rubric.",
            details_md=(
                "### Evaluation results\n"
                f"- Score: **{score:.2f}** (target **{float(state.get('target', 1.0)):.2f}**)\n"
                + ("- Components:" + "\n" + "\n".join([f"  - {k}: {v:.3f}" for k, v in (getattr(report, 'component_scores', {}) or {}).items()]) if getattr(report, 'component_scores', None) else "")
            ),
            iteration=current_iter,
            score=score,
        )
        
        # Check timeout AFTER evaluation (evaluation always runs)
        if elapsed > timeout_threshold:
            logger.warning(
                "workflow_timeout_abort",
                elapsed_sec=round(elapsed, 1),
                timeout_sec=settings.WORKFLOW_TIMEOUT_SEC,
                current_iter=current_iter,
                score=score,
                message="Workflow exceeded 90% of timeout threshold, aborting gracefully"
            )
            # Force finalize to return partial results
            return Command(goto="finalize", update={"report": report, "iteration": [current_iter]})
        
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
    builder.add_edge("synthesize", "audit")
    builder.add_edge("audit", "supervisor")
    # decide returns Command to goto next
    builder.add_edge("finalize", END)
    builder.add_edge("clarify", END)

    checkpointer = InMemorySaver()
    
    graph = builder.compile(checkpointer=checkpointer)
    return graph


