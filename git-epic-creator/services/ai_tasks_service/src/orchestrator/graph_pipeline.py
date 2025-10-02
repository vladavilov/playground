"""LangGraph pipeline for backlog generation workflow."""

from typing import Any, Dict, Annotated, TypedDict, Literal
from uuid import uuid4
from operator import add

from langgraph.graph import StateGraph, START, END
from langgraph.graph.message import add_messages
from langgraph.checkpoint.memory import InMemorySaver
from langgraph.types import Command
from langchain_core.messages.utils import trim_messages, count_tokens_approximately

from models.request_models import GeneratedBacklogBundle
from models.agent_models import AuditFindings
from orchestrator.experts.requirements_analyst import RequirementsAnalyst
from orchestrator.experts.context_retriever import ContextRetriever
from orchestrator.experts.backlog_engineer import BacklogEngineer
from orchestrator.experts.duplicate_mapper import DuplicateMapper
from orchestrator.experts.consistency_auditor import ConsistencyAuditor
from orchestrator.experts.evaluator import Evaluator
from orchestrator.experts.clarification_strategist import ClarificationStrategist
from orchestrator.experts.clients.gitlab_client import GitLabClient


async def create_backlog_graph(publisher: Any, *, target: float, max_iters: int):
    """Build a LangGraph StateGraph for the backlog generation workflow.
    
    State keys:
      - project_id: UUID
      - prompt_id: UUID
      - requirements: str (original requirements text)
      - messages: List (chat history with reducer)
      - analysis: RequirementsAnalysis
      - context: RetrievedContext
      - gitlab_backlog: Dict (epics and issues from GitLab)
      - draft: BacklogDraft
      - mappings: DuplicateMappings
      - findings: AuditFindings
      - report: EvaluationReport
      - iteration: int[]
      - target: float
      - max_iters: int
      - citations: str[]
      - result: GeneratedBacklogBundle | None
      - auth_header: str (optional)
      - gitlab_token: str (optional)
    """
    class State(TypedDict, total=False):
        # Chat history with reducer
        messages: Annotated[list[Any], add_messages]
        # Workflow data
        project_id: Any
        prompt_id: Any
        requirements: str
        analysis: Any
        context: Any
        gitlab_backlog: Any
        draft: Any
        mappings: Any
        findings: Any
        report: Any
        iteration: Annotated[list[int], add]
        target: float
        max_iters: int
        citations: Annotated[list[str], add]
        result: Any
        auth_header: str
        gitlab_token: str

    # Initialize experts
    analyst = RequirementsAnalyst()
    retriever = ContextRetriever()
    engineer = BacklogEngineer()
    mapper = DuplicateMapper()
    auditor = ConsistencyAuditor()
    evaluator = Evaluator()
    strategist = ClarificationStrategist()

    async def init_node(state: Dict[str, Any]) -> Dict[str, Any]:
        await publisher.publish_backlog_update(
            project_id=state["project_id"],
            prompt_id=state.get("prompt_id"),
            status="analyzing_requirements",
            thought_summary="Initializing backlog generation workflow.",
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
        }

    async def analyze_node(state: Dict[str, Any]) -> Dict[str, Any]:
        requirements = state.get("requirements", "")
        analysis = await analyst.analyze(requirements)
        await publisher.publish_backlog_update(
            project_id=state["project_id"],
            prompt_id=state.get("prompt_id"),
            status="analyzing_requirements",
            thought_summary=f"Extracted {len(analysis.intents)} intents and {len(analysis.entities)} entities.",
        )
        return {"analysis": analysis}

    async def retrieve_node(state: Dict[str, Any]) -> Dict[str, Any]:
        auth_header = state.get("auth_header")
        context = await retriever.retrieve(
            state["analysis"],
            state["project_id"],
            auth_header=auth_header,
        )
        citations = list(context.citations or [])
        
        try:
            total = len(citations)
            top = citations[:5]
            md_lines: list[str] = [
                "### Retrieved context",
                f"- Items: **{total}**",
            ]
            if top:
                md_lines.append("- Top refs:")
                md_lines.extend([f"  - {str(ref)}" for ref in top])
            details_md = "\n".join(md_lines)
            await publisher.publish_backlog_update(
                project_id=state["project_id"],
                prompt_id=state.get("prompt_id"),
                status="retrieving_context",
                thought_summary=f"Retrieved {total} context items.",
                details_md=details_md,
            )
        except Exception:
            pass
        
        return {"context": context, "citations": citations}

    async def fetch_backlog_node(state: Dict[str, Any]) -> Dict[str, Any]:
        # Fetch existing GitLab backlog
        from config import get_ai_tasks_settings
        settings = get_ai_tasks_settings()
        
        gitlab_client = GitLabClient(
            base_url=settings.GITLAB_INGESTION_BASE_URL,
            timeout_sec=settings.GITLAB_TIMEOUT_SEC,
        )
        
        auth_header = state.get("auth_header")
        gitlab_token = state.get("gitlab_token")
        
        gitlab_backlog = await gitlab_client.fetch_backlog(
            project_id=str(state["project_id"]),
            auth_header=auth_header,
            gitlab_token=gitlab_token,
        )
        
        epic_count = len(gitlab_backlog.get("epics", []))
        issue_count = len(gitlab_backlog.get("issues", []))
        
        await publisher.publish_backlog_update(
            project_id=state["project_id"],
            prompt_id=state.get("prompt_id"),
            status="fetching_backlog",
            thought_summary=f"Fetched {epic_count} epics and {issue_count} issues from GitLab.",
        )
        
        return {"gitlab_backlog": gitlab_backlog}

    async def draft_node(state: Dict[str, Any]) -> Dict[str, Any]:
        draft = await engineer.synthesize(
            state["analysis"],
            state["context"],
            state.get("findings") or AuditFindings(),
        )
        
        try:
            epic_count = len(draft.epics)
            task_count = sum(len(e.tasks) for e in draft.epics)
            md_lines: list[str] = [
                "### Draft synthesis",
                f"- Epics: **{epic_count}**",
                f"- Total tasks: **{task_count}**",
            ]
            
            # Sample epic titles
            if draft.epics:
                md_lines.append("- Sample epics:")
                for epic in draft.epics[:3]:
                    md_lines.append(f"  - {epic.id}: {epic.title}")
            
            details_md = "\n".join(md_lines)
            await publisher.publish_backlog_update(
                project_id=state["project_id"],
                prompt_id=state.get("prompt_id"),
                status="drafting_backlog",
                thought_summary=f"Drafted {epic_count} epics with {task_count} tasks.",
                details_md=details_md,
            )
        except Exception:
            pass
        
        return {"draft": draft}

    async def map_duplicates_node(state: Dict[str, Any]) -> Dict[str, Any]:
        mappings = await mapper.map_duplicates(
            state["draft"],
            state["gitlab_backlog"],
        )
        
        stats = mappings.stats
        await publisher.publish_backlog_update(
            project_id=state["project_id"],
            prompt_id=state.get("prompt_id"),
            status="mapping_duplicates",
            thought_summary=f"Found {stats.get('total_matches', 0)} similar items (avg similarity: {stats.get('avg_similarity', 0.0):.2f}).",
        )
        
        return {"mappings": mappings}

    async def audit_node(state: Dict[str, Any]) -> Dict[str, Any]:
        findings = await auditor.audit(
            state["mappings"].enriched_epics if state.get("mappings") else state["draft"],
            state.get("requirements", ""),
        )
        
        try:
            issues = list(findings.issues or [])
            suggestions = list(findings.suggestions or [])
            md_lines: list[str] = [
                "### Audit findings",
                f"- Issues: **{len(issues)}**",
                f"- Suggestions: **{len(suggestions)}**",
            ]
            if issues:
                md_lines.append("- Top issues:")
                md_lines.extend([f"  - {str(it)}" for it in issues[:3]])
            details_md = "\n".join(md_lines)
            await publisher.publish_backlog_update(
                project_id=state["project_id"],
                prompt_id=state.get("prompt_id"),
                status="evaluating",
                thought_summary=f"Audited draft; identified {len(issues)} issues.",
                details_md=details_md,
            )
        except Exception:
            pass
        
        return {"findings": findings}

    async def supervisor_node(state: Dict[str, Any]) -> Command[Literal["finalize", "clarify", "draft"]]:
        # Build draft for evaluation (use enriched if available)
        eval_draft = state["draft"]
        if state.get("mappings"):
            # Create a draft with enriched epics
            from models.agent_models import BacklogDraft
            eval_draft = BacklogDraft(
                epics=state["mappings"].enriched_epics,
                assumptions=state["draft"].assumptions,
                risks=state["draft"].risks,
            )
        
        report = await evaluator.evaluate(
            eval_draft,
            state["findings"],
            state.get("requirements", ""),
        )
        
        score = float(getattr(report, "score", 0.0) or 0.0)
        current_iter = int((state.get("iteration") or [0])[-1]) + 1
        
        await publisher.publish_backlog_update(
            project_id=state["project_id"],
            prompt_id=state.get("prompt_id"),
            status="evaluating",
            thought_summary="Evaluated backlog quality against rubric.",
            details_md=(
                "### Evaluation results\n"
                f"- Score: **{score:.2f}** (target **{float(state.get('target', 1.0)):.2f}**)\n"
                + (
                    "- Components:\n"
                    + "\n".join([f"  - {k}: {v:.3f}" for k, v in (getattr(report, "component_scores", {}) or {}).items()])
                    if getattr(report, "component_scores", None)
                    else ""
                )
            ),
            iteration=current_iter,
            score=score,
        )
        
        if score >= state.get("target", 1.0):
            return Command(goto="finalize", update={"report": report, "iteration": [current_iter]})
        if current_iter >= int(state.get("max_iters", 1)):
            return Command(goto="clarify", update={"report": report, "iteration": [current_iter]})
        return Command(goto="draft", update={"report": report, "iteration": [current_iter]})

    async def finalize_node(state: Dict[str, Any]) -> Dict[str, Any]:
        prompt_id = state.get("prompt_id") or uuid4()
        
        # Use enriched epics if available
        final_epics = state["draft"].epics
        if state.get("mappings"):
            final_epics = state["mappings"].enriched_epics
        
        # Build markdown text
        markdown_lines = ["# Generated Backlog\n"]
        for epic in final_epics:
            markdown_lines.append(f"## Epic: {epic.title}")
            markdown_lines.append(f"**ID**: {epic.id}")
            markdown_lines.append(f"**Description**: {epic.description}\n")
            
            if epic.similar:
                markdown_lines.append("**Similar GitLab Epics:**")
                for sim in epic.similar:
                    status_str = f" ({sim.status})" if sim.status else ""
                    markdown_lines.append(f"- [{sim.kind} {sim.id}]({sim.url}){status_str} - similarity: {sim.similarity:.2f}")
                markdown_lines.append("")
            
            markdown_lines.append("### Tasks")
            for task in epic.tasks:
                markdown_lines.append(f"#### Task: {task.title}")
                markdown_lines.append(f"**ID**: {task.id}")
                markdown_lines.append(f"**Description**: {task.description}")
                
                if task.acceptance_criteria:
                    markdown_lines.append("\n**Acceptance Criteria:**")
                    markdown_lines.extend([f"- {ac}" for ac in task.acceptance_criteria])
                
                if task.dependencies:
                    markdown_lines.append(f"\n**Dependencies**: {', '.join(task.dependencies)}")
                
                if task.similar:
                    markdown_lines.append("\n**Similar GitLab Issues:**")
                    for sim in task.similar:
                        status_str = f" ({sim.status})" if sim.status else ""
                        markdown_lines.append(f"- [{sim.kind} {sim.id}]({sim.url}){status_str} - similarity: {sim.similarity:.2f}")
                
                markdown_lines.append("")
            
            markdown_lines.append("---\n")
        
        bundle = GeneratedBacklogBundle(
            prompt_id=prompt_id,
            project_id=state["project_id"],
            epics=final_epics,
            assumptions=state["draft"].assumptions,
            risks=state["draft"].risks,
            score=state["report"].score,
            coverage_components=state["report"].component_scores,
            clarification_questions=None,
            markdown_text="\n".join(markdown_lines),
        )
        return {"result": bundle}

    async def clarify_node(state: Dict[str, Any]) -> Dict[str, Any]:
        plan = await strategist.propose(
            draft=state["draft"],
            requirements=state.get("requirements", ""),
            component_scores=state["report"].component_scores,
            target_score=float(state.get("target", 1.0)),
        )
        
        prompt_id = state.get("prompt_id") or uuid4()
        
        # Use enriched epics if available
        final_epics = state["draft"].epics
        if state.get("mappings"):
            final_epics = state["mappings"].enriched_epics
        
        from models.request_models import ClarificationQuestion
        questions = [ClarificationQuestion(id=q["id"], text=q["text"]) for q in plan.questions]
        
        bundle = GeneratedBacklogBundle(
            prompt_id=prompt_id,
            project_id=state["project_id"],
            epics=final_epics,
            assumptions=state["draft"].assumptions,
            risks=state["draft"].risks,
            score=state["report"].score,
            coverage_components=state["report"].component_scores,
            clarification_questions=questions,
        )
        return {"result": bundle}

    builder = StateGraph(State)
    builder.add_node("init", init_node)
    builder.add_node("analyze", analyze_node)
    builder.add_node("retrieve", retrieve_node)
    builder.add_node("fetch_backlog", fetch_backlog_node)
    builder.add_node("draft", draft_node)
    builder.add_node("map_duplicates", map_duplicates_node)
    builder.add_node("audit", audit_node)
    builder.add_node("supervisor", supervisor_node)
    builder.add_node("finalize", finalize_node)
    builder.add_node("clarify", clarify_node)

    builder.add_edge(START, "init")
    builder.add_edge("init", "analyze")
    builder.add_edge("analyze", "retrieve")
    builder.add_edge("retrieve", "fetch_backlog")
    builder.add_edge("fetch_backlog", "draft")
    builder.add_edge("draft", "map_duplicates")
    builder.add_edge("map_duplicates", "audit")
    builder.add_edge("audit", "supervisor")
    # supervisor returns Command to goto next node
    builder.add_edge("finalize", END)
    builder.add_edge("clarify", END)

    checkpointer = InMemorySaver()
    
    graph = builder.compile(checkpointer=checkpointer)
    return graph


