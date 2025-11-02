"""LangGraph pipeline for backlog generation workflow."""

from typing import Any, Dict, Annotated, TypedDict, Literal
from uuid import uuid4
from operator import add
import time
import asyncio

from langgraph.graph import StateGraph, START, END
from langgraph.graph.message import add_messages
from langgraph.types import Command
from langchain_core.messages.utils import trim_messages, count_tokens_approximately

from task_models.request_models import GeneratedBacklogBundle
from task_models.agent_models import AuditFindings
from orchestrator.experts.requirements_analyst import RequirementsAnalyst
from orchestrator.experts.context_retriever import ContextRetriever
from orchestrator.experts.backlog_engineer import BacklogEngineer
from orchestrator.experts.duplicate_mapper import DuplicateMapper
from orchestrator.experts.consistency_auditor import ConsistencyAuditor
from orchestrator.experts.evaluator import Evaluator
from orchestrator.experts.clarification_strategist import ClarificationStrategist
from orchestrator.experts.clients.gitlab_client import GitLabClient
from task_models.agent_models import BacklogDraft
from config import get_ai_tasks_settings
import structlog

logger = structlog.get_logger(__name__)


def _log_node_timing(node_name: str, duration_sec: float, state: Dict[str, Any]) -> None:
    """Log timing for a node execution.
    
    Args:
        node_name: Name of the node
        duration_sec: Duration in seconds
        state: Current state (for project_id logging)
    """
    logger.info(
        "node_execution_timing",
        node=node_name,
        duration_sec=round(duration_sec, 3),
        project_id=str(state.get("project_id", "unknown")),
        iteration=state.get("iteration", [0])[-1] if state.get("iteration") else 0,
    )


async def create_backlog_graph(publisher: Any, *, target: float, max_iters: int):
    """Build a LangGraph StateGraph for the backlog generation workflow.
    
    State keys:
      - project_id: UUID (internal PostgreSQL UUID)
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
      - gitlab_project_ids: list[str] (array of numeric GitLab project IDs)
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
        gitlab_project_ids: list[str]

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
            details_md="**Workflow Initialization**\nStarting requirements analysis pipeline.",
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

    async def analyze_and_fetch_parallel_node(state: Dict[str, Any]) -> Dict[str, Any]:
        """Run analysis and GitLab backlog fetch in parallel (no dependencies)."""
        start_time = time.perf_counter()
        
        # Define analysis task
        async def do_analyze():
            analyze_start = time.perf_counter()
            requirements = state.get("requirements", "")
            analysis = await analyst.analyze(requirements)
            _log_node_timing("analyze", time.perf_counter() - analyze_start, state)
            
            try:
                md_lines: list[str] = [
                    "**Requirements Analysis**",
                    "Extracted structured information from requirements:",
                    "",
                ]
                
                # Intents
                if analysis.intents:
                    md_lines.append(f"**Intents** ({len(analysis.intents)}):")
                    for intent in analysis.intents[:5]:  # Show top 5
                        md_lines.append(f"- {intent}")
                    md_lines.append("")
                
                # Entities
                if analysis.entities:
                    md_lines.append(f"**Domain Entities** ({len(analysis.entities)}):")
                    for entity in analysis.entities[:8]:  # Show top 8
                        md_lines.append(f"- {entity}")
                    md_lines.append("")
                
                # Constraints
                if analysis.constraints:
                    md_lines.append(f"**Constraints/NFRs** ({len(analysis.constraints)}):")
                    for constraint in analysis.constraints[:5]:  # Show top 5
                        md_lines.append(f"- {constraint}")
                
                details_md = "\n".join(md_lines)
                await publisher.publish_backlog_update(
                    project_id=state["project_id"],
                    prompt_id=state.get("prompt_id"),
                    status="analyzing_requirements",
                    thought_summary=f"Extracted {len(analysis.intents)} intents and {len(analysis.entities)} entities.",
                    details_md=details_md,
                )
            except Exception:
                await publisher.publish_backlog_update(
                    project_id=state["project_id"],
                    prompt_id=state.get("prompt_id"),
                    status="analyzing_requirements",
                    thought_summary=f"Extracted {len(analysis.intents)} intents and {len(analysis.entities)} entities.",
                )
            
            return analysis
        
        # Define fetch_backlog task
        async def do_fetch_backlog():
            fetch_start = time.perf_counter()
            settings = get_ai_tasks_settings()
            
            gitlab_project_ids = state.get("gitlab_project_ids", [])
            
            # Handle projects without GitLab integration
            if not gitlab_project_ids or len(gitlab_project_ids) == 0:
                logger.warning(
                    "No GitLab project IDs available; skipping backlog fetch for duplicate detection",
                    project_id=str(state.get("project_id")),
                )
                await publisher.publish_backlog_update(
                    project_id=state["project_id"],
                    prompt_id=state.get("prompt_id"),
                    status="fetching_backlog",
                    thought_summary="Skipping GitLab backlog fetch (project not linked to GitLab).",
                    details_md=(
                        "**GitLab Integration Not Configured**\n\n"
                        "This project is not linked to any GitLab projects. Duplicate detection will be skipped.\n\n"
                        "To enable duplicate detection:\n"
                        "1. Update project settings with valid `gitlab_backlog_project_urls`\n"
                        "2. Ensure GitLab authentication is configured"
                    ),
                )
                _log_node_timing("fetch_backlog", time.perf_counter() - fetch_start, state)
                return {"epics": [], "issues": []}
            
            gitlab_client = GitLabClient(
                base_url=settings.http.GITLAB_CLIENT_SERVICE_URL,
                timeout_sec=settings.http.CONNECTION_TIMEOUT,
            )
            
            auth_header = state.get("auth_header")
            gitlab_token = state.get("gitlab_token")
            
            # Fetch backlog from all configured GitLab projects
            gitlab_backlog = await gitlab_client.fetch_backlog_from_multiple_projects(
                gitlab_project_ids=gitlab_project_ids,
                auth_header=auth_header,
                gitlab_token=gitlab_token,
            )
            
            epic_count = len(gitlab_backlog.get("epics", []))
            issue_count = len(gitlab_backlog.get("issues", []))
            
            # Count projects for logging
            project_counts = {}
            for epic in gitlab_backlog.get("epics", []):
                proj_id = epic.get("project_id", "unknown")
                project_counts[proj_id] = project_counts.get(proj_id, {"epics": 0, "issues": 0})
                project_counts[proj_id]["epics"] += 1
            for issue in gitlab_backlog.get("issues", []):
                proj_id = issue.get("project_id", "unknown")
                if proj_id not in project_counts:
                    project_counts[proj_id] = {"epics": 0, "issues": 0}
                project_counts[proj_id]["issues"] += 1
            
            try:
                md_lines: list[str] = [
                    "**GitLab Backlog Retrieved**",
                    f"Fetched existing epics and issues from **{len(gitlab_project_ids)} project(s)** for duplicate detection:",
                    "",
                ]
                
                # Project breakdown
                for proj_id, counts in project_counts.items():
                    md_lines.append(f"**Project {proj_id}:** {counts['epics']} epics, {counts['issues']} issues")
                md_lines.append("")
                
                # Epics
                epics = gitlab_backlog.get("epics", [])
                if epics:
                    md_lines.append(f"**Epics found** ({epic_count}):")
                    for epic in epics[:5]:  # Show first 5
                        epic_id = epic.get("id", "N/A")
                        epic_title = epic.get("title", "Untitled")
                        epic_status = epic.get("status", "")
                        status_str = f" [{epic_status}]" if epic_status else ""
                        md_lines.append(f"- {epic_id}: {epic_title}{status_str}")
                    if epic_count > 5:
                        md_lines.append(f"- ... and {epic_count - 5} more")
                    md_lines.append("")
                
                # Issues
                issues = gitlab_backlog.get("issues", [])
                if issues:
                    md_lines.append(f"**Issues found** ({issue_count}):")
                    for issue in issues[:5]:  # Show first 5
                        issue_id = issue.get("id", "N/A")
                        issue_title = issue.get("title", "Untitled")
                        issue_status = issue.get("status", "")
                        status_str = f" [{issue_status}]" if issue_status else ""
                        md_lines.append(f"- {issue_id}: {issue_title}{status_str}")
                    if issue_count > 5:
                        md_lines.append(f"- ... and {issue_count - 5} more")
                
                details_md = "\n".join(md_lines)
                await publisher.publish_backlog_update(
                    project_id=state["project_id"],
                    prompt_id=state.get("prompt_id"),
                    status="fetching_backlog",
                    thought_summary=f"Fetched {epic_count} epics and {issue_count} issues from GitLab.",
                    details_md=details_md,
                )
            except Exception:
                await publisher.publish_backlog_update(
                    project_id=state["project_id"],
                    prompt_id=state.get("prompt_id"),
                    status="fetching_backlog",
                    thought_summary=f"Fetched {epic_count} epics and {issue_count} issues from GitLab.",
                )
            
            _log_node_timing("fetch_backlog", time.perf_counter() - fetch_start, state)
            return gitlab_backlog
        
        # Execute both tasks in parallel
        analysis, gitlab_backlog = await asyncio.gather(
            do_analyze(),
            do_fetch_backlog(),
        )
        
        _log_node_timing("analyze_and_fetch_parallel", time.perf_counter() - start_time, state)
        
        return {
            "analysis": analysis,
            "gitlab_backlog": gitlab_backlog,
        }

    async def retrieve_node(state: Dict[str, Any]) -> Dict[str, Any]:
        start_time = time.perf_counter()
        auth_header = state.get("auth_header")
        context = await retriever.retrieve(
            state["analysis"],
            state["project_id"],
            auth_header=auth_header,
        )
        _log_node_timing("retrieve", time.perf_counter() - start_time, state)
        citations = list(context.citations or [])
        
        try:
            total = len(citations)
            top = citations[:5]
            md_lines: list[str] = [
                "**Retrieved context**",
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

    async def validate_context_node(state: Dict[str, Any]) -> Dict[str, Any]:
        """Validate that retrieved context is sufficient for backlog generation.
        
        Raises:
            ValueError: If context is insufficient, preventing LLM calls
        """
        context = state.get("context")
        
        # Check if context is empty or insufficient
        context_answer = context.context_answer if context else ""
        key_facts = context.key_facts if context else []
        
        # Validation criteria
        is_context_empty = (
            not context_answer or 
            context_answer.strip() == "(no context available)" or
            len(context_answer.strip()) < 50
        )
        
        has_no_facts = not key_facts or len(key_facts) == 0
        
        if is_context_empty and has_no_facts:
            # Publish error to user
            await publisher.publish_backlog_update(
                project_id=state["project_id"],
                prompt_id=state.get("prompt_id"),
                status="error",
                thought_summary="Insufficient project context for backlog generation.",
                details_md=(
                    "**⚠️ Context Retrieval Failed**\n\n"
                    "Unable to retrieve sufficient project context (architecture, tech stack, services) "
                    "from the knowledge base.\n\n"
                    "**Possible reasons:**\n"
                    "- Project documentation not yet ingested into GraphRAG\n"
                    "- Requirements are too vague to match existing context\n"
                    "- Knowledge base is empty for this project\n\n"
                    "**Actions:**\n"
                    "1. Ensure project documentation is ingested via Neo4j ingestion service\n"
                    "2. Provide more specific requirements with technical details\n"
                    "3. Include explicit technology stack in requirements if context is unavailable"
                ),
            )
            
            # Raise validation error to halt workflow
            raise ValueError(
                "Insufficient context retrieved. Cannot generate backlog without project-specific "
                "technical context. Please ensure project documentation is ingested or provide "
                "explicit technology stack in requirements."
            )
        
        # If we have minimal context, issue a warning but proceed
        if is_context_empty or has_no_facts:
            await publisher.publish_backlog_update(
                project_id=state["project_id"],
                prompt_id=state.get("prompt_id"),
                status="warning",
                thought_summary="Limited context available. Backlog may lack project-specific details.",
                details_md=(
                    "⚠️ **Limited Context**\n\n"
                    "Retrieved context is minimal. Generated backlog may require refinement "
                    "to align with actual project architecture and technology stack."
                ),
            )
        
        return {}  # No state changes, just validation

    async def draft_node(state: Dict[str, Any]) -> Dict[str, Any]:
        start_time = time.perf_counter()
        draft = await engineer.synthesize(
            state["analysis"],
            state["context"],
            state.get("findings") or AuditFindings(),
        )
        _log_node_timing("draft", time.perf_counter() - start_time, state)
        
        try:
            epic_count = len(draft.epics)
            task_count = sum(len(e.tasks) for e in draft.epics)
            md_lines: list[str] = [
                "**Draft synthesis**",
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

    async def quality_check_parallel_node(state: Dict[str, Any]) -> Dict[str, Any]:
        """OPTIMIZED: Run duplicate mapping and consistency audit in parallel (independent operations)."""
        start_time = time.perf_counter()
        
        # Define duplicate mapping task
        async def do_map_duplicates():
            map_start = time.perf_counter()
            mappings = await mapper.map_duplicates(
                state["draft"],
                state["gitlab_backlog"],
            )
            _log_node_timing("map_duplicates", time.perf_counter() - map_start, state)
            
            stats = mappings.stats
            
            try:
                md_lines: list[str] = [
                    "**Duplicate Mapping Analysis**",
                    "Identified similar items between generated backlog and GitLab:",
                    "",
                ]
                
                total_matches = stats.get("total_matches", 0)
                avg_similarity = stats.get("avg_similarity", 0.0)
                
                md_lines.append(f"**Statistics:**")
                md_lines.append(f"- Total matches: **{total_matches}**")
                md_lines.append(f"- Average similarity: **{avg_similarity:.2f}**")
                md_lines.append("")
                
                # Show top duplicate matches
                if mappings.enriched_epics:
                    high_similarity_items = []
                    for epic in mappings.enriched_epics:
                        if epic.similar:
                            for sim in epic.similar:
                                if sim.similarity >= 0.7:  # High similarity threshold
                                    high_similarity_items.append({
                                        "generated": f"{epic.id}: {epic.title}",
                                        "gitlab": f"{sim.kind} {sim.id}",
                                        "similarity": sim.similarity
                                    })
                        
                        for task in epic.tasks:
                            if task.similar:
                                for sim in task.similar:
                                    if sim.similarity >= 0.7:
                                        high_similarity_items.append({
                                            "generated": f"{task.id}: {task.title}",
                                            "gitlab": f"{sim.kind} {sim.id}",
                                            "similarity": sim.similarity
                                        })
                    
                    if high_similarity_items:
                        md_lines.append(f"**High-confidence matches** (similarity ≥ 0.7):")
                        for item in high_similarity_items[:5]:  # Show top 5
                            md_lines.append(f"- {item['generated']} ↔ {item['gitlab']} ({item['similarity']:.2f})")
                        if len(high_similarity_items) > 5:
                            md_lines.append(f"- ... and {len(high_similarity_items) - 5} more")
                
                details_md = "\n".join(md_lines)
                await publisher.publish_backlog_update(
                    project_id=state["project_id"],
                    prompt_id=state.get("prompt_id"),
                    status="mapping_duplicates",
                    thought_summary=f"Found {total_matches} similar items (avg similarity: {avg_similarity:.2f}).",
                    details_md=details_md,
                )
            except Exception:
                await publisher.publish_backlog_update(
                    project_id=state["project_id"],
                    prompt_id=state.get("prompt_id"),
                    status="mapping_duplicates",
                    thought_summary=f"Found {stats.get('total_matches', 0)} similar items (avg similarity: {stats.get('avg_similarity', 0.0):.2f}).",
                )
            
            return mappings
        
        # Define audit task
        async def do_audit():
            audit_start = time.perf_counter()
            audit_draft = state["draft"]
            
            findings = await auditor.audit(
                audit_draft,
                state.get("requirements", ""),
            )
            _log_node_timing("audit", time.perf_counter() - audit_start, state)
            
            return findings
        
        # Execute both tasks in parallel
        mappings, findings = await asyncio.gather(
            do_map_duplicates(),
            do_audit(),
        )
        
        _log_node_timing("quality_check_parallel", time.perf_counter() - start_time, state)
        
        return {
            "mappings": mappings,
            "findings": findings,
        }

    async def supervisor_node(state: Dict[str, Any]) -> Command[Literal["finalize", "clarify", "draft"]]:
        start_time = time.perf_counter()
        # Build draft for evaluation (use enriched if available)
        eval_draft = state["draft"]
        if state.get("mappings"):
            # Create a draft with enriched epics
            from task_models.agent_models import BacklogDraft
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
        _log_node_timing("supervisor_evaluate", time.perf_counter() - start_time, state)
        
        score = float(getattr(report, "score", 0.0) or 0.0)
        current_iter = int((state.get("iteration") or [0])[-1]) + 1
        
        await publisher.publish_backlog_update(
            project_id=state["project_id"],
            prompt_id=state.get("prompt_id"),
            status="evaluating",
            thought_summary="Evaluated backlog quality against rubric.",
            details_md=(
                "**Evaluation results**\n"
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

    def _build_markdown_text(epics: list) -> str:
        """Build markdown-formatted backlog text from epics."""
        markdown_lines = ["# Generated Backlog\n"]
        for epic in epics:
            markdown_lines.append(f"## Epic: {epic.title}")
            markdown_lines.append(f"**ID**: {epic.id}")
            markdown_lines.append(f"**Description**: {epic.description}\n")
            
            if epic.similar:
                markdown_lines.append("**Similar GitLab Epics:**")
                for sim in epic.similar:
                    status_str = f" ({sim.status})" if sim.status else ""
                    markdown_lines.append(f"- [{sim.kind} {sim.id}]({sim.url}){status_str} - similarity: {sim.similarity:.2f}")
                markdown_lines.append("")
            
            markdown_lines.append("**Tasks**")
            for task in epic.tasks:
                markdown_lines.append(f"**Task**: {task.title}")
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
        
        return "\n".join(markdown_lines)

    async def finalize_node(state: Dict[str, Any]) -> Dict[str, Any]:
        prompt_id = state.get("prompt_id") or uuid4()
        
        # Use enriched epics if available
        final_epics = state["draft"].epics
        if state.get("mappings"):
            final_epics = state["mappings"].enriched_epics
        
        bundle = GeneratedBacklogBundle(
            prompt_id=prompt_id,
            project_id=state["project_id"],
            epics=final_epics,
            assumptions=state["draft"].assumptions,
            risks=state["draft"].risks,
            score=state["report"].score,
            coverage_components=state["report"].component_scores,
            clarification_questions=None,
            markdown_text=_build_markdown_text(final_epics),
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
        
        from task_models.request_models import ClarificationQuestion
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
            markdown_text=_build_markdown_text(final_epics),
        )
        return {"result": bundle}

    builder = StateGraph(State)
    builder.add_node("init", init_node)
    builder.add_node("analyze_and_fetch_parallel", analyze_and_fetch_parallel_node)
    builder.add_node("retrieve", retrieve_node)
    builder.add_node("validate_context", validate_context_node)
    builder.add_node("draft", draft_node)
    builder.add_node("quality_check_parallel", quality_check_parallel_node)
    builder.add_node("supervisor", supervisor_node)
    builder.add_node("finalize", finalize_node)
    builder.add_node("clarify", clarify_node)

    # OPTIMIZED FLOW: 
    # 1. analyze and fetch_backlog run in parallel after init
    # 2. map_duplicates and audit run in parallel after draft
    builder.add_edge(START, "init")
    builder.add_edge("init", "analyze_and_fetch_parallel")
    builder.add_edge("analyze_and_fetch_parallel", "retrieve")
    builder.add_edge("retrieve", "validate_context")
    builder.add_edge("validate_context", "draft")
    builder.add_edge("draft", "quality_check_parallel")
    builder.add_edge("quality_check_parallel", "supervisor")
    # supervisor returns Command to goto next node
    builder.add_edge("finalize", END)
    builder.add_edge("clarify", END)

    # PERFORMANCE: No checkpointer = no state serialization overhead between nodes
    # Workflow is single-shot (no mid-execution persistence needed)
    graph = builder.compile()
    return graph


