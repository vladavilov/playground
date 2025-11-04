"""BacklogEngineer expert - synthesizes epics and tasks from requirements and context."""

import asyncio
from typing import List, Dict
from pydantic import BaseModel, Field
import structlog

from task_models.agent_models import RequirementsAnalysis, RetrievedContext, BacklogDraft, AuditFindings
from task_models.backlog_models import Epic, Task
from utils.llm_client_factory import create_llm
from orchestrator.prompts import (
    BACKLOG_ENGINEER,
    BACKLOG_ENGINEER_EPICS_ONLY,
    BACKLOG_ENGINEER_TASKS_ONLY,
    build_chat_prompt,
)
from utils.chunk_utils import truncate_chunk_text

logger = structlog.get_logger(__name__)


class BacklogEngineer:
    """Expert that synthesizes epics and tasks following INVEST principles."""

    def __init__(self) -> None:
        pass

    async def synthesize(
        self,
        analysis: RequirementsAnalysis,
        context: RetrievedContext,
        findings: AuditFindings,
    ) -> BacklogDraft:
        """Synthesize epics and tasks using two-phase generation (optimized).
        
        Phase 1: Generate epic outlines with fast model (sequential)
        Phase 2: Generate tasks per epic with fast model (parallel)
        
        Args:
            analysis: Parsed requirements analysis
            context: Retrieved technical context (will be summarized)
            findings: Audit findings from previous iteration (may be empty)
            
        Returns:
            BacklogDraft with generated epics and tasks
        """
        logger.info("backlog_synthesis_starting", phase="two_phase_optimized")
        
        # Summarize context to reduce token usage
        summarized_context = self._summarize_context(context)
        
        # Phase 1: Generate epic outlines (fast model, sequential)
        logger.info("backlog_synthesis_phase1", action="generating_epic_outlines")
        epic_outlines = await self._generate_epics_outline(analysis, summarized_context)
        logger.info(
            "backlog_synthesis_phase1_complete",
            epic_count=len(epic_outlines),
        )
        
        # Phase 2: Generate tasks for each epic in parallel
        logger.info(
            "backlog_synthesis_phase2",
            action="generating_tasks_parallel",
            epic_count=len(epic_outlines),
        )
        tasks_per_epic_coros = [
            self._generate_tasks_for_epic(epic_outline, analysis, summarized_context)
            for epic_outline in epic_outlines
        ]
        
        tasks_per_epic_results = await asyncio.gather(*tasks_per_epic_coros, return_exceptions=True)
        
        # Combine into Epic objects
        epics = []
        for epic_outline, tasks_result in zip(epic_outlines, tasks_per_epic_results):
            if isinstance(tasks_result, Exception):
                logger.warning(
                    "epic_task_generation_failed",
                    epic_id=epic_outline["id"],
                    error=str(tasks_result),
                    error_type=type(tasks_result).__name__,
                )
                tasks = []
            else:
                tasks = tasks_result
            
            epics.append(
                Epic(
                    id=epic_outline["id"],
                    title=epic_outline["title"],
                    description=epic_outline["description"],
                    tasks=tasks,
                )
            )
        
        total_tasks = sum(len(epic.tasks) for epic in epics)
        logger.info(
            "backlog_synthesis_phase2_complete",
            epic_count=len(epics),
            total_tasks=total_tasks,
        )
        
        # Generate assumptions and risks (lightweight)
        logger.info("backlog_synthesis_phase3", action="generating_assumptions_risks")
        assumptions, risks = await self._generate_assumptions_and_risks(analysis, summarized_context, findings)
        
        logger.info("backlog_synthesis_complete", epics=len(epics), tasks=total_tasks)
        
        return BacklogDraft(
            epics=epics,
            assumptions=assumptions,
            risks=risks,
        )

    def _summarize_context(self, context: RetrievedContext) -> RetrievedContext:
        """Reduce context size by trimming context_answer and limiting key_facts.
        
        Args:
            context: Original retrieved context
            
        Returns:
            Summarized context with reduced token count
        """
        MAX_CONTEXT_ANSWER_LENGTH = 10000  # chars
        MAX_KEY_FACTS = 10
        
        summarized_answer = truncate_chunk_text(context.context_answer, MAX_CONTEXT_ANSWER_LENGTH)
        
        return RetrievedContext(
            context_answer=summarized_answer,
            key_facts=context.key_facts[:MAX_KEY_FACTS],
            citations=context.citations,  # Keep citations for reference
        )

    async def _generate_epics_outline(
        self,
        analysis: RequirementsAnalysis,
        context: RetrievedContext,
    ) -> List[Dict[str, str]]:
        """Phase 1: Generate epic outlines only (fast model).
        
        Returns:
            List of epic outlines with id, title, description
        """
        class EpicOutlineOut(BaseModel):
            id: str
            title: str
            description: str
            
        class EpicOutlinesOut(BaseModel):
            epics: List[EpicOutlineOut] = Field(default_factory=list)
        
        # Build summarized prompt (no task details)
        prompt_content = self._build_epics_only_prompt(analysis, context)
        
        prompt_tmpl = build_chat_prompt(BACKLOG_ENGINEER_EPICS_ONLY)
        
        llm = create_llm(use_fast_model=True)  # Use fast model for epics
        chain = prompt_tmpl | llm.with_structured_output(EpicOutlinesOut)
        out: EpicOutlinesOut = await chain.ainvoke({"prompt_content": prompt_content})
        
        return [{"id": e.id, "title": e.title, "description": e.description} for e in out.epics]

    async def _generate_tasks_for_epic(
        self,
        epic_outline: Dict[str, str],
        analysis: RequirementsAnalysis,
        context: RetrievedContext,
    ) -> List[Task]:
        """Phase 2: Generate tasks for a single epic (parallel execution).
        
        Args:
            epic_outline: Epic id, title, description
            analysis: Requirements analysis
            context: Retrieved context (summarized)
        
        Returns:
            List of Task objects for this epic
        """
        class TaskOut(BaseModel):
            id: str
            title: str
            description: str
            acceptance_criteria: List[str] = Field(default_factory=list)
            dependencies: List[str] = Field(default_factory=list)
        
        class TasksOut(BaseModel):
            tasks: List[TaskOut] = Field(default_factory=list)
        
        # Build focused prompt for single epic
        prompt_content = self._build_tasks_for_epic_prompt(epic_outline, analysis, context)
        
        prompt_tmpl = build_chat_prompt(BACKLOG_ENGINEER_TASKS_ONLY)
        
        llm = create_llm(use_fast_model=True)  # Use fast model for tasks too
        chain = prompt_tmpl | llm.with_structured_output(TasksOut)
        out: TasksOut = await chain.ainvoke({"prompt_content": prompt_content})
        
        return [
            Task(
                id=task_out.id,
                title=task_out.title,
                description=task_out.description,
                acceptance_criteria=task_out.acceptance_criteria,
                dependencies=task_out.dependencies,
            )
            for task_out in out.tasks
        ]

    async def _generate_assumptions_and_risks(
        self,
        analysis: RequirementsAnalysis,
        context: RetrievedContext,
        findings: AuditFindings,
    ) -> tuple[List[str], List[str]]:
        """Generate assumptions and risks separately (lightweight operation).
        
        Args:
            analysis: Requirements analysis
            context: Summarized context
            findings: Audit findings
            
        Returns:
            Tuple of (assumptions, risks)
        """
        class AssumptionsRisksOut(BaseModel):
            assumptions: List[str] = Field(default_factory=list)
            risks: List[str] = Field(default_factory=list)
        
        # Build minimal prompt for assumptions/risks
        prompt_content = self._build_assumptions_risks_prompt(analysis, context, findings)
        
        prompt_tmpl = build_chat_prompt(BACKLOG_ENGINEER)  # Reuse main prompt
        
        llm = create_llm(use_fast_model=True)  # Fast model sufficient
        chain = prompt_tmpl | llm.with_structured_output(AssumptionsRisksOut)
        out: AssumptionsRisksOut = await chain.ainvoke({"prompt_content": prompt_content})
        
        return out.assumptions, out.risks

    def _build_epics_only_prompt(
        self,
        analysis: RequirementsAnalysis,
        context: RetrievedContext,
    ) -> str:
        """Build prompt for epic-level decomposition only."""
        lines = [
            "### Requirements",
            analysis.requirements_text[:500] + "..." if len(analysis.requirements_text) > 500 else analysis.requirements_text,
            "",
            "### Intents",
            *([f"- {i}" for i in analysis.intents[:5]] if analysis.intents else ["- (none)"]),
            "",
            "### Technical Context (Summarized)",
            context.context_answer or "(no context available)",
        ]
        
        return "\n".join(lines)

    def _build_tasks_for_epic_prompt(
        self,
        epic_outline: Dict[str, str],
        analysis: RequirementsAnalysis,
        context: RetrievedContext,
    ) -> str:
        """Build prompt for task generation for a single epic."""
        lines = [
            "### Epic to Break Down",
            f"**ID**: {epic_outline['id']}",
            f"**Title**: {epic_outline['title']}",
            f"**Description**: {epic_outline['description']}",
            "",
            "### Requirements Context",
            analysis.requirements_text[:300] + "..." if len(analysis.requirements_text) > 300 else analysis.requirements_text,
            "",
            "### Technical Context",
            context.context_answer or "(no context available)",
        ]
        
        # Add key facts if available
        if context.key_facts:
            lines.extend([
                "",
                "**Key Technical Facts:**",
                *(f"- {kf}" for kf in context.key_facts[:3]),
            ])
        
        return "\n".join(lines)

    def _build_assumptions_risks_prompt(
        self,
        analysis: RequirementsAnalysis,
        context: RetrievedContext,
        findings: AuditFindings,
    ) -> str:
        """Build minimal prompt for assumptions and risks generation."""
        lines = [
            "### Task: Generate assumptions and risks for this backlog",
            "",
            "### Requirements Summary",
            analysis.requirements_text[:200] + "..." if len(analysis.requirements_text) > 200 else analysis.requirements_text,
            "",
            "### Technical Context",
            context.context_answer[:300] + "..." if len(context.context_answer) > 300 else context.context_answer,
        ]
        
        # Include audit findings if available
        if findings.issues:
            lines.extend([
                "",
                "### Identified Issues",
                *(f"- {issue}" for issue in findings.issues[:3]),
            ])
        
        lines.extend([
            "",
            "Generate:",
            "- Assumptions: Technology/approach assumptions made (2-4 items)",
            "- Risks: Technical and delivery risks with mitigation ideas (2-4 items)",
        ])
        
        return "\n".join(lines)

    def _build_prompt(
        self,
        analysis: RequirementsAnalysis,
        context: RetrievedContext,
        findings: AuditFindings,
    ) -> str:
        """Build comprehensive prompt for backlog synthesis."""
        lines = [
            "### Requirements",
            analysis.requirements_text,
            "",
            "### Intents",
            *([f"- {i}" for i in analysis.intents] if analysis.intents else ["- (none)"]),
            "",
            "### Constraints",
            *([f"- {c}" for c in analysis.constraints] if analysis.constraints else ["- (none)"]),
            "",
            "### Technical Context",
            context.context_answer or "(no context available)",
        ]
        
        # Add key facts if available
        if context.key_facts:
            lines.extend([
                "",
                "**Key Facts:**",
                *(f"- {kf}" for kf in context.key_facts[:5]),
            ])
        
        # Include audit findings if this is a revision
        if findings.issues or findings.suggestions:
            lines.extend(["", "### Previous Iteration Feedback"])
            if findings.issues:
                lines.extend([
                    "**Issues to address:**",
                    *(f"- {issue}" for issue in findings.issues[:3]),
                ])
            if findings.suggestions:
                lines.extend([
                    "**Suggestions:**",
                    *(f"- {sug}" for sug in findings.suggestions[:3]),
                ])
        
        return "\n".join(lines)


