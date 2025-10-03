"""Evaluator expert - computes quality scores for generated backlog."""

from typing import Dict
from pydantic import BaseModel, Field
from langchain_core.prompts import ChatPromptTemplate

from models.agent_models import BacklogDraft, AuditFindings, EvaluationReport
from config import get_ai_tasks_settings
from orchestrator.llm import get_llm


class Evaluator:
    """Expert that evaluates backlog quality against rubric axes."""

    def __init__(self) -> None:
        settings = get_ai_tasks_settings()
        self.weights = settings.EVAL_WEIGHTS

    async def evaluate(
        self,
        draft: BacklogDraft,
        findings: AuditFindings,
        requirements: str,
    ) -> EvaluationReport:
        """Evaluate backlog quality and compute score.
        
        Args:
            draft: Generated backlog draft
            findings: Audit findings
            requirements: Original requirements text
            
        Returns:
            EvaluationReport with overall score, component scores, and gaps
        """
        class EvalOut(BaseModel):
            coverage: float = Field(..., ge=0.0, le=1.0, description="Coverage of requirements")
            specificity: float = Field(..., ge=0.0, le=1.0, description="Technical specificity")
            feasibility: float = Field(..., ge=0.0, le=1.0, description="Feasibility within constraints")
            duplication: float = Field(..., ge=0.0, le=1.0, description="Absence of duplicates")
            rationale: str = Field(..., description="Brief scoring explanation")
            gaps: list[str] = Field(default_factory=list, description="Top 3 identified gaps")

        # Build evaluation prompt
        backlog_summary = self._summarize_for_eval(draft, findings)
        
        prompt_tmpl = ChatPromptTemplate.from_messages([
            (
                "system",
                "You are a Backlog Evaluator. Score the backlog on four axes (0.0 to 1.0):\n\n"
                "- **coverage**: Does every significant requirement have at least one task with traceable ACs?\n"
                "- **specificity**: Are tasks technically clear, testable (G/W/T), with non-ambiguous scopes?\n"
                "- **feasibility**: Are tasks viable within constraints; dependencies identified; risks called out?\n"
                "- **duplication**: Are duplicates minimized (penalize near-duplicates)?\n\n"
                "Include:\n"
                "- **rationale**: Brief explanation\n"
                "- **gaps**: Top 3 gaps (what's missing or unclear)\n\n"
                "Respond ONLY with JSON: {\"coverage\": float, \"specificity\": float, \"feasibility\": float, "
                "\"duplication\": float, \"rationale\": string, \"gaps\": string[]}",
            ),
            ("human", "### Requirements\n{requirements}\n\n### Backlog & Findings\n{backlog_summary}"),
        ])
        
        llm = get_llm()
        chain = prompt_tmpl | llm.with_structured_output(EvalOut)
        out: EvalOut = await chain.ainvoke({
            "requirements": requirements,
            "backlog_summary": backlog_summary,
        })
        
        # Compute weighted score
        component_scores = {
            "coverage": float(out.coverage),
            "specificity": float(out.specificity),
            "feasibility": float(out.feasibility),
            "duplication": float(out.duplication),
        }
        
        overall_score = sum(
            component_scores.get(k, 0.0) * self.weights.get(k, 0.0)
            for k in component_scores
        )
        
        return EvaluationReport(
            score=overall_score,
            component_scores=component_scores,
            rationale=out.rationale,
            gaps=out.gaps,
        )

    def _summarize_for_eval(self, draft: BacklogDraft, findings: AuditFindings) -> str:
        """Summarize backlog and findings for evaluation."""
        lines = [
            f"**Epics**: {len(draft.epics)}",
            f"**Total Tasks**: {sum(len(e.tasks) for e in draft.epics)}",
            f"**Assumptions**: {len(draft.assumptions)}",
            f"**Risks**: {len(draft.risks)}",
            "",
            "**Audit Findings:**",
            f"- Issues: {len(findings.issues)}",
            f"- Suggestions: {len(findings.suggestions)}",
            f"- Overlaps: {len(findings.overlaps)}",
            "",
        ]
        
        if findings.issues:
            lines.append("**Top Issues:**")
            lines.extend([f"- {issue}" for issue in findings.issues[:3]])
            lines.append("")
        
        # Sample epic/task details
        lines.append("**Sample Epics:**")
        for epic in draft.epics[:2]:
            lines.append(f"- {epic.id}: {epic.title} ({len(epic.tasks)} tasks)")
            for task in epic.tasks[:2]:
                lines.append(f"  - {task.id}: {task.title} ({len(task.acceptance_criteria)} ACs)")
        
        return "\n".join(lines)


