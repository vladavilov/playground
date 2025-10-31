"""ClarificationStrategist expert - generates targeted clarification questions."""

from typing import Dict, List
from pydantic import BaseModel, Field

from task_models.agent_models import BacklogDraft, EvaluationReport, ClarificationPlan
from orchestrator.experts.clients.llm import get_llm
from orchestrator.prompts import CLARIFICATION_STRATEGIST, build_chat_prompt


class ClarificationStrategist:
    """Expert that proposes clarification questions to improve backlog quality."""

    def __init__(self) -> None:
        pass

    async def propose(
        self,
        draft: BacklogDraft,
        requirements: str,
        component_scores: Dict[str, float],
        target_score: float,
    ) -> ClarificationPlan:
        """Generate clarification questions to raise score above target.
        
        Args:
            draft: Generated backlog draft
            requirements: Original requirements text
            component_scores: Scores per axis (coverage, specificity, feasibility, duplication)
            target_score: Target overall score
            
        Returns:
            ClarificationPlan with questions and focus areas
        """
        class QuestionOut(BaseModel):
            id: str
            text: str

        class ClarificationOut(BaseModel):
            questions: List[QuestionOut] = Field(default_factory=list, description="Clarification questions (3-5)")
            focus_areas: List[str] = Field(
                default_factory=list,
                description="Focus areas: coverage/specificity/feasibility/duplication",
            )

        # Identify weak areas
        weak_areas = [
            axis for axis, score in component_scores.items()
            if score < target_score
        ]
        
        prompt_tmpl = build_chat_prompt(CLARIFICATION_STRATEGIST)
        
        llm = get_llm()
        chain = prompt_tmpl | llm.with_structured_output(ClarificationOut)
        out: ClarificationOut = await chain.ainvoke({
            "requirements": requirements,
            "scores": str(component_scores),
            "weak_areas": ", ".join(weak_areas) if weak_areas else "none",
            "target_score": target_score,
        })
        
        questions = [{"id": q.id, "text": q.text} for q in out.questions]
        
        return ClarificationPlan(
            questions=questions,
            focus_areas=out.focus_areas,
        )


