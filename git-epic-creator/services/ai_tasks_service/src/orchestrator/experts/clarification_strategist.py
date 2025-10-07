"""ClarificationStrategist expert - generates targeted clarification questions."""

from typing import Dict, List
from pydantic import BaseModel, Field
from langchain_core.prompts import ChatPromptTemplate

from models.agent_models import BacklogDraft, EvaluationReport, ClarificationPlan
from orchestrator.experts.clients.llm import get_llm


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
        
        prompt_tmpl = ChatPromptTemplate.from_messages([
            (
                "system",
                "You are a Clarification Strategist. The backlog quality score is below target. "
                "Generate 3-5 targeted questions to clarify requirements and improve the score.\n\n"
                "**Focus on weak areas:**\n"
                "- **coverage**: Missing requirements or unclear scope\n"
                "- **specificity**: Ambiguous technical details (APIs, data models, integrations)\n"
                "- **feasibility**: Unclear constraints, dependencies, or risks\n"
                "- **duplication**: Ambiguous boundaries causing overlaps\n\n"
                "Respond ONLY with JSON: {{\"questions\": [{{\"id\": string, \"text\": string}}], "
                "\"focus_areas\": string[]}}",
            ),
            (
                "human",
                "### Requirements\n{requirements}\n\n"
                "### Current Scores\n{scores}\n\n"
                "### Weak Areas\n{weak_areas}\n\n"
                "Target: {target_score:.2f}",
            ),
        ])
        
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


