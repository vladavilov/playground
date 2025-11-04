from typing import List, Optional
from pydantic import BaseModel, Field, model_validator
from workflow_models.requirements_models import ClarificationQuestion
from workflow_models.agent_models import DraftRequirements, ClarificationPlan
from utils.llm_client_factory import create_llm
from orchestrator.prompts import build_chat_prompt, QUESTION_STRATEGIST


class QuestionStrategist:
    def __init__(self):
        pass

    async def propose(self, *, draft: DraftRequirements, user_prompt: str, component_scores: dict[str, float], target_score: float) -> ClarificationPlan:
        draft_payload = draft.model_dump()
        axes_payload = component_scores
        prompt = user_prompt

        class QOut(BaseModel):
            id: Optional[str] = None
            text: Optional[str] = None
            expected_impact: Optional[str] = None
            axis: Optional[str] = None
            priority: Optional[int] = None
            expected_score_gain: Optional[float] = None
            targets: List[str] = Field(default_factory=list)
            options: Optional[List[str]] = None

        class QList(BaseModel):
            questions: List[QOut] = Field(default_factory=list)

            @model_validator(mode="before")
            @classmethod
            def _coerce_list_to_obj(cls, value):  # noqa: ANN001
                # Accept both an object with {"questions": [...]} and a bare list [...]
                if isinstance(value, list):
                    return {"questions": value}
                return value

        tmpl = build_chat_prompt(QUESTION_STRATEGIST)
        llm = create_llm(use_fast_model=False)
        chain = tmpl | llm.with_structured_output(QList)
        out: QList = await chain.ainvoke({"prompt": prompt, "draft": draft_payload, "axes": axes_payload})
        questions: list[ClarificationQuestion] = []
        for q in out.questions:
            try:
                questions.append(
                    ClarificationQuestion(
                        id=q.id or "Q_llm",
                        text=q.text or "Please clarify requirements",
                        expected_impact=q.expected_impact or "Improve weakest evaluation axis",
                        axis=q.axis or "completeness",
                        priority=int(q.priority or 1),
                        expected_score_gain=float(q.expected_score_gain or 0.1),
                        targets=list(q.targets or []),
                        options=q.options,
                    )
                )
            except Exception:
                continue
        return ClarificationPlan(questions=questions)

    async def propose_llm(self, *, draft: DraftRequirements, user_prompt: str, component_scores: dict[str, float], target_score: float) -> ClarificationPlan:
        return await self.propose(draft=draft, user_prompt=user_prompt, component_scores=component_scores, target_score=target_score)



