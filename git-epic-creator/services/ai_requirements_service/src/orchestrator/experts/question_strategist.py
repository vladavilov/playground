from typing import List, Optional
from pydantic import BaseModel, Field, model_validator
from workflow_models.requirements_models import ClarificationQuestion
from workflow_models.agent_models import DraftRequirements, ClarificationPlan
from langchain_core.prompts import ChatPromptTemplate
from orchestrator.experts.clients.llm import get_llm


class QuestionStrategist:
    def __init__(self):
        pass

    async def propose(self, *, draft: DraftRequirements, user_prompt: str, component_scores: dict[str, float], target_score: float) -> ClarificationPlan:
        draft_payload = draft.model_dump()
        axes_payload = component_scores
        prompt = user_prompt

        system = (
            "You are a requirements analyst. Given the user's prompt, the current draft requirements, "
            "and axis scores (faithfulness, groundedness, response_relevancy, completeness), propose 1-3 concise, "
            "targeted clarification questions to improve the weakest axes. Respond ONLY with a JSON object with a top-level 'questions' array, e.g. {{\"questions\": [{{...}}]}}."
        )
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

        tmpl = ChatPromptTemplate.from_messages([
            ("system", system),
            ("human", "user_prompt: {prompt}\ndraft: {draft}\naxis_scores: {axes}"),
        ])
        llm = get_llm()
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



