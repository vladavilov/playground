from typing import List
from pydantic import BaseModel, Field
from workflow_models.agent_models import PromptAnalysis
from orchestrator.experts.clients.llm import get_llm
from orchestrator.prompts import build_chat_prompt, PROMPT_ANALYST

class PromptAnalyst:
    def __init__(self) -> None:
        pass

    async def analyze(self, prompt: str) -> PromptAnalysis:
        class IntentsOut(BaseModel):
            intents: List[str] = Field(default_factory=list)

        prompt_tmpl = build_chat_prompt(PROMPT_ANALYST)
        llm = get_llm(use_fast_model=True)
        chain = prompt_tmpl | llm.with_structured_output(IntentsOut)
        out: IntentsOut = await chain.ainvoke({"user_prompt": prompt})
        intents = [str(s).strip() for s in out.intents if isinstance(s, str)]
        return PromptAnalysis(prompt=prompt, intents=intents)



