from typing import List
from pydantic import BaseModel, Field
from workflow_models.agent_models import PromptAnalysis
from langchain_core.prompts import ChatPromptTemplate
from orchestrator.experts.clients.llm import get_llm

class PromptAnalyst:
    def __init__(self) -> None:
        pass

    async def analyze(self, prompt: str) -> PromptAnalysis:
        class IntentsOut(BaseModel):
            intents: List[str] = Field(default_factory=list)

        prompt_tmpl = ChatPromptTemplate.from_messages([
            ("system", "You are a senior requirements analyst. Extract concise atomic requirement intents. Respond ONLY with JSON object: {{\"intents\": string[1-5]}}"),
            ("human", "{user_prompt}"),
        ])
        llm = get_llm()
        chain = prompt_tmpl | llm.with_structured_output(IntentsOut)
        out: IntentsOut = await chain.ainvoke({"user_prompt": prompt})
        intents = [str(s).strip() for s in out.intents if isinstance(s, str)]
        return PromptAnalysis(prompt=prompt, intents=intents)



