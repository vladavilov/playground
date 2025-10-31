"""RequirementsAnalyst expert - parses requirements into structured intents."""

from typing import List
from pydantic import BaseModel, Field

from task_models.agent_models import RequirementsAnalysis
from orchestrator.experts.clients.llm import get_llm
from orchestrator.prompts import REQUIREMENTS_ANALYST, build_chat_prompt


class RequirementsAnalyst:
    """Expert that analyzes requirements input and extracts structured information."""

    def __init__(self) -> None:
        pass

    async def analyze(self, requirements: str) -> RequirementsAnalysis:
        """Analyze requirements and extract intents, entities, and constraints.
        
        Args:
            requirements: Raw requirements text (markdown or plain text)
            
        Returns:
            RequirementsAnalysis with parsed structure
        """
        class AnalysisOut(BaseModel):
            intents: List[str] = Field(default_factory=list, description="Key intents (1-5)")
            entities: List[str] = Field(default_factory=list, description="Domain entities")
            constraints: List[str] = Field(default_factory=list, description="Constraints/SLAs")

        prompt_tmpl = build_chat_prompt(REQUIREMENTS_ANALYST)
        
        llm = get_llm(use_fast_model=True)
        chain = prompt_tmpl | llm.with_structured_output(AnalysisOut)
        out: AnalysisOut = await chain.ainvoke({"requirements": requirements})
        
        return RequirementsAnalysis(
            requirements_text=requirements,
            intents=[str(s).strip() for s in out.intents if isinstance(s, str)],
            entities=[str(s).strip() for s in out.entities if isinstance(s, str)],
            constraints=[str(s).strip() for s in out.constraints if isinstance(s, str)],
        )


