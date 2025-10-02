"""RequirementsAnalyst expert - parses requirements into structured intents."""

from typing import List
from pydantic import BaseModel, Field
from langchain_core.prompts import ChatPromptTemplate

from models.agent_models import RequirementsAnalysis
from orchestrator.llm import get_llm


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

        prompt_tmpl = ChatPromptTemplate.from_messages([
            (
                "system",
                "You are a Senior Agile Architect analyzing requirements for backlog generation. "
                "Extract:\n"
                "- **intents**: 1-5 concise, atomic requirement intents\n"
                "- **entities**: domain entities (e.g., User, Order, Payment)\n"
                "- **constraints**: SLAs, NFRs, compliance requirements\n\n"
                "Respond ONLY with JSON: {\"intents\": string[], \"entities\": string[], \"constraints\": string[]}",
            ),
            ("human", "{requirements}"),
        ])
        
        llm = get_llm()
        chain = prompt_tmpl | llm.with_structured_output(AnalysisOut)
        out: AnalysisOut = await chain.ainvoke({"requirements": requirements})
        
        return RequirementsAnalysis(
            requirements_text=requirements,
            intents=[str(s).strip() for s in out.intents if isinstance(s, str)],
            entities=[str(s).strip() for s in out.entities if isinstance(s, str)],
            constraints=[str(s).strip() for s in out.constraints if isinstance(s, str)],
        )


