"""RequirementsAnalyst expert - parses requirements into structured intents."""

from typing import List
from pydantic import BaseModel, Field
from langchain_core.prompts import ChatPromptTemplate

from task_models.agent_models import RequirementsAnalysis
from orchestrator.experts.clients.llm import get_llm


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
                "You are a Senior Technical Architect analyzing requirements to generate technical epics and user stories. "
                "Your goal is to extract structured information that will guide the creation of IMPLEMENTABLE, technically-specific work items.\n\n"
                "**Extraction Guidelines:**\n\n"
                "1. **Intents** (1-5 atomic, actionable intents):\n"
                "   - Focus on WHAT needs to be implemented from a technical perspective\n"
                "   - Each intent should be independently implementable\n"
                "   - Emphasize technical capabilities, not business outcomes\n"
                "   - Examples: 'Expose REST API for user authentication', 'Implement real-time data synchronization', 'Add caching layer for query optimization'\n\n"
                "2. **Entities** (domain and technical entities):\n"
                "   - Domain entities: User, Order, Payment, Account, Transaction, etc.\n"
                "   - Technical entities: Service, API, Database, Queue, Cache, Config, etc.\n"
                "   - Infrastructure entities: Container, Pod, Gateway, LoadBalancer, etc.\n"
                "   - Include both business domain and technical architecture entities\n\n"
                "3. **Constraints** (technical and business constraints):\n"
                "   - Performance: latency thresholds, throughput requirements, response times\n"
                "   - Scalability: concurrent users, data volume, transaction rates\n"
                "   - Security: authentication methods, authorization models, encryption standards\n"
                "   - Compliance: regulatory requirements (GDPR, SOX, PCI-DSS), audit requirements\n"
                "   - Technology: specific frameworks, languages, platforms required/prohibited\n"
                "   - Integration: APIs that must be used, data formats, protocols\n"
                "   - Operational: SLAs, uptime requirements, monitoring needs\n\n"
                "**Focus**: Extract technical implementation signals that will help generate specific, actionable epics and stories.\n\n"
                "Respond ONLY with JSON: {{\"intents\": string[], \"entities\": string[], \"constraints\": string[]}}",
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


