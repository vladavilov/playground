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
                "Your goal is to extract structured information EXACTLY AS STATED in requirements, without adding assumptions.\n\n"
                
                "**⚠️ CRITICAL: Zero-Assumption Extraction**\n\n"
                "Extract information ONLY from what is explicitly stated in requirements. DO NOT:\n"
                "- Add technology details not mentioned (e.g., if requirements say 'API', don't assume 'REST API')\n"
                "- Introduce specific frameworks/tools not in requirements (avoid: Express, JWT, Docker unless stated)\n"
                "- Expand generic terms into specifics (e.g., 'authentication' should not become 'JWT authentication')\n"
                "- Make assumptions about implementation technologies\n\n"
                
                "**Extraction Guidelines:**\n\n"
                "1. **Intents** (1-5 atomic, actionable intents):\n"
                "   - Extract WHAT needs to be implemented from requirements AS STATED\n"
                "   - Each intent should be independently implementable\n"
                "   - If requirements say 'REST API' → extract 'REST API'\n"
                "   - If requirements say 'API' generically → extract 'API' (not 'REST API')\n"
                "   - If no technology specified → use functional description only\n"
                "   - Examples (technology-agnostic):\n"
                "     * 'Expose API for [domain capability]' (if 'REST' not mentioned)\n"
                "     * 'Implement data synchronization between [systems]' (if 'real-time' not specified)\n"
                "     * 'Add caching for [resource]' (if 'Redis' not mentioned)\n\n"
                "2. **Entities** (domain and technical entities MENTIONED in requirements):\n"
                "   - Domain entities: Extract actual business objects mentioned (User, Order, Transaction, etc.)\n"
                "   - Technical entities: Extract only if explicitly mentioned (API, Service, Database, etc.)\n"
                "   - Infrastructure entities: Extract only if stated (Container, Kubernetes, etc.)\n"
                "   - DO NOT add entities not mentioned in requirements\n"
                "   - If requirements mention 'customer data' → extract 'Customer'\n"
                "   - If requirements mention 'PostgreSQL' → extract 'PostgreSQL'\n"
                "   - If requirements say 'database' generically → extract 'Database'\n\n"
                "3. **Constraints** (technical and business constraints EXPLICITLY stated):\n"
                "   - Performance: Extract if mentioned (e.g., 'sub-200ms latency', '1000 TPS')\n"
                "   - Scalability: Extract if stated (e.g., '10k concurrent users')\n"
                "   - Security: Extract specific requirements (e.g., 'OAuth 2.0', 'encrypted at rest')\n"
                "   - Compliance: Extract if mentioned (e.g., 'GDPR compliant', 'SOX auditable')\n"
                "   - Technology: Extract ONLY explicitly required/prohibited tech (e.g., 'must use Python', 'no third-party dependencies')\n"
                "   - Integration: Extract specific APIs/systems mentioned (e.g., 'integrate with GitLab API')\n"
                "   - Operational: Extract stated SLAs (e.g., '99.9% uptime')\n"
                "   - DO NOT invent constraints not in requirements\n\n"
                "**Focus**: Extract factual, stated information without elaboration or assumption.\n\n"
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


