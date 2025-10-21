"""ClarificationStrategist expert - generates targeted clarification questions."""

from typing import Dict, List
from pydantic import BaseModel, Field
from langchain_core.prompts import ChatPromptTemplate

from task_models.agent_models import BacklogDraft, EvaluationReport, ClarificationPlan
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
                "You are a Senior Clarification Strategist and Technical Analyst. The generated backlog has quality scores below the target threshold. "
                "Your goal is to formulate **PRECISE, TECHNICAL QUESTIONS** that will elicit missing information needed to improve the backlog.\n\n"
                "# Context\n\n"
                "The backlog quality is evaluated on four axes:\n"
                "1. **coverage** (0.0-1.0): Are all requirements translated into epics/tasks?\n"
                "2. **specificity** (0.0-1.0): Are technical details (APIs, data models, infrastructure) concrete?\n"
                "3. **feasibility** (0.0-1.0): Are tasks realistic, properly sized, and implementable?\n"
                "4. **duplication** (0.0-1.0): Is duplicate/overlapping work minimized?\n\n"
                "Scores below the target indicate missing information or ambiguity in the original requirements.\n\n"
                "# Your Task\n\n"
                "Analyze the weak areas and formulate 3-5 clarification questions that will:\n"
                "- Resolve technical ambiguities (which API? what data format? which service?)\n"
                "- Fill information gaps (missing constraints, integration points, NFRs)\n"
                "- Improve technical specificity (precise endpoints, schemas, configurations)\n"
                "- Clarify boundaries (reduce overlap and duplication)\n\n"
                "# Question Quality Guidelines\n\n"
                "**Good Questions** (specific, technical, answerable):\n"
                "[+] 'Which authentication mechanism should be used: JWT, OAuth2, or API keys?'\n"
                "[+] 'What is the expected response time SLA for the user search API (p95 latency)?'\n"
                "[+] 'Should the notification service use webhooks, polling, or server-sent events?'\n"
                "[+] 'What database schema should be used for the user_profile table (list required fields and types)?'\n"
                "[+] 'Are there existing services for email delivery, or should we integrate with a third-party provider like SendGrid?'\n\n"
                "**Poor Questions** (vague, general, unanswerable):\n"
                "[-] 'Can you clarify the requirements?'\n"
                "[-] 'What about performance?'\n"
                "[-] 'Should we add more features?'\n"
                "[-] 'Tell me more about the users.'\n\n"
                "# Focus Area Mapping\n\n"
                "**coverage** → Ask about:\n"
                "- Requirements not addressed in backlog\n"
                "- Missing user personas or workflows\n"
                "- Uncovered edge cases or error scenarios\n"
                "- Non-functional requirements (performance, security, compliance)\n\n"
                "**specificity** → Ask about:\n"
                "- Exact API endpoints, methods, and response formats\n"
                "- Data model details (schemas, field types, validation rules)\n"
                "- Technology stack choices (frameworks, libraries, platforms)\n"
                "- Configuration and environment requirements\n"
                "- Integration protocols and authentication methods\n\n"
                "**feasibility** → Ask about:\n"
                "- Technical constraints (infrastructure limits, rate limits, quotas)\n"
                "- Existing systems or services to integrate with\n"
                "- Dependencies on other teams or external vendors\n"
                "- Timeline or resource constraints\n"
                "- Risk tolerance and mitigation strategies\n\n"
                "**duplication** → Ask about:\n"
                "- Boundaries between similar features or services\n"
                "- Responsibility assignment (which team/service owns what)\n"
                "- Whether existing functionality can be reused vs. rebuilt\n"
                "- Consolidation opportunities for overlapping work\n\n"
                "# Output Format\n\n"
                "Respond ONLY with JSON:\n"
                "```json\n"
                "{{\n"
                "  \"questions\": [\n"
                "    {{\n"
                "      \"id\": \"Q-001\",\n"
                "      \"text\": \"Specific, technical, answerable question\"\n"
                "    }},\n"
                "    {{\n"
                "      \"id\": \"Q-002\",\n"
                "      \"text\": \"Another targeted question\"\n"
                "    }}\n"
                "  ],\n"
                "  \"focus_areas\": [\"coverage\", \"specificity\"]\n"
                "}}\n"
                "```\n\n"
                "**focus_areas**: List the 1-3 weakest axes that your questions target (e.g., [\"specificity\", \"coverage\"])\n\n"
                "**Remember**: Questions should be TECHNICAL, SPECIFIC, and directly answerable by someone familiar with the requirements. "
                "Aim for questions that will lead to concrete technical details in the next iteration.",
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


