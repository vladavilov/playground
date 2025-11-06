"""Requirement Enhancer Expert - Enhances individual BR/FR items with AI."""

from typing import Dict, Any, List
from pydantic import BaseModel, Field
from utils.llm_client_factory import create_llm
from orchestrator.prompts import (
    build_chat_prompt, 
    BUSINESS_REQUIREMENT_ENHANCER,
    FUNCTIONAL_REQUIREMENT_ENHANCER
)


class RequirementEnhancer:
    """Expert that enhances individual requirements with detailed descriptions and acceptance criteria."""
    
    def __init__(self) -> None:
        pass
    
    def _format_retrieved_context(self, context: Any) -> str:
        """Format retrieved context for LLM prompt.
        
        Args:
            context: Retrieved context object with context_answer, key_facts, citations
            
        Returns:
            Formatted context string
        """
        contexts: List[str] = []
        if context.context_answer:
            contexts.append(str(context.context_answer))
        contexts.extend([str(k) for k in context.key_facts or []])
        contexts.extend([
            f"[{c.document_name}] {c.text_preview}" 
            for c in (context.citations or [])
        ])
        
        return "\n\n".join(contexts) if contexts else "No additional context available"
    
    def _format_requirement_for_prompt(self, requirement_content: Dict[str, Any], requirement_id: str) -> str:
        """Format requirement content for LLM prompt.
        
        Args:
            requirement_content: Dict containing requirement fields
            requirement_id: Requirement identifier
            
        Returns:
            Formatted requirement string
        """
        return f"""
ID: {requirement_content.get('id', requirement_id)}
Title: {requirement_content.get('title', '')}
Description: {requirement_content.get('description', '')}
Acceptance Criteria: {'; '.join(requirement_content.get('acceptance_criteria', []))}
Priority: {requirement_content.get('priority', 'Must')}
"""
    
    async def enhance(
        self,
        requirement_type: str,
        requirement_id: str,
        current_content: Dict[str, Any],
        retrieved_context: Any
    ) -> Dict[str, Any]:
        """Enhance a single requirement with AI-generated content.
        
        Selects appropriate prompt based on requirement_type:
        - 'business': Uses BUSINESS_REQUIREMENT_ENHANCER (business context focus)
        - 'functional': Uses FUNCTIONAL_REQUIREMENT_ENHANCER (technical/functional focus)
        
        Args:
            requirement_type: Type of requirement ('business' or 'functional')
            requirement_id: Requirement identifier
            current_content: Current requirement content dict
            retrieved_context: Retrieved context from ContextRetriever
            
        Returns:
            Dict with enhanced requirement fields (id, title, description, acceptance_criteria, rationale, priority)
        """
        # Format context and requirement using helper methods
        context_str = self._format_retrieved_context(retrieved_context)
        requirement_str = self._format_requirement_for_prompt(current_content, requirement_id)
        
        # Define output schema (reusable for both prompt types)
        class EnhancedOut(BaseModel):
            id: str = Field(..., description="Requirement ID")
            title: str = Field(..., description="Enhanced title")
            description: str = Field(..., description="Enhanced description")
            rationale: str = Field(default="", description="Enhanced rationale")
            acceptance_criteria: List[str] = Field(default_factory=list, description="Enhanced acceptance criteria")
            priority: str = Field(default="Must", description="Priority")
        
        # Select appropriate prompt based on requirement type
        if requirement_type == "business":
            tmpl = build_chat_prompt(BUSINESS_REQUIREMENT_ENHANCER)
        else:  # functional
            tmpl = build_chat_prompt(FUNCTIONAL_REQUIREMENT_ENHANCER)
        
        # Build and invoke enhancement chain
        llm = create_llm()
        chain = tmpl | llm.with_structured_output(EnhancedOut)
        
        enhanced: EnhancedOut = await chain.ainvoke({
            "current_requirement": requirement_str,
            "retrieved_context": context_str
        })
        
        # Return enhanced requirement as dict
        return {
            "id": enhanced.id,
            "title": enhanced.title,
            "description": enhanced.description,
            "acceptance_criteria": enhanced.acceptance_criteria,
            "rationale": enhanced.rationale or None,
            "priority": enhanced.priority or current_content.get('priority', 'Must')
        }

