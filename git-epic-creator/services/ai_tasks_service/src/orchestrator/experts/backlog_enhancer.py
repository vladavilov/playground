"""Backlog Enhancer Expert - Enhances individual epic/task items with AI."""

from typing import Dict, Any, List
from pydantic import BaseModel, Field
from utils.llm_client_factory import create_llm
from orchestrator.prompts import build_chat_prompt, EPIC_ENHANCER, TASK_ENHANCER


class BacklogEnhancer:
    """Expert that enhances individual epics and tasks with detailed descriptions and acceptance criteria."""
    
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
    
    def _format_item_for_prompt(self, item_content: Dict[str, Any], item_id: str) -> str:
        """Format epic/task content for LLM prompt.
        
        Args:
            item_content: Dict containing item fields
            item_id: Item identifier
            
        Returns:
            Formatted item string
        """
        return f"""
ID: {item_content.get('id', item_id)}
Title: {item_content.get('title', '')}
Description: {item_content.get('description', '')}
Acceptance Criteria: {'; '.join(item_content.get('acceptance_criteria', []))}
Dependencies: {', '.join(item_content.get('dependencies', []))}
"""
    
    def _format_parent_epic_context(self, parent_epic_content: Dict[str, Any] | None) -> str:
        """Format parent epic context for task enhancement.
        
        Args:
            parent_epic_content: Dict containing parent epic fields, or None
            
        Returns:
            Formatted parent epic context string
        """
        if not parent_epic_content:
            return "No parent epic context available"
        
        epic_id = parent_epic_content.get("id", "Unknown")
        epic_title = parent_epic_content.get("title", "")
        epic_description = parent_epic_content.get("description", "")
        epic_acceptance = parent_epic_content.get("acceptance_criteria", [])
        
        return f"""
Parent Epic ID: {epic_id}
Parent Epic Title: {epic_title}
Parent Epic Description:
{epic_description}

Parent Epic Acceptance Criteria:
{chr(10).join([f'- {ac}' for ac in epic_acceptance]) if epic_acceptance else '(none)'}
"""
    
    async def enhance(
        self,
        item_type: str,
        item_id: str,
        current_content: Dict[str, Any],
        retrieved_context: Any,
        parent_epic_content: Dict[str, Any] | None = None
    ) -> Dict[str, Any]:
        """Enhance a single epic or task with AI-generated content.
        
        Selects appropriate prompt based on item_type:
        - 'epic': Uses EPIC_ENHANCER (architecture focus, no parent context)
        - 'task': Uses TASK_ENHANCER (implementation focus, includes parent epic context)
        
        Args:
            item_type: Type of item ('epic' or 'task')
            item_id: Item identifier
            current_content: Current item content dict
            retrieved_context: Retrieved context from ContextRetriever
            parent_epic_content: Optional parent epic content dict (for tasks only)
            
        Returns:
            Dict with enhanced item fields (item_id, title, description, acceptance_criteria, dependencies)
        """
        # Format context and item using helper methods
        context_str = self._format_retrieved_context(retrieved_context)
        item_str = self._format_item_for_prompt(current_content, item_id)
        
        # Define output schema (reusable for both prompt types)
        class EnhancedOut(BaseModel):
            id: str = Field(..., description="Item ID")
            title: str = Field(..., description="Enhanced title")
            description: str = Field(..., description="Enhanced description")
            acceptance_criteria: List[str] = Field(default_factory=list, description="Enhanced acceptance criteria")
            dependencies: List[str] = Field(default_factory=list, description="Dependencies")
        
        llm = create_llm()

        # Select appropriate prompt and build chain based on item type
        if item_type == "epic":
            # Epic enhancement - no parent context needed
            tmpl = build_chat_prompt(EPIC_ENHANCER)
            chain = tmpl | llm.with_structured_output(EnhancedOut)
            
            enhanced: EnhancedOut = await chain.ainvoke({
                "current_item": item_str,
                "retrieved_context": context_str
            })
        else:
            # Task enhancement - include parent epic context
            parent_epic_str = self._format_parent_epic_context(parent_epic_content)
            
            tmpl = build_chat_prompt(TASK_ENHANCER)
            chain = tmpl | llm.with_structured_output(EnhancedOut)
            
            enhanced: EnhancedOut = await chain.ainvoke({
                "current_item": item_str,
                "parent_epic_context": parent_epic_str,
                "retrieved_context": context_str
            })
        
        # Return enhanced item as dict
        return {
            "item_id": enhanced.id,
            "title": enhanced.title,
            "description": enhanced.description,
            "acceptance_criteria": enhanced.acceptance_criteria,
            "dependencies": enhanced.dependencies or []
        }

