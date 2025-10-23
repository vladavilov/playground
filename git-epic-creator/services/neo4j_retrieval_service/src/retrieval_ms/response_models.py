"""Pydantic models for validating and normalizing LLM responses in retrieval pipeline.

These models provide:
- Automatic normalization of mixed string/dict inputs
- Validation with graceful fallbacks
- Type-safe response processing
- Clear error messages for debugging
"""

from pydantic import BaseModel, Field, field_validator
from typing import List, Optional, Any
import structlog

logger = structlog.get_logger(__name__)


class Followup(BaseModel):
    """Follow-up question with optional community targeting."""
    
    question: str = Field(..., min_length=1, description="Follow-up question text")
    target_communities: Optional[List[int]] = Field(
        default_factory=list,
        description="Optional list of community IDs to target"
    )
    
    @field_validator("question", mode="before")
    @classmethod
    def ensure_string(cls, v):
        """Ensure question is a non-empty string."""
        if not isinstance(v, str):
            return str(v) if v else ""
        return v.strip()


class PrimerResponse(BaseModel):
    """Response from primer stage: initial answer + follow-up questions."""
    
    initial_answer: str = Field(default="", description="Initial answer to user question")
    followups: List[Followup] = Field(default_factory=list, description="Follow-up questions to explore")
    rationale: str = Field(default="", description="Explanation of primer strategy")
    
    @field_validator("followups", mode="before")
    @classmethod
    def normalize_followups(cls, v):
        """Convert plain strings to dict format for backwards compatibility.
        
        Handles LLM returning ["question1", "question2"] instead of 
        [{"question": "question1"}, {"question": "question2"}]
        """
        if not isinstance(v, list):
            logger.warning("primer_followups_not_list", type=type(v).__name__)
            return []
        
        normalized = []
        for idx, item in enumerate(v):
            if isinstance(item, str):
                logger.debug("primer_followup_string_normalized", index=idx, question=item[:50])
                normalized.append({"question": item})
            elif isinstance(item, dict):
                normalized.append(item)
            else:
                logger.warning("primer_followup_invalid_type", index=idx, type=type(item).__name__)
        
        return normalized


class Citation(BaseModel):
    """Citation reference to chunk with optional span and document name."""
    
    chunk_id: Optional[str] = Field(default=None, description="Chunk ID reference (string)")
    span: str = Field(default="", description="Text span or excerpt")
    document_name: Optional[str] = Field(default=None, description="Source document title")
    
    @field_validator("chunk_id", mode="before")
    @classmethod
    def normalize_chunk_id(cls, v):
        """Ensure chunk_id is a string (handles various ID formats).
        
        Logs warnings when chunk_id is None (indicates LLM output error that will be filtered).
        """
        if v is None:
            logger.warning(
                "citation_model_null_chunk_id",
                message="Citation has None chunk_id (LLM output error - will be filtered in post-processing)"
            )
            return None
        
        # Empty string after strip is also problematic
        if isinstance(v, str) and not v.strip():
            logger.warning(
                "citation_model_empty_chunk_id",
                original_value=repr(v),
                message="Citation has empty/whitespace chunk_id (LLM output error - will be filtered)"
            )
            return None  # Normalize to None for consistent filtering
        
        # Log when coercing non-string types to aid debugging
        if not isinstance(v, str):
            logger.info(
                "citation_chunk_id_coerced",
                original_type=type(v).__name__,
                original_value=str(v)[:50],
                message="Coerced non-string chunk_id to string"
            )
        
        return str(v).strip()  # Strip whitespace for consistency
    
    @field_validator("span", mode="before")
    @classmethod
    def ensure_string(cls, v):
        """Ensure span is a string."""
        if not isinstance(v, str):
            return str(v) if v else ""
        return v
    
    @field_validator("document_name", mode="before")
    @classmethod
    def normalize_document_name(cls, v):
        """Ensure document_name is a string or None."""
        if v is None:
            return None
        if not isinstance(v, str):
            return str(v)
        return v


class LocalExecutorResponse(BaseModel):
    """Response from local executor: answer to follow-up with citations and new follow-ups."""
    
    answer: str = Field(default="", description="Answer to follow-up question")
    citations: List[Citation] = Field(default_factory=list, description="Citations to source chunks")
    new_followups: List[Followup] = Field(default_factory=list, description="Additional follow-up questions")
    confidence: float = Field(default=0.0, ge=0.0, le=1.0, description="Confidence score [0..1]")
    should_continue: bool = Field(default=False, description="Whether to continue exploration")
    
    @field_validator("new_followups", mode="before")
    @classmethod
    def normalize_new_followups(cls, v):
        """Convert plain strings to dict format - CRITICAL FIX.
        
        This validator fixes the AttributeError: 'str' object has no attribute 'get'
        that occurs when LLM returns new_followups as plain strings instead of objects.
        """
        if not isinstance(v, list):
            logger.warning("local_executor_new_followups_not_list", type=type(v).__name__)
            return []
        
        normalized = []
        for idx, item in enumerate(v):
            if isinstance(item, str):
                logger.info(
                    "local_executor_followup_string_normalized",
                    index=idx,
                    question=item[:50],
                    message="Normalized string to object format"
                )
                normalized.append({"question": item})
            elif isinstance(item, dict):
                normalized.append(item)
            else:
                logger.warning(
                    "local_executor_followup_invalid_type",
                    index=idx,
                    type=type(item).__name__,
                    message="Skipping invalid followup item"
                )
        
        return normalized
    
    @field_validator("citations", mode="before")
    @classmethod
    def normalize_citations(cls, v):
        """Ensure citations is a list."""
        if not isinstance(v, list):
            logger.warning("citations_not_list", type=type(v).__name__)
            return []
        return v
    
    @field_validator("confidence", mode="before")
    @classmethod
    def normalize_confidence(cls, v):
        """Ensure confidence is a float in [0, 1]."""
        try:
            conf = float(v)
            return max(0.0, min(1.0, conf))
        except (TypeError, ValueError):
            logger.warning("confidence_invalid", value=v)
            return 0.0


class KeyFact(BaseModel):
    """Key fact with citations."""
    
    fact: str = Field(..., description="Key fact statement")
    citations: List[str] = Field(default_factory=list, description="Chunk ID citations (strings)")
    
    @field_validator("fact", mode="before")
    @classmethod
    def ensure_string(cls, v):
        """Ensure fact is a string."""
        if not isinstance(v, str):
            return str(v) if v else ""
        return v.strip()
    
    @field_validator("citations", mode="before")
    @classmethod
    def normalize_citations(cls, v):
        """Ensure citations is a list of strings."""
        if not isinstance(v, list):
            return []
        
        normalized = []
        for item in v:
            if item is not None:
                normalized.append(str(item))
        
        return normalized


class AggregatorResponse(BaseModel):
    """Final aggregated response with key facts and uncertainty."""
    
    final_answer: str = Field(default="", description="Final synthesized answer")
    key_facts: List[KeyFact] = Field(default_factory=list, description="Key facts with citations")
    residual_uncertainty: str = Field(default="", description="Remaining uncertainties or gaps")
    
    @field_validator("key_facts", mode="before")
    @classmethod
    def normalize_key_facts(cls, v):
        """Ensure key_facts is a list."""
        if not isinstance(v, list):
            logger.warning("key_facts_not_list", type=type(v).__name__)
            return []
        return v

