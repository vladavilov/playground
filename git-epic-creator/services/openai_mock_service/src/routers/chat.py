from typing import Any, Dict
from fastapi import APIRouter, Depends, HTTPException
import structlog
from auth import require_authentication
from configuration.common_config import get_app_settings
from handlers.base import HandlerRegistry
from handlers.drift_search import (
    DriftHydeHandler, 
    DriftPrimerHandler, 
    DriftLocalExecutorHandler, 
    DriftAggregatorHandler,
    DriftSearchSystemHandler,
    DriftReduceHandler
)
from handlers.graphrag import GraphRAGExtractionHandler, CommunityReportGraphHandler, CommunityReportTextHandler
from handlers.deepeval import (
    DeepEvalClaimsHandler, 
    DeepEvalTruthsHandler, 
    DeepEvalFaithfulnessReasonHandler,
    DeepEvalGEvalHandler, 
    DeepEvalStatementsHandler,
    DeepEvalAnswerRelevancyScoreHandler,
    DeepEvalVerdictsHandler
)
from handlers.workflow import AnalystHandler, EngineerHandler, AuditorHandler, StrategistHandler
from handlers.tasks import (
    TasksRequirementsAnalystHandler,
    TasksBacklogEngineerHandler,
    TasksConsistencyAuditorHandler,
    TasksEvaluatorHandler,
    TasksClarificationStrategistHandler,
)
from handlers.search import ExtractClaimsHandler, GlobalSearchHandler, BasicSearchHandler, QuestionGenerationHandler, SummarizeDescriptionsHandler
from handlers.fallback import FallbackGraphHandler

logger = structlog.get_logger(__name__)
router = APIRouter()

# Initialize handler registry with handlers in priority order (most specific first)
handler_registry = HandlerRegistry()

# DRIFT-search handlers (highest priority - complete DRIFT workflow)
handler_registry.register(DriftHydeHandler())                # HyDE embeddings phase
handler_registry.register(DriftPrimerHandler())              # Primer phase
handler_registry.register(DriftLocalExecutorHandler())       # Local execution phase
handler_registry.register(DriftAggregatorHandler())          # Aggregation phase
handler_registry.register(DriftSearchSystemHandler())        # System prompt (DRIFT_LOCAL_SYSTEM_PROMPT)
handler_registry.register(DriftReduceHandler())              # Reduce phase (DRIFT_REDUCE_PROMPT)

# GraphRAG handlers (entity extraction and community reports)
handler_registry.register(GraphRAGExtractionHandler())       # Entity/relationship extraction
handler_registry.register(CommunityReportGraphHandler())     # Graph-based reports
handler_registry.register(CommunityReportTextHandler())      # Text-based reports with date_range

# DeepEval metric handlers (evaluation JSON structures)
handler_registry.register(DeepEvalClaimsHandler())                  # Claims extraction
handler_registry.register(DeepEvalTruthsHandler())                  # Truths verification
handler_registry.register(DeepEvalFaithfulnessReasonHandler())      # Faithfulness reason summary
handler_registry.register(DeepEvalGEvalHandler())                   # GEval scoring
handler_registry.register(DeepEvalStatementsHandler())              # Statement extraction
handler_registry.register(DeepEvalAnswerRelevancyScoreHandler())    # Answer relevancy scoring
handler_registry.register(DeepEvalVerdictsHandler())                # Verdicts evaluation

# AI Tasks Service handlers (backlog generation workflow)
# These handlers check system vs user content separately for precise matching
handler_registry.register(TasksRequirementsAnalystHandler())     # RequirementsAnalyst intents/entities/constraints
handler_registry.register(TasksBacklogEngineerHandler())         # BacklogEngineer epics/tasks synthesis
handler_registry.register(TasksConsistencyAuditorHandler())      # ConsistencyAuditor issues/suggestions/overlaps
handler_registry.register(TasksEvaluatorHandler())               # Evaluator rationale/gaps (supports OLD & NEW formats)
handler_registry.register(TasksClarificationStrategistHandler()) # ClarificationStrategist questions

# AI Workflow handlers (requirements engineering system prompts)
handler_registry.register(AnalystHandler())                  # PromptAnalyst intents
handler_registry.register(EngineerHandler())                 # RequirementsEngineer BR/FR
handler_registry.register(AuditorHandler())                  # ConsistencyAuditor critique
handler_registry.register(StrategistHandler())               # QuestionStrategist questions

# GraphRAG search and summarization handlers
handler_registry.register(ExtractClaimsHandler())            # Extract claims with delimiters
handler_registry.register(GlobalSearchHandler())             # Global search (map) with points
handler_registry.register(BasicSearchHandler())              # Basic search with tables
handler_registry.register(QuestionGenerationHandler())       # Question generation bullets
handler_registry.register(SummarizeDescriptionsHandler())    # Description summarization

# Fallback handler (lowest priority - always matches, generates default graph)
handler_registry.register(FallbackGraphHandler())


@router.post("/chat/completions", dependencies=[Depends(require_authentication)])
@router.post("/v1/chat/completions", dependencies=[Depends(require_authentication)])
async def chat_completions(body: Dict[str, Any]) -> Dict[str, Any]:
    settings = get_app_settings()
    model = body.get("model")
    messages = body.get("messages")
    response_format = body.get("response_format", {})
    
    # Enhanced logging for debugging
    logger.info(
        "oai_chat_request_received",
        model=model,
        model_type=type(model).__name__ if model is not None else "None",
        messages_present=messages is not None,
        messages_type=type(messages).__name__ if messages is not None else "None",
        messages_length=len(messages) if isinstance(messages, list) else 0,
        response_format_present=bool(response_format),
        body_keys=list(body.keys()),
    )
    
    # Validation with detailed error messages
    if model is None or (isinstance(model, str) and not model.strip()):
        logger.error(
            "validation_failed_model",
            model=model,
            model_type=type(model).__name__ if model is not None else "None",
            body=body,
        )
        raise HTTPException(
            status_code=400,
            detail=f"Bad Request: 'model' field is required and must be non-empty (received: {model!r})"
        )
    
    if messages is None:
        logger.error(
            "validation_failed_messages",
            messages=messages,
            body=body,
        )
        raise HTTPException(
            status_code=400,
            detail="Bad Request: 'messages' field is required"
        )
    
    # Log incoming messages for observability
    try:
        logger.info("oai_chat_request", model=model, messages=messages, response_format=response_format)
    except Exception as e:
        logger.info("oai_chat_request_unloggable", error=str(e))
    
    try:
        # Build combined text for handler matching
        all_text = "\n".join([str(m.get("content", "")) for m in messages if isinstance(m, dict)])
        lower_all = all_text.lower()
        
        # Find appropriate handler
        handler = handler_registry.find_handler(messages, all_text, lower_all)
        
        if handler is None:
            # This should never happen since FallbackGraphHandler always matches
            logger.warning("no_handler_found_using_default")
            generated = '{\n  "nodes": [],\n  "relationships": []\n}'
        else:
            handler_name = type(handler).__name__
            logger.info("handler_matched", handler=handler_name, message_preview=all_text[:200])
            generated = handler.generate_response(messages, all_text, model)
    
    except Exception as exc:
        logger.warning("chat_generation_failed", error=str(exc))
        generated = '{\n  "nodes": [],\n  "relationships": []\n}'
    
    logger.info("completion_output", text=generated, response_format_requested=bool(response_format), model=model)
    
    # Return the same model that was requested (support both standard and fast models)
    return {
        "id": "cmpl-mock-000",
        "object": "chat.completion",
        "created": 1700000000,
        "model": model,  # Echo back the requested model
        "choices": [
            {
                "index": 0,
                "message": {"role": "assistant", "content": generated},
                "finish_reason": "stop",
            }
        ],
        "usage": {"prompt_tokens": 3, "completion_tokens": 2, "total_tokens": 5},
    }

