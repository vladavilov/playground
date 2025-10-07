import json
from typing import Any, Dict, List
import structlog
from handlers.base import BaseHandler

logger = structlog.get_logger(__name__)


class DeepEvalClaimsHandler(BaseHandler):
    """Handles DeepEval FaithfulnessMetric claims extraction."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        # Must explicitly request "claims" array output format AND include extraction keywords
        # Avoid matching GEval requests that mention claims in evaluation steps
        claims_array_requested = (
            '"claims"' in lower_text 
            or "'claims'" in lower_text
            or "list of claims" in lower_text
        )
        
        # Secondary: Extraction context with "claims" mention
        extraction_with_claims = (
            ("extract" in lower_text or "comprehensive list of facts" in lower_text or "generate a list of claims" in lower_text)
            and "claims" in lower_text
        )
        
        # Explicitly exclude GEval patterns that might mention claims
        not_geval = not (
            ("score" in lower_text and "reason" in lower_text)
            or "evaluation steps" in lower_text
        )
        
        return (claims_array_requested and not_geval) or (extraction_with_claims and not_geval)

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        logger.info("deepeval_claims_extraction")
        claims = {
            "claims": [
                "Smallpdf provides document upload and organization capabilities",
                "Storage option retains processed files when enabled",
                "Mobile App syncs files to the online portal",
                "Right-click actions include Convert, Compress, and Modify",
                "Collaboration features include e-signatures and large file sending",
                "Users can access files across devices (computer, phone, tablet)"
            ]
        }
        return json.dumps(claims)


class DeepEvalTruthsHandler(BaseHandler):
    """Handles DeepEval FaithfulnessMetric truths verification."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        # Must explicitly request "truths" array output format
        # CRITICAL: Avoid broad patterns like "verify" + "claims" that match GEval evaluation steps
        truths_array_requested = (
            '"truths"' in lower_text 
            or "'truths'" in lower_text
            or "list of truths" in lower_text
            or "array of truths" in lower_text
        )
        verification_context = (
            "determine whether the claims" in lower_text
            or "verify the claims are" in lower_text
            or "for each claim in the list" in lower_text
        )
        # Explicitly exclude GEval patterns
        not_geval = not (
            ("score" in lower_text and "reason" in lower_text)
            or "evaluation steps" in lower_text
            or "assign a score" in lower_text
        )
        return truths_array_requested or (verification_context and not_geval)

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        logger.info("deepeval_truths_verification")
        truths = {
            "truths": ["yes", "yes", "yes", "yes", "yes", "yes"]
            
        }
        return json.dumps(truths)


class DeepEvalFaithfulnessReasonHandler(BaseHandler):
    """Handles DeepEval FaithfulnessMetric final summary/reason step."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        # This is the final FaithfulnessMetric step that asks to summarize contradictions
        # It requests ONLY {"reason": ...} without "score"
        
        # Primary pattern: Faithfulness-specific summary request
        faithfulness_summary = (
            ("faithfulness" in lower_text and "score" in lower_text and "reason" in lower_text)
            or ("contradictions" in lower_text and "summarize" in lower_text)
            or ("list of contradictions" in lower_text and "justify" in lower_text)
        )
        
        # Must request ONLY "reason" field, NOT both "score" and "reason" together
        reason_only_request = (
            '"reason"' in lower_text 
            and '"score"' not in lower_text
        ) or (
            "'reason'" in lower_text 
            and "'score'" not in lower_text
        )
        
        # Alternative pattern: Specific FaithfulnessMetric summary format
        faithfulness_format = (
            "the score is <faithfulness_score> because" in lower_text
            or ("concisely summarize" in lower_text and "faithfulness" in lower_text)
        )
        
        # Must NOT be requesting array outputs
        not_array_output = not (
            ('"claims"' in lower_text or "list of claims" in lower_text)
            or ('"truths"' in lower_text or "list of truths" in lower_text)
            or ('"statements"' in lower_text)
            or ('"verdicts"' in lower_text)
        )
        
        # Must NOT be GEval evaluation steps
        not_geval_steps = not ("evaluation steps" in lower_text)
        
        return (faithfulness_summary or (reason_only_request and faithfulness_format)) and not_array_output and not_geval_steps

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        logger.info("deepeval_faithfulness_reason_summary")
        
        # Check if there are contradictions mentioned in the prompt
        if "contradictions:" in combined_text.lower() and "[]" in combined_text:
            # No contradictions - positive message
            reason = {
                "reason": "The score is 1.0 because the actual output is fully faithful to the retrieval context with no contradictions."
            }
        else:
            # Generic positive response
            reason = {
                "reason": "The score is 0.95 because the actual output aligns well with the retrieval context with minimal discrepancies."
            }
        
        return json.dumps(reason)


class DeepEvalGEvalHandler(BaseHandler):
    """Handles DeepEval GEval metric evaluation."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        # GEval prompts explicitly request JSON with "score" AND "reason" fields
        # Must be highly specific to avoid matching other metric types
        
        # Primary pattern: Explicitly requests score+reason JSON structure
        score_reason_structure = (
            ('"score"' in lower_text and '"reason"' in lower_text)
            or ("'score'" in lower_text and "'reason'" in lower_text)
            or ("score" in lower_text and "reason" in lower_text and "json object" in lower_text)
        )
        
        # Secondary pattern: Evaluation steps methodology (GEval signature)
        evaluation_steps_present = (
            "evaluation steps" in lower_text
            or "given the following evaluation steps" in lower_text
            or "assess the response below" in lower_text
        )
        
        # Tertiary patterns: GEval-specific evaluation contexts
        geval_evaluation_context = (
            ("assign a score" in lower_text and ("0" in lower_text or "10" in lower_text))
            or ("you are an evaluator" in lower_text and "json" in lower_text)
            or ("given the following evaluation steps, assess" in lower_text)
            or ("cite" in lower_text and "derive from" in lower_text and "context" in lower_text)
            or ("grounded" in lower_text and "context" in lower_text and "score" in lower_text)
        )
        
        # Must NOT be requesting array outputs (claims, truths, statements, verdicts)
        not_array_output = not (
            ('"claims"' in lower_text or "list of claims" in lower_text)
            or ('"truths"' in lower_text or "list of truths" in lower_text)
            or ('"statements"' in lower_text or "list of statements" in lower_text)
            or ('"verdicts"' in lower_text or "list of verdicts" in lower_text)
        )
        
        return (score_reason_structure or evaluation_steps_present or geval_evaluation_context) and not_array_output

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        # Check if this is a grounding/citation check
        if "cite" in combined_text.lower() or "derive from" in combined_text.lower():
            logger.info("deepeval_geval_groundedness")
            result = {
                "reason": "The requirements are well-grounded in the retrieved context. They cite specific capabilities from the documentation and derive logical acceptance criteria.",
                "score": 0.85
            }
        # Check if this is a completeness check
        elif "completeness" in combined_text.lower() or "fully addressed" in combined_text.lower():
            logger.info("deepeval_geval_completeness")
            result = {
                "reason": "All user intents are addressed with specific requirements and testable acceptance criteria using Given/When/Then format.",
                "score": 0.90
            }
        else:
            logger.info("deepeval_geval_generic")
            result = {
                "reason": "The output meets the evaluation criteria with minor areas for improvement.",
                "score": 0.80
            }
        return json.dumps(result)


class DeepEvalStatementsHandler(BaseHandler):
    """Handles DeepEval AnswerRelevancyMetric statements extraction."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        # Must explicitly request "statements" array output format
        statements_array_requested = (
            '"statements"' in lower_text 
            or "'statements'" in lower_text
            or "list of statements" in lower_text
            or "array of statements" in lower_text
        )
        
        # Secondary: Extraction context with "statements" mention
        extraction_with_statements = (
            ("break down" in lower_text or "generate statements" in lower_text or "extract statements" in lower_text)
            and "statements" in lower_text
        )
        
        # Explicitly exclude GEval patterns
        not_geval = not (
            ("score" in lower_text and "reason" in lower_text)
            or "evaluation steps" in lower_text
        )
        
        # CRITICAL: Explicitly exclude verdicts requests (ContextualRelevancyMetric)
        # ContextualRelevancyMetric prompts mention "statements" but request "verdicts" output
        not_verdicts = not (
            ('"verdicts"' in lower_text or "list of verdicts" in lower_text)
            or ("verdict" in lower_text and "statement" in lower_text and "relevant" in lower_text)
        )
        
        return (statements_array_requested and not_geval and not_verdicts) or (extraction_with_statements and not_geval and not_verdicts)

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        logger.info("deepeval_answer_relevancy_statements", 
                   message_preview=combined_text[:500] if combined_text else "")
        statements = {
            "statements": [
                "Smallpdf provides document management features",
                "Storage option is available for processed files",
                "Mobile app synchronization is supported",
                "Multiple device access is enabled"
            ]
        }
        return json.dumps(statements)


class DeepEvalAnswerRelevancyScoreHandler(BaseHandler):
    """Handles DeepEval AnswerRelevancyMetric scoring step."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        # AnswerRelevancyMetric scoring step requests ONLY "reason" field (NOT "score" in JSON)
        # Must distinguish from:
        # 1. GEval (has "evaluation steps" and requests BOTH "score" and "reason")
        # 2. Statements extraction (has "statements" array request)
        # 3. FaithfulnessMetric reason (also ONLY "reason", but about "contradictions")
        
        # Must request ONLY "reason" field in JSON, NOT "score"
        reason_only_request = (
            ('"reason"' in lower_text and '"score"' not in lower_text)
            or ("'reason'" in lower_text and "'score'" not in lower_text)
        )
        
        # Must NOT be GEval (which has evaluation steps)
        not_geval = not ("evaluation steps" in lower_text)
        
        # Must NOT be statements extraction
        not_statements = not (
            ('"statements"' in lower_text or "list of statements" in lower_text)
            or ("break down" in lower_text and "statements" in lower_text)
        )
        
        # Must NOT be other array-based extractions
        not_array_output = not (
            ('"claims"' in lower_text or "list of claims" in lower_text)
            or ('"truths"' in lower_text or "list of truths" in lower_text)
            or ('"verdicts"' in lower_text or "list of verdicts" in lower_text)
        )
        
        # AnswerRelevancy-specific patterns (distinguish from FaithfulnessMetric)
        relevancy_context = (
            "answer relevancy score" in lower_text
            or "irrelevant statements" in lower_text
            or ("reasons why the score can't be higher" in lower_text and "irrelevant" in lower_text)
        )
        
        # Must NOT be FaithfulnessMetric (which mentions contradictions)
        not_faithfulness = not (
            "contradictions" in lower_text
            or "faithfulness" in lower_text
        )
        
        return reason_only_request and not_geval and not_statements and not_array_output and relevancy_context and not_faithfulness

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        logger.info("deepeval_answer_relevancy_score",
                   message_preview=combined_text[:500] if combined_text else "")
        
        # AnswerRelevancyMetric expects ONLY "reason" field (score is already computed)
        result = {
            "reason": "The score is excellent because the actual output directly addresses all aspects of the input with relevant and focused content."
        }
        return json.dumps(result)


class DeepEvalVerdictsHandler(BaseHandler):
    """Handles DeepEval ContextualRelevancyMetric verdicts evaluation."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        # Must explicitly request "verdicts" array output format
        verdicts_array_requested = (
            '"verdicts"' in lower_text 
            or "'verdicts'" in lower_text
            or "list of verdicts" in lower_text
            or "array of verdicts" in lower_text
            or "generate a list of json objects to indicate" in lower_text
        )
        relevancy_context = (
            ("contextual" in lower_text and "relevance" in lower_text)
            or ("each claim" in lower_text and "relevant" in lower_text and "context" in lower_text)
        )
        # Explicitly exclude GEval patterns
        not_geval = not (
            ("score" in lower_text and "reason" in lower_text and "json object" in lower_text)
            or "evaluation steps" in lower_text
            or "you are an evaluator" in lower_text
        )
        return verdicts_array_requested or (relevancy_context and not_geval)

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        logger.info("deepeval_contextual_relevancy_verdicts")
        # ContextualRelevancyMetric expects verdicts with "statement" field (not "reason")
        verdicts = {
            "verdicts": [
                {"verdict": "yes", "statement": "Smallpdf provides document upload and organization capabilities"},
                {"verdict": "yes", "statement": "Storage option retains processed files when enabled"},
                {"verdict": "yes", "statement": "Mobile App syncs files to the online portal"},
                {"verdict": "yes", "statement": "Users can access files across multiple devices"}
            ]
        }
        return json.dumps(verdicts)
