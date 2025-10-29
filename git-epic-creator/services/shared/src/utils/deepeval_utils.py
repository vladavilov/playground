"""Shared DeepEval utility module with telemetry permanently disabled and optimized metric execution.

This module provides a centralized way to execute DeepEval metrics with:
- Telemetry/analytics permanently disabled at import time (PostHog, LiteLLM logging)
- Shared LiteLLMModel instance to reduce duplicate calls
- Timeout support for metric execution
- Proper error handling that doesn't fail on telemetry issues
- Direct configuration from shared LLM config

Telemetry is disabled globally when this module is imported for maximum performance.

CRITICAL: This module sets environment variables and monkeypatches PostHog BEFORE
importing DeepEval to ensure all PostHog instances are disabled.
"""

import os
import re
import json
import random
from typing import Dict, Optional, Any
from functools import lru_cache
import asyncio
import structlog
from configuration.llm_config import get_llm_config

logger = structlog.get_logger(__name__)

# Hardcoded timeout for all metric executions (performance optimization)
_METRIC_TIMEOUT_SECONDS = 30.0


# ============================================================================
# STEP 1: SET ENVIRONMENT VARIABLES FIRST (BEFORE POSTHOG/DEEPEVAL IMPORTS)
# ============================================================================
# PostHog environment variables
os.environ["POSTHOG_DISABLED"] = "1"
os.environ["POSTHOG_API_KEY"] = ""
os.environ["POSTHOG_HOST"] = ""
os.environ["POSTHOG_PROJECT_API_KEY"] = ""

# LiteLLM telemetry and logging
os.environ["LITELLM_DISABLE_LOGGING"] = "True"
os.environ["LITELLM_LOGGING_LEVEL"] = "ERROR"
os.environ["LITELLM_DROP_PARAMS"] = "True"
os.environ["LITELLM_TELEMETRY"] = "False"
os.environ["LITELLM_LOG"] = "ERROR"

# LiteLLM retry and timeout configuration for stability
os.environ["LITELLM_NUM_RETRIES"] = "2"
os.environ["LITELLM_REQUEST_TIMEOUT"] = str(_METRIC_TIMEOUT_SECONDS)

# DeepEval telemetry (official documentation: must be "1" to opt out)
os.environ["DEEPEVAL_TELEMETRY_OPT_OUT"] = "1"
os.environ["ERROR_REPORTING"] = "0"  # Explicitly disable error reporting to Confident AI

# Confident AI (DeepEval parent) telemetry/tracing
os.environ["CONFIDENT_TRACE_VERBOSE"] = "0"
os.environ["CONFIDENT_TRACE_FLUSH"] = "0"


# ============================================================================
# STEP 2: MONKEYPATCH POSTHOG CLASS (BEFORE DEEPEVAL IMPORT)
# ============================================================================
try:
    import posthog
    
    # Store original methods before patching
    _original_posthog_init = posthog.Posthog.__init__
    _original_posthog_capture = posthog.Posthog.capture
    _original_posthog_identify = posthog.Posthog.identify
    _original_posthog_group = posthog.Posthog.group
    _original_posthog_post = posthog.Client.post if hasattr(posthog, 'Client') else None
    
    # Patch __init__ to ALWAYS create disabled clients
    def _patched_posthog_init(self, *args, **kwargs):
        # Force all PostHog clients to be disabled
        kwargs['disabled'] = True
        kwargs['enable_exception_autocapture'] = False
        try:
            _original_posthog_init(self, *args, **kwargs)
        except Exception:
            pass  # Ignore initialization errors
        # Double-ensure disabled after init
        self.disabled = True
    
    # Replace all telemetry methods with no-ops
    def _noop(*args, **kwargs):
        return None
    
    # Apply patches to PostHog class
    posthog.Posthog.__init__ = _patched_posthog_init
    posthog.Posthog.capture = _noop
    posthog.Posthog.identify = _noop
    posthog.Posthog.group = _noop
    posthog.Posthog.alias = _noop
    posthog.Posthog.page = _noop
    posthog.Posthog.screen = _noop
    posthog.Posthog.flush = _noop
    
    # Patch Client.post if it exists (low-level HTTP method)
    if hasattr(posthog, 'Client'):
        posthog.Client.post = _noop
    
    # Disable module-level singleton if it exists
    if hasattr(posthog, 'default_client') and posthog.default_client:
        posthog.default_client.disabled = True
    
    # Set module-level disabled flag
    posthog.disabled = True
    
    logger.info("posthog_fully_disabled", 
                methods_patched=["__init__", "capture", "identify", "group", "alias", "page", "screen", "flush", "post"],
                message="PostHog completely disabled via monkeypatching")
    
except (ImportError, AttributeError) as e:
    logger.warning("posthog_not_available", error=str(e))


# ============================================================================
# STEP 3: NOW SAFE TO IMPORT DEEPEVAL (PostHog already neutered)
# ============================================================================
from deepeval.models import LiteLLMModel
from deepeval.metrics import GEval

logger.info("deepeval_utils_initialized", 
            telemetry_disabled=True,
            posthog_monkeypatched=True,
            env_vars_configured=True)


class StrictGEval:
    """GEval wrapper with strict JSON output parsing and sanitization.
    
    This class wraps DeepEval's GEval metric to add:
    - Explicit JSON formatting instructions in the criteria
    - Output sanitization to strip markdown fences and extract JSON
    - Graceful fallback to 0.0 score on parsing failures
    
    Usage:
        Use StrictGEval as a drop-in replacement for GEval in metrics_config:
        
        metrics_config = {
            "groundedness": {
                "class": StrictGEval,
                "kwargs": {
                    "name": "Citations",
                    "criteria": "Does output cite context?",
                    "evaluation_steps": [...],
                    "evaluation_params": [LLMTestCaseParams.ACTUAL_OUTPUT, LLMTestCaseParams.CONTEXT],
                }
            }
        }
    """
    
    def __init__(self, **geval_kwargs):
        """Initialize StrictGEval with enhanced criteria.
        
        Args:
            **geval_kwargs: All standard GEval kwargs (name, criteria, evaluation_steps, etc.)
        """
        # Add strict JSON instruction to criteria
        criteria = geval_kwargs.get("criteria", "")
        strict_suffix = (
            "\n\nIMPORTANT OUTPUT INSTRUCTIONS:\n"
            "1. Follow the evaluation steps PRECISELY - count and calculate as instructed\n"
            "2. Use the scoring scaled provided - DO NOT default to 0.5 or round numbers\n"
            "3. Calculate the score based on actual percentages/coverage (e.g., if 5 out of 20 items pass, score 0.25)\n"
            "4. Output ONLY valid JSON with exact format: "
            '{"score": <precise_float_between_0_and_1>, "reason": "<detailed explanation with calculations>"}.\n'
            "5. In your reason, show your work: state counts, percentages, and how you arrived at the score\n"
            "6. No markdown, no code blocks, no commentary outside the JSON object."
        )
        geval_kwargs["criteria"] = criteria + strict_suffix
        
        # Store for access
        self.name = geval_kwargs.get("name", "StrictGEval")
        
        # Wrap GEval instance
        self._geval = GEval(**geval_kwargs)
        
        # Initialize score attributes
        self.score = None
        self.reason = None
    
    def measure(self, test_case):
        """Measure with output sanitization and fallback handling.
        
        Args:
            test_case: LLMTestCase instance with test data
        """
        try:
            # Call original GEval
            self._geval.measure(test_case)
            
            # If score extraction succeeded, copy attributes
            if hasattr(self._geval, "score") and self._geval.score is not None:
                self.score = float(self._geval.score)
                self.reason = str(getattr(self._geval, "reason", ""))
                return
        except Exception as e:
            logger.warning(
                "geval_measure_failed",
                metric=self.name,
                error=str(e),
                message="GEval measure failed, attempting sanitization"
            )
        
        # Fallback: try sanitizing raw output
        raw_output = getattr(self._geval, "_raw_output", None)
        if raw_output:
            try:
                sanitized = self._sanitize_json(str(raw_output))
                parsed = json.loads(sanitized)
                self.score = float(parsed.get("score", 0.0))
                self.reason = str(parsed.get("reason", ""))
                
                logger.info(
                    "geval_output_sanitized",
                    metric=self.name,
                    score=self.score,
                    message="Successfully sanitized and parsed GEval output"
                )
                return
            except Exception as sanitize_err:
                logger.error(
                    "geval_sanitization_failed",
                    metric=self.name,
                    error=str(sanitize_err),
                    raw_output_preview=str(raw_output)[:200],
                )
        
        # Final fallback: set score to 0.0
        self.score = 0.0
        self.reason = "Failed to parse metric output"
        logger.warning(
            "geval_defaulted_to_zero",
            metric=self.name,
            message="Using default score 0.0 due to parsing failure"
        )
    
    @staticmethod
    def _sanitize_json(text: str) -> str:
        """Strip markdown fences and extract first JSON block.
        
        Args:
            text: Raw text that may contain markdown-wrapped JSON
            
        Returns:
            Sanitized JSON string
        """
        # Remove markdown code blocks (```json ... ``` or ``` ... ```)
        text = re.sub(r'^```(?:json)?\s*\n?', '', text, flags=re.MULTILINE)
        text = re.sub(r'\n?```\s*$', '', text, flags=re.MULTILINE)
        text = text.strip()
        
        # Find first { ... } block using simple brace matching
        start = text.find('{')
        if start == -1:
            return '{}'
        
        depth = 0
        for i in range(start, len(text)):
            if text[i] == '{':
                depth += 1
            elif text[i] == '}':
                depth -= 1
                if depth == 0:
                    return text[start:i+1]
        
        # If no closing brace found, return empty object
        return '{}'


@lru_cache(maxsize=1)
def _get_litellm_model() -> Any:
    """Get or create a cached LiteLLMModel instance from shared configuration.
    
    Uses LRU cache to avoid creating multiple instances with same configuration,
    which reduces duplicate model initialization and API overhead.
    
    Configuration is loaded directly from shared LLM config (get_llm_config).
    Telemetry is already disabled at module import time, so no context manager needed.
    
    Returns:
        LiteLLMModel instance configured for DeepEval metrics
    
    Raises:
        RuntimeError: If OAI_KEY is not configured
        ImportError: If deepeval is not installed
    """
    llm_config = get_llm_config()
    
    if not llm_config.OAI_KEY:
        raise RuntimeError(
            "OpenAI API key not configured. Set OAI_KEY environment variable. "
            "DeepEval requires OpenAI API access for evaluation metrics."
        )
    
    model_instance = LiteLLMModel(
        model=f"azure/{llm_config.OAI_MODEL}",
        api_key=llm_config.OAI_KEY,
        api_base=llm_config.OAI_BASE_URL,
        api_version=llm_config.OAI_API_VERSION,
        response_format={"type": "json_object"},
    )
    logger.info(
        "litellm_model_created",
        model=llm_config.OAI_MODEL,
        api_base=llm_config.OAI_BASE_URL,
        api_version=llm_config.OAI_API_VERSION,
        json_mode_enabled=True,
    )
    
    return model_instance


async def _execute_metric_with_timeout(
    metric: Any,
    test_case: Any,
    metric_name: str,
) -> Optional[float]:
    """Execute a DeepEval metric with timeout protection.
    
    Telemetry is already disabled at module import time for maximum performance.
    Uses hardcoded timeout from _METRIC_TIMEOUT_SECONDS.
    
    Args:
        metric: DeepEval metric instance (e.g., FaithfulnessMetric)
        test_case: LLMTestCase instance with test data
        metric_name: Metric name for logging
    
    Returns:
        Metric score as float, or None if execution failed/timed out
    
    Note:
        This function handles telemetry errors gracefully and doesn't raise
        exceptions for analytics failures.
    """
    logger.debug(
        "metric_execution_starting",
        metric=metric_name,
        timeout=_METRIC_TIMEOUT_SECONDS,
    )
    
    try:
        # Run metric measurement in executor to allow timeout
        loop = asyncio.get_event_loop()
        await asyncio.wait_for(
            loop.run_in_executor(None, metric.measure, test_case),
            timeout=_METRIC_TIMEOUT_SECONDS,
        )
        
        score = float(getattr(metric, "score", 0.0) or 0.0)
        reason = str(getattr(metric, "reason", "No reason provided"))

        logger.debug(
            "metric_execution_completed",
            metric=metric_name,
            score=score,
            reason=reason[:500],
        )
        
        return score
        
    except asyncio.TimeoutError:
        logger.warning(
            "metric_execution_timeout",
            metric=metric_name,
            timeout=_METRIC_TIMEOUT_SECONDS,
        )
        raise
        
    except Exception as e:
        logger.error(
            "metric_execution_failed",
            metric=metric_name,
            error=str(e),
            error_type=type(e).__name__,
        )
        raise


async def _execute_single_metric(
    metric_name: str,
    metric_config: Dict[str, Any],
    model: Any,
    test_case: Any,
) -> float:
    """Execute a single metric with timeout.
    
    Args:
        metric_name: Name of metric for logging
        metric_config: Metric configuration dict with "class" and "kwargs" keys
        model: LiteLLMModel instance
        test_case: LLMTestCase instance
        
    Returns:
        Metric score as float
    """
    metric_class = metric_config["class"]
    metric_kwargs = metric_config.get("kwargs", {})
    metric_kwargs_with_model = {**metric_kwargs, "model": model}
    
    metric_instance = metric_class(**metric_kwargs_with_model)
    
    return await _execute_metric_with_timeout(
        metric_instance,
        test_case,
        metric_name,
    )


async def evaluate_with_metrics(
    test_case: Any,
    metrics_config: Dict[str, Dict[str, Any]],
) -> Dict[str, float]:
    """Evaluate test case with multiple DeepEval metrics.
    
    This is the main entry point for running DeepEval evaluations with:
    - Telemetry disabled at module import
    - Shared model instance from configuration
    - Serialized execution for GEval/StrictGEval metrics to avoid rate limits
    - Random jitter between GEval calls (50-150ms) to distribute load
    - Parallel execution for non-GEval metrics
    - Hardcoded timeout protection (30s per metric)
    - Graceful error handling
    
    Configuration is loaded directly from shared LLM config (get_llm_config).
    
    Args:
        test_case: LLMTestCase instance with test data
        metrics_config: Dict mapping metric names to metric configurations
            Example:
            {
                "faithfulness": {
                    "class": FaithfulnessMetric,
                    "kwargs": {}
                },
                "groundedness": {
                    "class": StrictGEval,
                    "kwargs": {
                        "name": "Citations",
                        "criteria": "...",
                        "evaluation_steps": [...],
                        "evaluation_params": [LLMTestCaseParams.ACTUAL_OUTPUT],
                        "strict_mode": False,
                        "verbose_mode": False,
                    }
                }
            }
    
    Returns:
        Dict mapping metric names to scores (0.0-1.0)
        Failed/timed-out metrics will have score of 0.0
    
    Example:
        from deepeval.metrics import FaithfulnessMetric
        from deepeval.test_case import LLMTestCase, LLMTestCaseParams
        from utils.deepeval_utils import evaluate_with_metrics, StrictGEval
        
        test_case = LLMTestCase(
            input="question",
            actual_output="answer",
            context=["context1", "context2"]
        )
        
        metrics_config = {
            "faithfulness": {"class": FaithfulnessMetric, "kwargs": {}},
            "groundedness": {
                "class": StrictGEval,
                "kwargs": {
                    "name": "Citations",
                    "criteria": "Does output cite context?",
                    "evaluation_steps": ["Check citations", "Verify grounding"],
                    "evaluation_params": [LLMTestCaseParams.ACTUAL_OUTPUT, LLMTestCaseParams.CONTEXT],
                    "strict_mode": False,
                    "verbose_mode": False,
                }
            }
        }
        
        scores = await evaluate_with_metrics(test_case, metrics_config)
        # Returns: {"faithfulness": 0.85, "groundedness": 0.92}
    """
    logger.info(
        "deepeval_evaluation_starting",
        metrics_count=len(metrics_config),
        timeout=_METRIC_TIMEOUT_SECONDS,
    )
    
    scores: Dict[str, float] = {}
    
    try:
        # Get shared model instance (cached, configured from shared LLM config)
        model = _get_litellm_model()
        
        logger.debug("litellm_model_loaded_from_config")
        
        # Separate GEval/StrictGEval metrics from others for serialization
        geval_metrics = {}
        other_metrics = {}
        
        for metric_name, metric_config in metrics_config.items():
            metric_class = metric_config["class"]
            class_name = metric_class.__name__ if hasattr(metric_class, "__name__") else str(metric_class)
            
            if class_name in ("GEval", "StrictGEval"):
                geval_metrics[metric_name] = metric_config
            else:
                other_metrics[metric_name] = metric_config
        
        logger.debug(
            "metrics_categorized",
            geval_count=len(geval_metrics),
            other_count=len(other_metrics),
        )
        
        # Execute GEval metrics serially with jitter to avoid rate limits
        for metric_name, metric_config in geval_metrics.items():
            # Add random jitter before each GEval call (except first)
            if scores:  # Skip jitter for first metric
                jitter_ms = random.uniform(50, 150)
                logger.debug("geval_jitter", metric=metric_name, jitter_ms=jitter_ms)
                await asyncio.sleep(jitter_ms / 1000.0)
            
            try:
                score = await _execute_single_metric(metric_name, metric_config, model, test_case)
                scores[metric_name] = score
            except Exception as e:
                logger.warning(
                    "geval_metric_failed",
                    metric=metric_name,
                    error=str(e),
                    error_type=type(e).__name__,
                )
                scores[metric_name] = 0.0
        
        # Execute other metrics in parallel
        if other_metrics:
            tasks = []
            metric_names = []
            
            for metric_name, metric_config in other_metrics.items():
                task = _execute_single_metric(metric_name, metric_config, model, test_case)
                tasks.append(task)
                metric_names.append(metric_name)
            
            results = await asyncio.gather(*tasks, return_exceptions=True)
            
            for metric_name, result in zip(metric_names, results):
                if isinstance(result, Exception):
                    logger.warning(
                        "metric_execution_exception",
                        metric=metric_name,
                        error=str(result),
                        error_type=type(result).__name__,
                    )
                    scores[metric_name] = 0.0
                elif result is None:
                    scores[metric_name] = 0.0
                else:
                    scores[metric_name] = float(result)
        
        logger.info(
            "deepeval_evaluation_completed",
            scores=scores,
            metrics_executed=len(scores),
            geval_serialized=len(geval_metrics),
            other_parallelized=len(other_metrics),
        )
        
    except Exception as e:
        logger.error(
            "deepeval_evaluation_failed",
            error=str(e),
            error_type=type(e).__name__,
        )
        raise
    
    return scores

