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

# DeepEval telemetry
os.environ["DEEPEVAL_TELEMETRY_OPT_OUT"] = "YES"

# Confident AI (DeepEval parent) telemetry/tracing
os.environ["CONFIDENT_TRACE_VERBOSE"] = "NO"
os.environ["CONFIDENT_TRACE_FLUSH"] = "NO"


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

logger.info("deepeval_utils_initialized", 
            telemetry_disabled=True,
            posthog_monkeypatched=True,
            env_vars_configured=True)


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
    )
    
    logger.info(
        "litellm_model_created",
        model=llm_config.OAI_MODEL,
        api_base=llm_config.OAI_BASE_URL,
        api_version=llm_config.OAI_API_VERSION,
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
        
        logger.debug(
            "metric_execution_completed",
            metric=metric_name,
            score=score,
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


async def evaluate_with_metrics(
    test_case: Any,
    metrics_config: Dict[str, Dict[str, Any]],
) -> Dict[str, float]:
    """Evaluate test case with multiple DeepEval metrics in parallel.
    
    This is the main entry point for running DeepEval evaluations with:
    - Telemetry disabled at module import
    - Shared model instance from configuration
    - Parallel execution where possible
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
                    "class": GEval,
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
        from deepeval.metrics import FaithfulnessMetric, GEval
        from deepeval.test_case import LLMTestCase, LLMTestCaseParams
        from utils.deepeval_utils import evaluate_with_metrics
        
        test_case = LLMTestCase(
            input="question",
            actual_output="answer",
            context=["context1", "context2"]
        )
        
        metrics_config = {
            "faithfulness": {"class": FaithfulnessMetric, "kwargs": {}},
            "groundedness": {
                "class": GEval,
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
        
        # Execute metrics in parallel with timeout
        tasks = []
        metric_names = []
        
        for metric_name, metric_config in metrics_config.items():
            metric_class = metric_config["class"]
            metric_kwargs = metric_config.get("kwargs", {})
            
            # Add model to kwargs
            metric_kwargs_with_model = {**metric_kwargs, "model": model}
            
            # Instantiate metric (telemetry already disabled at module import)
            metric_instance = metric_class(**metric_kwargs_with_model)
            
            # Schedule execution
            task = _execute_metric_with_timeout(
                metric_instance,
                test_case,
                metric_name,
            )
            tasks.append(task)
            metric_names.append(metric_name)
        
        # Execute all metrics in parallel
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # Collect scores
        for metric_name, result in zip(metric_names, results):
            if isinstance(result, Exception):
                logger.warning(
                    "metric_execution_exception",
                    metric=metric_name,
                    error=str(result),
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
        )
        
    except Exception as e:
        logger.error(
            "deepeval_evaluation_failed",
            error=str(e),
            error_type=type(e).__name__,
        )
        raise
    
    return scores

