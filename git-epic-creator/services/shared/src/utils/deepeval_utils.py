"""Shared DeepEval utility module with telemetry permanently disabled and optimized metric execution.

This module provides a centralized way to execute DeepEval metrics with:
- Telemetry/analytics permanently disabled at import time (PostHog, LiteLLM logging)
- Shared LiteLLMModel instance to reduce duplicate calls
- Timeout support for metric execution
- Proper error handling that doesn't fail on telemetry issues

Telemetry is disabled globally when this module is imported for maximum performance.
"""

import os
from typing import Dict, Optional, Any
from functools import lru_cache
import asyncio
import structlog
from deepeval.models import LiteLLMModel
import posthog

logger = structlog.get_logger(__name__)

# ============================================================================
# TELEMETRY DISABLED AT MODULE IMPORT (HARDCODED FOR PERFORMANCE)
# ============================================================================
# Disable PostHog (used by DeepEval for analytics)
os.environ["POSTHOG_DISABLED"] = "1"
os.environ["POSTHOG_API_KEY"] = ""

# Disable LiteLLM logging and telemetry
os.environ["LITELLM_DISABLE_LOGGING"] = "True"
os.environ["LITELLM_LOGGING_LEVEL"] = "ERROR"
os.environ["LITELLM_DROP_PARAMS"] = "True"
os.environ["LITELLM_TELEMETRY"] = "False"

# Disable DeepEval telemetry
os.environ["DEEPEVAL_TELEMETRY_OPT_OUT"] = "YES"

posthog.disabled = True
posthog.api_key = None

logger.debug("telemetry_permanently_disabled", module="deepeval_utils")

@lru_cache(maxsize=8)
def get_litellm_model(
    model: str,
    api_key: str,
    api_base: str,
    api_version: str,
) -> Any:
    """Get or create a cached LiteLLMModel instance.
    
    Uses LRU cache to avoid creating multiple instances with same configuration,
    which reduces duplicate model initialization and API overhead.
    
    Telemetry is already disabled at module import time, so no context manager needed.
    
    Args:
        model: Model identifier (e.g., "azure/gpt-4o")
        api_key: API key for authentication
        api_base: Base URL for API endpoint
        api_version: API version string
    
    Returns:
        LiteLLMModel instance configured for DeepEval metrics
    
    Raises:
        ImportError: If deepeval is not installed
    """
    model_instance = LiteLLMModel(
        model=model,
        api_key=api_key,
        api_base=api_base,
        api_version=api_version,
    )
    
    logger.debug(
        "litellm_model_created",
        model=model,
        api_base=api_base,
        api_version=api_version,
        cached=False,
    )
        
    return model_instance


async def execute_metric_with_timeout(
    metric: Any,
    test_case: Any,
    timeout_seconds: float = 30.0,
    metric_name: str = "unknown",
) -> Optional[float]:
    """Execute a DeepEval metric with timeout protection.
    
    Telemetry is already disabled at module import time for maximum performance.
    
    Args:
        metric: DeepEval metric instance (e.g., FaithfulnessMetric)
        test_case: LLMTestCase instance with test data
        timeout_seconds: Maximum execution time in seconds
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
        timeout=timeout_seconds,
    )
    
    try:
        # Run metric measurement in executor to allow timeout
        loop = asyncio.get_event_loop()
        await asyncio.wait_for(
            loop.run_in_executor(None, metric.measure, test_case),
            timeout=timeout_seconds,
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
            timeout=timeout_seconds,
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
    model_config: Dict[str, str],
    timeout_seconds: float = 30.0,
) -> Dict[str, float]:
    """Evaluate test case with multiple DeepEval metrics in parallel.
    
    This is the main entry point for running DeepEval evaluations with:
    - Telemetry disabled
    - Shared model instance
    - Parallel execution where possible
    - Timeout protection
    - Graceful error handling
    
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
                        ...
                    }
                }
            }
        model_config: Dict with LiteLLM model configuration
            Example:
            {
                "model": "azure/gpt-4o",
                "api_key": "...",
                "api_base": "https://...",
                "api_version": "2024-02-15-preview"
            }
        timeout_seconds: Maximum execution time per metric in seconds
    
    Returns:
        Dict mapping metric names to scores (0.0-1.0)
        Failed/timed-out metrics will have score of 0.0
    
    Example:
        from deepeval.metrics import FaithfulnessMetric, GEval
        from deepeval.test_case import LLMTestCase, LLMTestCaseParams
        
        test_case = LLMTestCase(
            input="question",
            actual_output="answer",
            context=["context1", "context2"]
        )
        
        metrics_config = {
            "faithfulness": {
                "class": FaithfulnessMetric,
                "kwargs": {}
            },
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
        
        model_config = {
            "model": "azure/gpt-4o",
            "api_key": config.OAI_KEY,
            "api_base": config.OAI_BASE_URL,
            "api_version": config.OAI_API_VERSION
        }
        
        scores = await evaluate_with_metrics(test_case, metrics_config, model_config)
        # Returns: {"faithfulness": 0.85, "groundedness": 0.92}
    """
    logger.info(
        "deepeval_evaluation_starting",
        metrics_count=len(metrics_config),
        timeout=timeout_seconds,
    )
    
    scores: Dict[str, float] = {}
    
    try:
        # Get shared model instance (cached)
        model = get_litellm_model(
            model=model_config["model"],
            api_key=model_config["api_key"],
            api_base=model_config["api_base"],
            api_version=model_config["api_version"],
        )
        
        logger.debug(
            "litellm_model_configured",
            model=model_config["model"],
            api_base=model_config["api_base"],
        )
        
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
            task = execute_metric_with_timeout(
                metric_instance,
                test_case,
                timeout_seconds,
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

