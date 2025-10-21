from typing import Dict, Any
from utils.deepeval_utils import StrictGEval
from deepeval.metrics import FaithfulnessMetric, AnswerRelevancyMetric
from deepeval.test_case import LLMTestCaseParams

def groundedness_rubric() -> Dict[str, Any]:
    return {
        "name": "Citations",
        "criteria": "Does the actual output cite or clearly derive from the provided context?",
        "evaluation_steps": [
            "Identify all claims, facts, or statements in the actual output",
            "For each claim, verify if it can be traced back to or derived from the provided context",
            "Check if the actual output includes explicit citations or references to the context",
            "Evaluate whether the actual output introduces information not present in the context",
            "Assign a score based on how well the actual output is grounded in the provided context",
        ],
        "evaluation_params": ["ACTUAL_OUTPUT", "CONTEXT"],
        "strict_mode": False,
        "verbose_mode": False,
    }


def completeness_rubric() -> Dict[str, Any]:
    return {
        "name": "Completeness",
        "criteria": "All user intents and constraints are fully addressed with grounded requirements and testable acceptance criteria.",
        "evaluation_steps": [
            "Extract all user intents, requirements, and constraints from the input",
            "Identify each requirement, acceptance criterion, and assumption in the actual output",
            "Verify that each user intent from the input is addressed in the actual output",
            "Check if acceptance criteria are testable and follow Given/When/Then format",
            "Assess if any user intent or constraint is missing or inadequately addressed",
            "Assign a score based on completeness of coverage and quality of testable criteria",
        ],
        "evaluation_params": ["INPUT", "ACTUAL_OUTPUT"],
        "strict_mode": False,
        "verbose_mode": False,
    }


def build_metrics_config(has_real_context: bool):

    grounded = groundedness_rubric()
    complete = completeness_rubric()

    metrics_config = {
        "faithfulness": {
            "class": FaithfulnessMetric,
            "kwargs": {},
        },
        "groundedness": {
            "class": StrictGEval,
            "kwargs": {
                "name": grounded["name"],
                "criteria": grounded["criteria"],
                "evaluation_steps": grounded["evaluation_steps"],
                "evaluation_params": [
                    LLMTestCaseParams.ACTUAL_OUTPUT,
                    LLMTestCaseParams.CONTEXT,
                ],
                "strict_mode": grounded["strict_mode"],
                "verbose_mode": grounded["verbose_mode"],
            },
        },
        "response_relevancy": {
            "class": AnswerRelevancyMetric,
            "kwargs": {},
        },
        "completeness": {
            "class": StrictGEval,
            "kwargs": {
                "name": complete["name"],
                "criteria": complete["criteria"],
                "evaluation_steps": complete["evaluation_steps"],
                "evaluation_params": [
                    LLMTestCaseParams.INPUT,
                    LLMTestCaseParams.ACTUAL_OUTPUT,
                ],
                "strict_mode": complete["strict_mode"],
                "verbose_mode": complete["verbose_mode"],
            },
        },
    }

    if not has_real_context:
        metrics_config.pop("groundedness", None)

    return metrics_config


