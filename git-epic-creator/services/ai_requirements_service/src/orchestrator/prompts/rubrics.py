from typing import Dict, Any
from utils.deepeval_utils import StrictGEval
from deepeval.metrics import FaithfulnessMetric, AnswerRelevancyMetric
from deepeval.test_case import LLMTestCaseParams

def groundedness_rubric() -> Dict[str, Any]:
    return {
        "name": "Citations",
        "criteria": (
            "Does the actual output cite or clearly derive from the provided context?\n\n"
            "Scoring scale:\n"
            "- 0.00: No connection to context, entirely fabricated content\n"
            "- 0.15: ~15% of claims grounded; mostly unsupported or speculative\n"
            "- 0.30: ~30% Minimal grounding, most content appears fabricated or assumed\n"
            "- 0.45-0.60: Some requirements grounded, but significant claims lack context support\n"
            "- 0.60-0.75: Major part of requirements is grounded, but some claims lack context support, or some assumptions are made\n"
            "- 0.90-1.00: All requirements and details directly traceable to context, explicit citations present\n\n"
        ),
        "evaluation_steps": [
            "Score based on percentage of claims traceable to context."
            "Identify all claims, facts, or statements in the actual output",
            "For each claim, verify if it can be traced back to or derived from the provided context",
            "Check if the actual output includes explicit citations or references to the context",
            "Evaluate whether the actual output introduces information not present in the context",
            "Count the percentage of claims that are grounded: (grounded_claims/ total_claims) = base_score",
            "Apply deductions: -0.10 for each critical unsupported claim (e.g., requirements, acceptance criteria)",
            "Assign final score between 0.00 and 1.00 based on base_score and deductions",
        ],
        "evaluation_params": ["ACTUAL_OUTPUT", "CONTEXT"],
        "strict_mode": False,
        "verbose_mode": False,
    }


def completeness_rubric() -> Dict[str, Any]:
    return {
        "name": "Completeness",
        "criteria": (
            "All user intents and constraints are fully addressed with grounded requirements and testable acceptance criteria.\n\n"
            "Scoring scale:\n"
            "- 0.00: Critical intents missing, ACs not testable, major coverage gaps\n"
            "- 0.15: Many intents unaddressed; ACs largely non-testable or absent\n"
            "- 0.30: Some intents covered, but significant gaps; few G/W/T ACs\n"
            "- 0.45-0.60: Majority of intents covered; several ACs lack G/W/T or are shallow\n"
            "- 0.60-0.75: Most intents covered with testable ACs; minor gaps remain\n"
            "- 0.90-1.00: All intents addressed; all ACs fully testable (Given/When/Then), comprehensive edge/error coverage\n"
        ),
        "evaluation_steps": [
            "Extract and count all user intents from INPUT (intents_total)",
            "Identify, in ACTUAL_OUTPUT, which intents are addressed with corresponding requirements (intents_addressed)",
            "For each addressed intent, verify presence of at least one Acceptance Criteria in Given/When/Then (G/W/T) form; count those intents (intents_testable)",
            "Optional quality bonus: count intents that include multiple scenarios (normal, edge, error) with concrete data; (intents_rich)",
            "Handle edge case: if intents_total == 0 then score = 1.0",
            "Compute coverage = intents_addressed / intents_total",
            "Compute testability = intents_testable / intents_total",
            "Compute bonus = min(0.05, intents_rich / intents_total * 0.10)  (cap at +0.05)",
            "Compute deductions: ded_missing = 0.10 * (intents_total - intents_addressed)",
            "Compute ded_untestable = 0.05 * max(0, intents_addressed - intents_testable)",
            "Compute ded_ambiguity = 0.05 if ACTUAL_OUTPUT contains >3 vague terms (e.g., 'user-friendly', 'fast', 'should be able to'), else 0.00",
            "Compute raw_score = 0.5 * coverage + 0.5 * testability + bonus - (ded_missing + ded_untestable + ded_ambiguity)",
            "Clamp final score to [0.00, 1.00] and round to two decimals",
            "Provide a brief justification citing counts (covered/testable/missing, bonuses, deductions). For deductions, list several reasons and include only the top contributors (those that deducted the most).",
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


