from workflow_models.agent_models import DraftRequirements, AuditFindings, ScoreReport, RetrievedContext
import config
from typing import Dict
import structlog

logger = structlog.get_logger(__name__)

class Evaluator:
    async def evaluate(self, draft: DraftRequirements, findings: AuditFindings, user_prompt: str, context: RetrievedContext) -> ScoreReport:
        axes: Dict[str, float] = dict(getattr(findings, "component_scores", {}) or {})
        severity = float(getattr(findings, "llm_critique_severity", 0.0) or 0.0)
        
        # Apply severity penalty: reduce all axes by severity factor
        # severity 0.0 = no penalty (factor=1.0), severity 0.7 = 70% penalty (factor=0.3)
        penalty_factor = max(0.0, 1.0 - severity)
        penalized_axes = {k: v * penalty_factor for k, v in axes.items()}
        
        logger.info(
            "evaluator_severity_penalty_applied",
            severity=round(severity, 3),
            penalty_factor=round(penalty_factor, 3),
            raw_axes={k: round(v, 3) for k, v in axes.items()},
            penalized_axes={k: round(v, 3) for k, v in penalized_axes.items()},
        )

        settings = config.get_ai_requirements_settings()
        weights: Dict[str, float] = getattr(settings, "EVAL_WEIGHTS", {})

        # Weighted aggregation across available axes using penalized scores
        acc = 0.0
        total = 0.0
        for axis, w in weights.items():
            if w > 0 and axis in penalized_axes:
                acc += penalized_axes[axis] * w
                total += w
        final_score = acc / total if total > 0 else (sum(penalized_axes.values()) / len(penalized_axes) if penalized_axes else 0.0)

        # Round penalized components for report
        comps = {k: round(float(v), 3) for k, v in penalized_axes.items()}
        return ScoreReport(score=round(float(final_score), 3), component_scores=comps)


