from workflow_models.agent_models import DraftRequirements, AuditFindings, ScoreReport, RetrievedContext
import config
from typing import Dict

class Evaluator:
    async def evaluate(self, draft: DraftRequirements, findings: AuditFindings, user_prompt: str, context: RetrievedContext) -> ScoreReport:
        axes: Dict[str, float] = dict(getattr(findings, "component_scores", {}) or {})

        severity = getattr(findings, "llm_critique_severity", 0.0)
        settings = config.get_ai_requirements_settings()
        weights: Dict[str, float] = getattr(settings, "EVAL_WEIGHTS", {})

        # Weighted aggregation across available axes per weights
        acc = 0.0
        total = 0.0
        for axis, w in weights.items():
            if w > 0 and axis in axes:
                acc += axes[axis] * w
                total += w
        base_score = acc / total if total > 0 else (sum(axes.values()) / len(axes) if axes else 0.0)

        # Penalty multiplier: (1 - severity)
        final_score = max(0.0, base_score * (1.0 - severity))
        # Round components for report
        comps = {k: round(float(v), 3) for k, v in axes.items()}
        return ScoreReport(score=round(float(final_score), 3), component_scores=comps)


