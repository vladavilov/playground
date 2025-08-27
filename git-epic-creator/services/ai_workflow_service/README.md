AI Workflow Service — Agentic Requirements Generation (Design and Requirements)

Overview

The AI Workflow Service orchestrates an agentic, high‑precision pipeline that converts a user prompt into structured business and functional requirements. It augments context via a GraphRAG Retrieval microservice (separate service, out of scope here) backed by Neo4j and uses an ensemble‑of‑experts (EoE) strategy. The workflow performs self‑evaluation; if confidence (score) < 0.70, it generates targeted clarification questions so that user answers are expected to raise the score ≥ 0.70. The service publishes stepwise workflow updates to the UI via Redis Pub/Sub on the same channel convention used by project_management_service → ui_service.

Non‑Goals

- GraphRAG Retrieval service implementation (assumed to exist and expose HTTP APIs)
- UI and SSE bridge implementation (already handled by ui_service)
- Long‑term memory/store beyond the project context handled by GraphRAG

Key Requirements (ingestible by coding agent)

Interfaces

- Public API (FastAPI):
  - POST /workflow/requirements
    - Request: { project_id: UUID, prompt: string, options?: object }
    - Response: RequirementsBundle (see Schemas)
    - Behavior: kicks off the agentic pipeline synchronously, but publishes progress updates over Redis during execution.
  - POST /workflow/answers
    - Request: { project_id: UUID, prompt_id: UUID, answers: QuestionAnswer[] }
    - Response: RequirementsBundle; re‑runs the pipeline with the provided answers to attempt to reach score ≥ 0.70.
  - GET /health
    - Returns liveness/readiness including connectivity to Redis and GraphRAG.

- Outbound calls:
  - GraphRAG Retrieval Service (HTTP):
    - GET /search/entities?q=..., GET /search/docs?q=..., POST /search/composite { strategies: [...] }
    - Must support multiple strategy queries per request to minimize latency.

- Redis Pub/Sub:
  - Channel: ui:ai_workflow_progress
  - Publisher messages follow the WorkflowProgressMessage schema.

Message Schemas

- WorkflowProgressMessage:
  - message_type: "ai_workflow_progress"
  - project_id: string (UUID)
  - status: string (e.g., "analyzing_prompt", "retrieving_context", "drafting_requirements", "evaluating", "needs_clarification", "completed", "error")
  - score?: number in [0,1]
  - chain_of_thought?: string (expanded reasoning text as configured)
  - timestamp, message_id — provided by unified message base

Data Models (local Pydantic)

- RequirementsBundle
  - prompt_id: UUID
  - project_id: UUID
  - business_requirements: Requirement[]
  - functional_requirements: Requirement[]
  - assumptions: string[]
  - risks: string[]
  - score: float (0..1)
  - clarification_questions?: ClarificationQuestion[] (present if score < 0.70)

- Requirement
  - id: string
  - title: string
  - description: string
  - rationale?: string
  - acceptance_criteria: string[]
  - priority: string (e.g., Must/Should/Could/Won’t)

  (StepTrace removed — no step‑level traces are returned in the bundle)

- ClarificationQuestion
  - id: string
  - text: string
  - options?: string[] (optional multiple choice to speed up user response)
  - expected_impact: string (short reason how it will raise the score)

- QuestionAnswer
  - id: string (matches ClarificationQuestion.id)
  - answer: string

Agentic Pipeline (expanded, requirements‑focused)

0) Session Init
   - Normalize input, create prompt_id, resolve project context keys.
   - Publish status: analyzing_prompt.

1) Prompt Understanding and Decomposition (PromptAnalyst)
   - Intent classification: feature, enhancement, bugfix, compliance, reporting, etc.
   - Decompose into atomic requirement intents and constraints (NER + pattern libs).
   - Extract domain entities, KPIs, SLAs, regulatory markers.
   - Output: DecompositionGraph. Publish status: analyzing_prompt, with chain_of_thought.

2) GraphRAG Context Retrieval (ContextRetriever)
   - Proxy design: This service does not connect to Neo4j directly; it prepares strategy inputs and calls the external GraphRAG service.
   - Proxy workflow:
     1) Build RetrievalPlan from DecompositionGraph: entity seeds, relation hints, schema tags, query expansions.
     2) Construct payload with multiple strategies (entity_neighborhood, relation_paths, hybrid_semantic, schema_guided, ranking weights for trust/recency/centrality).
     3) POST /v1/retrieve {project_id, plan, strategies[]} to GraphRAG service.
     4) Apply resilience: httpx AsyncClient, exponential backoff (tenacity), per‑strategy fallbacks, partial aggregation on timeout.
     5) Normalize response into ContextPack with provenance and citations. Publish status: retrieving_context.

3) Requirement Synthesis (RequirementEngineer) — iterative agentic loop
   - Approach: iterative refinement with reflection, guided by prompt + ContextPack + prior iteration output.
   - Orchestration: LangGraph state machine with nodes [synthesize → audit → revise] and checkpointing; optional human‑in‑the‑loop via interrupts.
   - Iteration i:
     1) Synthesize: produce BR/FR and ACs grounded in ContextPack with citations.
     2) Audit (ConsistencyAuditor): detect conflicts, gaps, duplicates, non‑testable ACs, compliance issues.
     3) Revise: apply audit diffs; if blocking issues remain, propose targeted clarifications.
   - Stop when: score ≥ threshold, or max_iters reached, or no material diffs.
   - Libraries: langgraph for iteration/checkpointing; prompts enforce citation and Given/When/Then ACs. Publish status: drafting_requirements.

4) Consistency, Constraints, and Compliance (ConsistencyAuditor)
   - Checks: contradiction detection, duplicate/overlap clustering, constraint coverage, AC testability (Given/When/Then presence), NFRs mapping, regulatory mapping.
   - Methods: rule‑based validators + LLM critique prompts with citations back to ContextPack.
   - Links: requirement ↔ constraints ↔ entities; surface missing links as gaps.
   - Output: ValidatedRequirements + risks + assumptions. Publish status update to Redis (WorkflowProgressMessage) with {project_id, status: "evaluating", score?}.

5) Traceability Enrichment
   - Build bidirectional trace: requirement ↔ evidence (graph ids, doc ids), requirement ↔ constraint, FR ↔ ACs; capture source spans.
   - Generate citation map for each requirement pointing to ContextPack items.
   - Produce machine‑readable trace tables to support downstream tooling (e.g., Git epics/stories).
   - Output: EnrichedRequirements. Publish status: evaluating.

6) Evaluation and Scoring (Evaluator)
   - Compute metrics: precision/faithfulness, grounding, completeness, feasibility.
   - Aggregate to s ∈ [0,1] (see rubric below). Publish status: evaluating with score.

7a) If s ≥ 0.70 → Finalize
   - Return RequirementsBundle, publish status: completed with score.

7b) If s < 0.70 → Clarification Loop (QuestionStrategist)
   - Identify weakest rubric axes and missing evidence/constraints.
   - Generate targeted clarification_questions with expected_impact descriptions.
   - Publish status: needs_clarification.
   - On POST /workflow/answers: augment DecompositionGraph/ContextPack, repeat steps 2–6 until s ≥ 0.70 or question budget exhausted.

Evaluation Rubric (configurable) and Technical Implementation

- Weights (defaults):
  - precision_weight: 0.40 — faithfulness vs. retrieved evidence
  - grounding_weight: 0.30 — explicit citations and support
  - completeness_weight: 0.20 — covers intents, constraints, ACs, and traceability
  - feasibility_weight: 0.10 — realistic, testable, implementable

- Implementation (Ragas‑based evaluators):
  - Faithfulness: `ragas.metrics.Faithfulness`
  - Grounding: `ragas.metrics.ResponseGroundedness`
  - Completeness: `ragas.metrics.AspectCritic` with definition "All user intents and constraints are fully addressed with grounded requirements and testable acceptance criteria; return 1 else 0" and/or `RubricsScore` tuned to requirements completeness.
  - Optional: ContextRecall/ContextPrecision for retrieval sanity checks.

Composite scoring API (example):

```python
from ragas.metrics import Faithfulness, ResponseGroundedness, AspectCritic
from ragas.llms import LangchainLLMWrapper

class CompletenessScorer:
    def __init__(self, llm, weights=None):
        self.evaluator_llm = LangchainLLMWrapper(llm)
        self.faithfulness = Faithfulness(llm=self.evaluator_llm)
        self.groundedness = ResponseGroundedness(llm=self.evaluator_llm)
        self.completeness = AspectCritic(
            name="Requirements Completeness",
            llm=self.evaluator_llm,
            definition=(
                "Return 1 if the requirements comprehensively cover user intents, "
                "constraints, and include testable acceptance criteria grounded in context; else 0."
            ),
        )
        self.weights = weights or dict(precision=0.40, grounding=0.30, completeness=0.20, feasibility=0.10)

    async def score(self, user_input, response, retrieved_contexts, feasibility_hint=None):
        sample = SingleTurnSample(user_input=user_input, response=response, retrieved_contexts=retrieved_contexts)
        p = await self.faithfulness.single_turn_ascore(sample)
        g = await self.groundedness.single_turn_ascore(sample)
        c = await self.completeness.single_turn_ascore(sample)
        f = 1.0 if (feasibility_hint or self._heuristic_feasibility(response)) else 0.5
        w = self.weights
        return p*w["precision"] + g*w["grounding"] + c*w["completeness"] + f*w["feasibility"], dict(p=p,g=g,c=c,f=f)

    def _heuristic_feasibility(self, response):
        # simple heuristic: presence of measurable ACs, constraints, and no contradictions
        return all(k in response.lower() for k in ["given", "when", "then"]) 
```

Redis Pub/Sub Integration

- Channel: "ui:ai_workflow_progress" (new channel for AI Workflow updates).
- Publisher logic mirrors existing publisher but uses `WorkflowProgressMessage` with fields { project_id, status, score? }.

Service Components

- FastAPI app with dependency wiring via shared FastAPIFactory
- Orchestrator: coordinates experts, scoring, and clarification loop
- Experts:
  - PromptAnalyst, ContextRetriever, RequirementEngineer, ConsistencyAuditor, Evaluator, QuestionStrategist
  - Implemented as pure classes with explicit inputs/outputs for testability
- Clients:
  - GraphRAGClient (HTTP)
  - RedisPublisher (async)

Configuration

- Env vars (examples):
  - GRAPH_RAG_BASE_URL, GRAPH_RAG_TIMEOUT_SEC
  - EVAL_WEIGHTS: JSON or separate vars (PRECISION_WEIGHT, GROUNDING_WEIGHT, ...)
  - CLARIFICATION_SCORE_TARGET=0.70
  - REDIS_URL (via shared config)

Testing Strategy

- Unit tests per expert with golden prompts and fixtures
- Orchestrator tests cover:
  - happy path (score ≥ 0.70)
  - low score → clarification loop → improved score
  - GraphRAG partial failures with fallbacks
- Contract tests for Redis publishing (mocked) and GraphRAG client (HTTP mock)

Implementation Notes

- Keep expert classes stateless and deterministic where possible.
- Use clear typed dataclasses/Pydantic models for all boundaries.
- Log and publish non‑PII status only.
- Respect existing channel naming and message schema used by ui_service/project_management_service.

Prompt Decomposition Details (NER + pattern libs)

- Goals: break the user prompt into minimal, testable requirement intents and explicit constraints to guide retrieval and synthesis.
- Techniques:
  - NER stack: spaCy pipelines (en_core_web_trf) with domain adapters, plus custom EntityRuler for product/module names, systems, SLAs, KPIs, regulatory refs (e.g., GDPR articles), actors/roles.
  - Pattern libraries: curated regex/patterns for dates, amounts, KPI forms, SLO/SLA phrasing, non-functional requirements (performance, security, availability), CRUD verbs, reporting/aggregation patterns.
  - Intent classifier: lightweight text classifier (e.g., scikit‑learn/SVC or small transformer) to tag intents: feature/enhancement/bugfix/compliance/reporting/migration.
  - Constraint extractor: dependency phrases (must/shall/should/unless), scope bounds, acceptance hints; normalize to canonical constraint objects.
- Output structure (DecompositionGraph):
  - intents: [{id, type, verb, object, modifiers}]
  - constraints: [{id, type, expression, normalized_value, source_span}]
  - domain_entities: [{name, type, aliases}]
  - metrics_and_slas: [{metric, target, unit, window}]
  - compliance_markers: [{regulation, clause, topic}]
  - links: edges among intents ↔ constraints ↔ entities

Implementation notes:
- Use spaCy Pipeline with EntityRuler before the transformer NER to boost domain precision.
- Maintain pattern packs as versioned YAML with tests; load at startup.
- All extracted items carry source spans for traceability.

Example Progress Messages

{
  "message_type": "ai_workflow_progress",
  "project_id": "<uuid>",
  "status": "evaluating",
  "score": 0.62,
  "timestamp": "<iso>"
}

Acceptance Criteria

- Exposes the two endpoints with schemas above
- Publishes Redis updates compatible with UI SSE listener on ui:ai_workflow_progress
- Produces RequirementsBundle with scoring (no step traces)
- Enters clarification loop when score < 0.70 and returns questions that target rubric deficiencies
- Configuration‑driven thresholds/weights


