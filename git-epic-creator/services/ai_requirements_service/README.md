AI Requirements Service — Agentic Requirements Generation (Design and Requirements)

Overview

The AI Requirements Service orchestrates an agentic, high‑precision pipeline that converts a user prompt into structured business and functional requirements. It augments context via a GraphRAG Retrieval microservice (separate service, out of scope here) backed by Neo4j and uses an ensemble‑of‑experts (EoE) strategy. The workflow performs self‑evaluation; if confidence (score) < 0.70, it generates targeted clarification questions so that user answers are expected to raise the score ≥ 0.70. The service publishes stepwise workflow updates to the UI via Redis Pub/Sub on the same channel convention used by project_management_service → ui_service.

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
    - POST /retrieve { query, top_k }
    - Supports multiple retrieval strategies per request with weighting to minimize latency and improve recall/precision.

Message Schemas

- WorkflowProgressMessage (user-visible step updates):
  - message_type: "ai_requirements_progress"
  - project_id: string (UUID)
  - iteration?: integer (>=1)
  - status: string (e.g., "analyzing_prompt", "retrieving_context", "drafting_requirements", "evaluating", "needs_clarification", "completed", "error")
  - score?: number in [0,1]
  - thought_summary: string (concise summary of progress/insight; no raw chain-of-thought)
  - details_md?: string (markdown-formatted step details)
  - message_id: string (UUID)
  - timestamp: string (ISO8601)

Redis Pub/Sub

- Channel: "ui:ai_requirements_progress"
- Publisher sends only `WorkflowProgressMessage` for user-visible updates.

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

- ClarificationQuestion
  - id: string
  - text: string
  - options?: string[] (optional multiple choice to speed up user response)
  - expected_impact: string (short reason how it will raise the score)
  - axis?: string ("precision" | "grounding" | "response_relevancy" | "completeness")
  - priority?: integer (1 = highest)
  - expected_score_gain?: number (0..1)
  - targets?: string[] (ids of affected requirements/intents)

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
   - Output: DecompositionGraph. Publish WorkflowProgressMessage (status: analyzing_prompt, stage: "prompt_decomposition").

2) GraphRAG Context Retrieval (ContextRetriever)
   - Proxy design: This service does not connect to Neo4j directly; it formats the user query and intents, and calls the external Retrieval service.
   - Proxy workflow:
     1) Merge prompt and intents into markdown:
        """
        ### Question
        {prompt}

        ### Intents
        - intent_1
        - intent_2
        """
     2) POST /retrieve { query: <merged_markdown>, top_k }
     3) Apply resilience: httpx AsyncClient, exponential backoff (tenacity).
     4) Map response to RetrievedContext { context_answer, key_facts, citations } and publish WorkflowProgressMessage (status: retrieving_context).
   
   **Citation Quality Monitoring:**
   - Logs warnings when citations have "unknown" document names (indicates upstream filtering or missing data)
   - Logs warnings for empty/missing chunk_ids
   - Logs warnings for legacy string citation format (no document name or span)
   - Deduplicates citations by chunk_id while preserving order

3) Requirement Synthesis (RequirementEngineer) — iterative agentic loop
   - Approach: iterative refinement with reflection, guided by prompt + RetrievedContext + prior iteration output.
   - Orchestration: LangGraph state machine with nodes [synthesize → audit → revise] and checkpointing; optional human‑in‑the‑loop via interrupts.
   - Iteration i:
     1) Synthesize: produce BR/FR and ACs grounded in RetrievedContext with citations.
     2) Audit (ConsistencyAuditor): detect conflicts, gaps, duplicates, non‑testable ACs, compliance issues.
     3) Revise: apply audit diffs; if blocking issues remain, propose targeted clarifications.
   - Stop when: score ≥ threshold, or max_iters reached, or no material diffs.
   - Libraries: langgraph for iteration/checkpointing; prompts enforce citation and Given/When/Then ACs. Publish WorkflowProgressMessage on each iteration (status: drafting_requirements, stage: "draft_requirements", iteration).

4) Consistency, Constraints, and Compliance (ConsistencyAuditor)
   - Checks: contradiction detection, duplicate/overlap clustering, constraint coverage, AC testability (Given/When/Then presence), NFRs mapping, regulatory mapping.
   - Methods: rule‑based validators + LLM critique prompts with citations back to RetrievedContext.
   - Links: requirement ↔ constraints ↔ entities; surface missing links as gaps.
   - Output: ValidatedRequirements + risks + assumptions. Publish WorkflowProgressMessage (status: evaluating).

5) Traceability Enrichment
   - Build bidirectional trace: requirement ↔ evidence (graph ids, doc ids), requirement ↔ constraint, FR ↔ ACs; capture source spans.
   - Generate citation map for each requirement pointing to RetrievedContext.citations.
   - Produce machine‑readable trace tables to support downstream tooling (e.g., Git epics/stories).
   - Output: EnrichedRequirements. Publish WorkflowProgressMessage (status: evaluating, stage: "traceability").

6) Evaluation and Scoring (Evaluator)
   - Compute metrics: precision/faithfulness, grounding, response_relevancy, completeness.
   - Apply severity penalty from ConsistencyAuditor to all component scores:
     * severity 0.0-0.3 (minor issues) → minimal penalty (multiplier 0.7-1.0)
     * severity 0.4-0.6 (moderate issues) → medium penalty (multiplier 0.4-0.6)
     * severity 0.7-1.0 (critical issues) → severe penalty (multiplier 0.0-0.3)
   - Aggregate penalized scores to s ∈ [0,1] (see rubric below). Publish WorkflowProgressMessage (status: evaluating, score) summarizing rubric axes.

7a) If s ≥ 0.70 → Finalize
   - Return RequirementsBundle. Publish WorkflowProgressMessage (status: completed, score).

7b) If s < 0.70 → Clarification Loop (QuestionStrategist)
   - Identify weakest rubric axes and missing evidence/constraints.
   - Generate targeted clarification_questions with expected_impact descriptions.
   - Publish WorkflowProgressMessage (status: needs_clarification, score).
   - On POST /workflow/answers: augment DecompositionGraph/RetrievedContext, repeat steps 2–6 until s ≥ 0.70 or question budget exhausted.

Evaluation Rubric (configurable) and Technical Implementation

- Weights (defaults):
  - precision_weight: 0.30 — faithfulness vs. retrieved evidence
  - grounding_weight: 0.30 — explicit citations and support
  - response_relevancy_weight: 0.20 — topical relevance to the prompt/project
  - completeness_weight: 0.20 — covers intents, constraints, ACs, and traceability

- Implementation (DeepEval‑based evaluators):
  - Faithfulness: `deepeval.metrics.FaithfulnessMetric`
  - Grounding: `deepeval.metrics.GEval` (criteria enforces citation/derivation from provided context; params: ACTUAL_OUTPUT + CONTEXT)
  - ResponseRelevancy: `deepeval.metrics.AnswerRelevancyMetric`
  - Completeness: `deepeval.metrics.GEval` (criteria ensures intents/constraints addressed with testable ACs; params: INPUT + ACTUAL_OUTPUT)
  - Optional: Retrieval sanity checks via `deepeval.metrics.ContextualRelevancyMetric` or `ContextualPrecisionMetric`.

Implementation note: Scoring uses DeepEval metrics with configurable weights via `EVAL_WEIGHTS`. Only axes present in weights contribute to the final score. **All component scores are penalized by the consistency audit severity** (penalty_factor = 1.0 - severity) before weighted aggregation, ensuring quality issues directly impact the final score and trigger clarification loops.


Input/Output Contracts

- Input (POST /workflow/requirements):
  - project_id: UUID
  - prompt: string (free-form)

- Output (score ≥ 0.70):
  - RequirementsBundle plus markdown_text: string (requirements in markdown with headings/ACs)

- Output (score < 0.70):
  - RequirementsBundle with clarification_questions populated

Examples

```json
{
  "prompt_id": "...",
  "project_id": "...",
  "business_requirements": [...],
  "functional_requirements": [...],
  "assumptions": [...],
  "risks": [...],
  "score": 0.78,
  "markdown_text": "## Business Requirements\n...\n## Functional Requirements\n..."
}
```

```json
{
  "prompt_id": "...",
  "project_id": "...",
  "business_requirements": [...],
  "functional_requirements": [...],
  "assumptions": [...],
  "risks": [...],
  "score": 0.62,
  "clarification_questions": [
    {"id": "Q1", "text": "Which reporting time window is required?", "axis": "completeness", "expected_impact": "Enables ACs for report latency", "priority": 1, "expected_score_gain": 0.08}
  ]
}
```

Service Components

- FastAPI app with dependency wiring via shared FastAPIFactory
- Orchestrator: coordinates experts, scoring, and clarification loop
- Experts:
  - PromptAnalyst, ContextRetriever (HTTP), RequirementEngineer, ConsistencyAuditor, Evaluator, QuestionStrategist
  - Implemented as pure classes with explicit inputs/outputs for testability
- Clients:
  - RedisPublisher (async)

Prompt Repository

- Centralized prompts live in `orchestrator/prompts`.
- Use `build_chat_prompt(spec)` with one of the exported specs:
  - `PROMPT_ANALYST`
  - `REQUIREMENTS_ENGINEER`
  - `CONSISTENCY_AUDITOR`
  - `QUESTION_STRATEGIST`
- Example usage in an expert:

```python
from orchestrator.prompts import build_chat_prompt, REQUIREMENTS_ENGINEER

tmpl = build_chat_prompt(REQUIREMENTS_ENGINEER)
chain = tmpl | llm.with_structured_output(OutputModel)
out = await chain.ainvoke({"intents": intents, "contexts": contexts, "findings": findings_payload})
```

Evaluation Rubrics

- Shared rubric definitions and metric configuration live in `orchestrator/prompts/rubrics.py`.
- Use `build_metrics_config(has_real_context: bool)` to obtain the metrics config for DeepEval.
- This centralizes LLM instructions like `criteria` and `evaluation_steps` for groundedness and completeness.

Configuration

- Env vars (examples):
  - **Azure OpenAI Configuration** (required):
    - `OAI_BASE_URL`: Azure OpenAI endpoint (e.g., `https://<resource>.openai.azure.com/`)
    - `OAI_MODEL`: Azure OpenAI deployment name (e.g., `gpt-4o`, `gpt-4.1`)
    - `OAI_KEY`: Azure OpenAI API key
    - `OAI_API_VERSION`: Azure OpenAI API version (e.g., `2024-02-15-preview`)
    - Note: Service uses `AzureChatOpenAI` connector requiring `deployment_name` parameter
  - **Timeout Configuration** (for long-running workflows):
    - `HTTP_READ_TIMEOUT`: Read timeout from shared HTTPClientSettings (default: 180s for UI, overridable per service)
    - `WORKFLOW_TIMEOUT_SEC`: Maximum workflow execution time before graceful abort (default: 150s)
    - Note: WORKFLOW_TIMEOUT_SEC should be < HTTP_READ_TIMEOUT to allow graceful response before client timeout
  - GRAPH_RAG_BASE_URL
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

Prompt Decomposition Details (brief)

- Extract minimal, testable requirement intents and explicit constraints to guide retrieval and synthesis.
- Techniques: spaCy with EntityRuler, curated pattern libraries, lightweight intent classifier, and a constraint extractor.
- Output: DecompositionGraph with intents, constraints, entities, metrics/SLAs, compliance markers, links among them, and source spans for traceability.
- For deeper NER/pattern details, see the Design Details document.

Example Progress Messages

{
  "message_type": "ai_requirements_progress",
  "project_id": "<uuid>",
  "status": "evaluating",
  "score": 0.62,
  "thought_summary": "Scoring draft against retrieved context; gaps in completeness.",
  "message_id": "<uuid>",
  "timestamp": "<iso-8601>"
}

Acceptance Criteria

- Exposes the two endpoints with schemas above
- Publishes Redis updates compatible with UI SSE listener on ui:ai_requirements_progress
- Produces RequirementsBundle with scoring (no step traces)
- Enters clarification loop when score < 0.70 and returns questions that target rubric deficiencies
- Configuration‑driven thresholds/weights


