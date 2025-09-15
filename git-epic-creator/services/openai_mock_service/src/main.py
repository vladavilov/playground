import os
import json
import re
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from fastapi import Depends, FastAPI, HTTPException, Request
from fastapi.responses import ORJSONResponse
from fastapi.exceptions import RequestValidationError
import structlog
import uvicorn
from collections import deque
from starlette.middleware.base import BaseHTTPMiddleware


# Ensure Hugging Face cache dir uses HF_HOME to avoid deprecation warning
if os.getenv("HF_HOME") is None:
    os.environ.setdefault("HF_HOME", os.getenv("TRANSFORMERS_CACHE", "/models/hf-cache"))


def get_environment_variable(name: str, default: Optional[str] = None) -> Optional[str]:
    value = os.getenv(name)
    return value if value is not None else default


def get_config() -> Dict[str, Optional[str]]:
    return {
        "API_PORT": get_environment_variable("API_PORT", "8000"),
        "OAI_KEY": get_environment_variable("OAI_KEY"),
        "OAI_MODEL": get_environment_variable("OAI_MODEL", "gpt-4.1"),
        "OAI_EMBED_MODEL": get_environment_variable(
            "OAI_EMBED_MODEL", "text-embedding-3-large"
        ),
        "EMBEDDINGS_SIZE": get_environment_variable("EMBEDDINGS_SIZE", "1536"),
    }


def require_authentication(request: Request) -> None:
    config = get_config()
    required_key = config["OAI_KEY"]
    if not required_key:
        return
    auth_header = request.headers.get("authorization")
    expected = f"Bearer {required_key}"
    if auth_header != expected:
        raise HTTPException(status_code=401, detail="Unauthorized")


def configure_logging() -> None:
    """Minimal structlog setup compatible with other services."""
    structlog.configure(
        processors=[
            structlog.processors.add_log_level,
            structlog.processors.TimeStamper(fmt="iso"),
            structlog.processors.JSONRenderer(),
        ],
    )


configure_logging()
logger = structlog.get_logger(__name__)


app = FastAPI(default_response_class=ORJSONResponse)




SPY_MAX_RECORDS = int(os.getenv("SPY_MAX_RECORDS", "1000"))
_spy_requests = deque(maxlen=SPY_MAX_RECORDS)


class SpyMiddleware(BaseHTTPMiddleware):
    async def dispatch(self, request: Request, call_next):
        # Avoid spying on spy endpoints themselves to prevent recursion
        path = request.url.path or ""
        if not path.startswith("/spy"):
            try:
                raw_body = await request.body()
                # Restore body for downstream consumers
                async def receive():
                    return {"type": "http.request", "body": raw_body, "more_body": False}
                request._receive = receive  # type: ignore[attr-defined]

                headers = {k.lower(): v for k, v in request.headers.items()}
                body_text: str
                try:
                    body_text = raw_body.decode("utf-8")
                except Exception:
                    body_text = "<non-text-body>"
                _spy_requests.append({
                    "ts": datetime.now(timezone.utc).isoformat(),
                    "method": request.method,
                    "path": path,
                    "headers": headers,
                    "body": body_text,
                })
            except Exception as exc:
                logger.warning("spy_record_failed", error=str(exc))
        response = await call_next(request)
        return response


app.add_middleware(SpyMiddleware)


@app.post("/spy/clear")
async def spy_clear() -> Dict[str, str]:
    _spy_requests.clear()
    return {"status": "cleared"}


@app.get("/spy/requests")
async def spy_requests(howMany: int = 50) -> Dict[str, Any]:
    n = max(0, min(int(howMany), SPY_MAX_RECORDS))
    # Return most recent first
    items = list(_spy_requests)[-n:][::-1]
    return {"items": items, "count": len(items)}

@app.exception_handler(RequestValidationError)
async def request_validation_exception_handler(
    request: Request, exc: RequestValidationError
) -> ORJSONResponse:
    return ORJSONResponse(status_code=400, content={"detail": "Bad Request"})


@app.get("/health")
async def health() -> Dict[str, str]:
    logger.info("health_check")
    return {"status": "ok"}


@app.get("/models", dependencies=[Depends(require_authentication)])
@app.get("/v1/models", dependencies=[Depends(require_authentication)])
async def list_models() -> Dict[str, Any]:
    config = get_config()
    models_list = [config["OAI_MODEL"], config["OAI_EMBED_MODEL"]]
    logger.info(
        "list_models",
        oai_model=config["OAI_MODEL"],
        oai_embed_model=config["OAI_EMBED_MODEL"],
        response_models=models_list,
    )
    return {
        "object": "list",
        "data": [
            {"id": config["OAI_MODEL"], "object": "model", "owned_by": "mock"},
            {
                "id": config["OAI_EMBED_MODEL"],
                "object": "model",
                "owned_by": "mock",
            },
        ],
    }


@app.post("/chat/completions", dependencies=[Depends(require_authentication)])
@app.post("/v1/chat/completions", dependencies=[Depends(require_authentication)])
async def chat_completions(body: Dict[str, Any]) -> Dict[str, Any]:
    config = get_config()
    model = body.get("model")
    messages = body.get("messages")
    if not model or messages is None:
        raise HTTPException(status_code=400, detail="Bad Request")
    # Log incoming messages for observability
    try:
        logger.info("oai_chat_request", messages=messages)
    except Exception:
        logger.info("oai_chat_request_unloggable")
    user_prompt: str = ""
    try:
        # Build a tiny prompt from the last user message if available
        if isinstance(messages, list) and messages:
            for msg in reversed(messages):
                if isinstance(msg, dict) and msg.get("role") == "user":
                    user_prompt = str(msg.get("content", ""))
                    break

        # DRIFT-search deterministic routing for E2E tests
        # Recognize simple keyword cues and return fixed, sensible JSON/text
        # Assume test user question is: "what are the main components of the bridge?"
        all_text = "\n".join([str(m.get("content", "")) for m in messages if isinstance(m, dict)])
        lower_all = all_text.lower()
        if "hyde" in lower_all or "hypothetical answer paragraph" in lower_all:
            generated = (
                "Hypothetical paragraph about bridge components: deck, supports, arches, cables, and foundations."
            )
            logger.info("drift_mock_hyde")
            return {
                "id": "cmpl-mock-hyde",
                "object": "chat.completion",
                "created": 1700000001,
                "model": config["OAI_MODEL"],
                "choices": [
                    {"index": 0, "message": {"role": "assistant", "content": generated}, "finish_reason": "stop"}
                ],
                "usage": {"prompt_tokens": 3, "completion_tokens": 20, "total_tokens": 23},
            }

        if "you are drift-search primer" in lower_all:
            primer_json = {
                "initial_answer": "Bridges consist of deck, supports, and load-bearing structures.",
                "followups": [
                    {"question": "Clarify deck materials and structural role.", "target_communities": [1]},
                    {"question": "Explain arch mechanics in load distribution.", "target_communities": [2]},
                ],
                "rationale": "Primer synthesized from community summaries and sampled chunks.",
            }
            generated = json.dumps(primer_json)
            logger.info("drift_mock_primer")
            return {
                "id": "cmpl-mock-primer",
                "object": "chat.completion",
                "created": 1700000002,
                "model": config["OAI_MODEL"],
                "choices": [
                    {"index": 0, "message": {"role": "assistant", "content": generated}, "finish_reason": "stop"}
                ],
                "usage": {"prompt_tokens": 3, "completion_tokens": 30, "total_tokens": 33},
            }

        if "you are drift-search local executor" in lower_all:
            # Branch 1: Clarify deck materials and structural role -> answer + 1 new follow-up, should_continue = true
            if "clarify deck materials and structural role" in lower_all:
                local_json = {
                    "answer": (
                        "Bridge decks are commonly built from reinforced concrete, steel orthotropic panels, or composite systems. "
                        "The deck distributes vehicular loads to primary load-bearing elements (girders, arches, or cables) and provides a riding surface."
                    ),
                    "citations": [
                        {"chunk_id": 0, "span": "deck materials overview"},
                        {"chunk_id": 1, "span": "load path from deck to supports"},
                    ],
                    "new_followups": [
                        {
                            "question": "List common deck materials and how they influence load distribution.",
                            "target_communities": [1],
                        }
                    ],
                    "confidence": 0.9,
                    "should_continue": True,
                }
                generated = json.dumps(local_json)
                logger.info("drift_mock_local_deck_materials")
                return {
                    "id": "cmpl-mock-local",
                    "object": "chat.completion",
                    "created": 1700000003,
                    "model": config["OAI_MODEL"],
                    "choices": [
                        {"index": 0, "message": {"role": "assistant", "content": generated}, "finish_reason": "stop"}
                    ],
                    "usage": {"prompt_tokens": 3, "completion_tokens": 28, "total_tokens": 31},
                }

            # Branch 2: Answer the generated follow-up; should_continue = false
            if "list common deck materials and how they influence load distribution" in lower_all:
                local_json = {
                    "answer": (
                        "Reinforced concrete decks spread loads through slab action to supporting girders; steel orthotropic decks channel loads via stiffened plates; "
                        "composite steel–concrete decks combine slab and girder action, improving stiffness and distributing loads more evenly."
                    ),
                    "citations": [
                        {"chunk_id": 2, "span": "concrete slab load distribution"},
                        {"chunk_id": 3, "span": "orthotropic deck behavior"},
                    ],
                    "new_followups": [],
                    "confidence": 0.92,
                    "should_continue": False,
                }
                generated = json.dumps(local_json)
                logger.info("drift_mock_local_deck_materials_followup")
                return {
                    "id": "cmpl-mock-local",
                    "object": "chat.completion",
                    "created": 1700000003,
                    "model": config["OAI_MODEL"],
                    "choices": [
                        {"index": 0, "message": {"role": "assistant", "content": generated}, "finish_reason": "stop"}
                    ],
                    "usage": {"prompt_tokens": 3, "completion_tokens": 28, "total_tokens": 31},
                }

            # Branch 3: Explain arch mechanics in load distribution; should_continue = false
            if "explain arch mechanics in load distribution" in lower_all:
                local_json = {
                    "answer": (
                        "Arches carry deck loads primarily in compression, transferring forces as thrust to abutments. "
                        "Load paths curve along the arch rib, minimizing bending and channeling forces into supports."
                    ),
                    "citations": [
                        {"chunk_id": 4, "span": "arch compression and thrust"},
                        {"chunk_id": 5, "span": "load path along arch rib"},
                    ],
                    "new_followups": [],
                    "confidence": 0.9,
                    "should_continue": False,
                }
                generated = json.dumps(local_json)
                logger.info("drift_mock_local_arch_mechanics")
                return {
                    "id": "cmpl-mock-local",
                    "object": "chat.completion",
                    "created": 1700000003,
                    "model": config["OAI_MODEL"],
                    "choices": [
                        {"index": 0, "message": {"role": "assistant", "content": generated}, "finish_reason": "stop"}
                    ],
                    "usage": {"prompt_tokens": 3, "completion_tokens": 28, "total_tokens": 31},
                }

            # Default Local Executor behavior
            local_json = {
                "answer": "The deck carries traffic; arches transfer loads to supports.",
                "citations": [{"chunk_id": 0, "span": "deck overview"}, {"chunk_id": 1, "span": "arch mechanics"}],
                "new_followups": [],
                "confidence": 0.9,
                "should_continue": False,
            }
            generated = json.dumps(local_json)
            logger.info("drift_mock_local")
            return {
                "id": "cmpl-mock-local",
                "object": "chat.completion",
                "created": 1700000003,
                "model": config["OAI_MODEL"],
                "choices": [
                    {"index": 0, "message": {"role": "assistant", "content": generated}, "finish_reason": "stop"}
                ],
                "usage": {"prompt_tokens": 3, "completion_tokens": 28, "total_tokens": 31},
            }

        if "you are drift-search aggregator" in lower_all:
            agg_json = {
                "final_answer": "Bridges comprise the deck, supports, and load-bearing structures such as arches or cables.",
                "key_facts": [
                    {"fact": "Deck carries traffic and distributes loads.", "citations": [0]},
                    {"fact": "Arches channel forces into supports.", "citations": [1]},
                ],
                "residual_uncertainty": "Specific materials and design vary by bridge type.",
            }
            generated = json.dumps(agg_json)
            logger.info("drift_mock_aggregator")
            return {
                "id": "cmpl-mock-agg",
                "object": "chat.completion",
                "created": 1700000004,
                "model": config["OAI_MODEL"],
                "choices": [
                    {"index": 0, "message": {"role": "assistant", "content": generated}, "finish_reason": "stop"}
                ],
                "usage": {"prompt_tokens": 3, "completion_tokens": 35, "total_tokens": 38},
            }
        # First, try to satisfy integration-test patterns from ai_workflow_service
        if isinstance(messages, list):
            system_content = "".join(
                str(m.get("content", ""))
                for m in messages
                if isinstance(m, dict) and m.get("role") == "system"
            )
            user_content = "".join(
                str(m.get("content", "")) for m in messages if isinstance(m, dict)
            )
            generated: Optional[str] = None
            auditor_markers = (
                "Return ONLY JSON with: {severity: number in [0,1], suggestions: string[]}",
                "senior requirements QA reviewer",
                "Critique the requirements",
            )

            # Analyst: match only PromptAnalyst system prompt semantics
            if (
                ("senior requirements analyst" in system_content.lower())
                and ("respond only with json object" in system_content.lower())
                and ('"intents"' in system_content)
            ):
                generated = json.dumps({
                    "intents": [
                        "Upload files",
                        "Enable optional Storage for processed files",
                        "Access files across devices",
                        "Enhance documents in one click",
                        "Collaborate and request e-signatures",
                    ]
                })
            # Auditor: severity/suggestions
            elif any(marker in system_content for marker in auditor_markers):
                generated = json.dumps({
                    "severity": 0.0,
                    "suggestions": [
                        "Clarify capabilities included in 'Enhance documents in one click'.",
                        "Specify device support and sync latency for 'Access files anytime, anywhere'.",
                        "Define size limits and retention for 'send large files'.",
                        "State security controls when 'Storage' is enabled (e.g., encryption, retention).",
                    ],
                })
            # Strategist: axis scores / questions
            elif ("axis_scores" in user_content) or ("questions" in user_content):
                payload = [
                    {
                        "id": "Q1",
                        "text": "What exact enhancements does 'Enhance in one click' perform?",
                        "axis": "completeness",
                        "priority": 1,
                        "expected_score_gain": 0.15,
                        "targets": [],
                    },
                    {
                        "id": "Q2",
                        "text": "What are the size limits, quotas, and expiry for 'send large files'?",
                        "axis": "specificity",
                        "priority": 1,
                        "expected_score_gain": 0.12,
                        "targets": [],
                    },
                    {
                        "id": "Q3",
                        "text": "What encryption and retention apply when 'Storage' is enabled?",
                        "axis": "risk",
                        "priority": 1,
                        "expected_score_gain": 0.1,
                        "targets": [],
                    },
                ]
                generated = json.dumps({"questions": payload})
            # Auditor: plain dict when 'requirements' key present
            elif ("'requirements':" in user_content) or ('\"requirements\":' in user_content):
                generated = json.dumps({
                    "severity": 0.0,
                    "suggestions": [
                        "Clarify capabilities included in 'Enhance documents in one click'.",
                        "Specify device support and sync latency for 'Access files anytime, anywhere'.",
                        "Define size limits and retention for 'send large files'.",
                        "State security controls when 'Storage' is enabled (e.g., encryption, retention).",
                    ],
                })
            # Engineer: schema/draft recognition or system mentions requirements engineer
            elif (
                # Strong signals in system prompt
                "requirements engineer" in system_content.lower()
                or "business_requirements" in system_content
                or "functional_requirements" in system_content
                or (
                    "respond only with json object" in system_content.lower()
                    and "business_requirements" in system_content
                )
                # Or explicit schema cues in user content
                or "'schema': {'business_requirements'" in user_content
                or '"business_requirements":' in user_content
            ):
                payload = {
                    "business_requirements": [
                        {
                            "id": "BR-1",
                            "title": "Upload and organize digital documents",
                            "description": "Users can freely upload and organize digital documents within the Smallpdf experience.",
                            "acceptance_criteria": [
                                "Given a signed-in user, When they upload a PDF, Then the file appears in their library.",
                                "Given files in the library, When the user organizes them into folders, Then the structure persists.",
                            ],
                            "priority": "Must",
                        },
                        {
                            "id": "BR-2",
                            "title": "Optional Storage for processed files",
                            "description": "When 'Storage' is enabled, all processed files are stored and accessible later.",
                            "acceptance_criteria": [
                                "Given Storage is enabled, When a file is processed, Then it is saved to the user's stored files.",
                                "Given Storage is disabled, When a file is processed, Then it is not persisted beyond download.",
                            ],
                            "priority": "Must",
                        },
                        {
                            "id": "BR-3",
                            "title": "Access files across devices",
                            "description": "Users can access files on computer, phone, or tablet, with Mobile App sync to the portal.",
                            "acceptance_criteria": [
                                "Given stored files, When a user signs in on mobile, Then the same files are visible.",
                                "Given the Mobile App, When a file is uploaded there, Then it syncs and appears on the web portal.",
                            ],
                            "priority": "Should",
                        },
                    ],
                    "functional_requirements": [
                        {
                            "id": "FR-1",
                            "title": "Right-click context actions",
                            "description": "Provide right-click actions to convert, compress, or modify files.",
                            "acceptance_criteria": [
                                "Given a file in the library, When user right-clicks it, Then options include Convert, Compress, and Modify.",
                            ],
                            "priority": "Must",
                        },
                        {
                            "id": "FR-2",
                            "title": "Collaboration features",
                            "description": "Support requesting e-signatures, sending large files, and enabling the Smallpdf G Suite App.",
                            "acceptance_criteria": [
                                "Given a document, When user requests an e-signature, Then a signature workflow is initiated.",
                                "Given a document, When user selects 'Send large files', Then a shareable transfer link is created.",
                                "Given an admin, When they enable the Smallpdf G Suite App, Then the organization can use it.",
                            ],
                            "priority": "Should",
                        },
                        {
                            "id": "FR-3",
                            "title": "Enhance documents in one click",
                            "description": "Provide one-click enhancements.",
                            "acceptance_criteria": [
                                "Given an uploaded document, When 'Enhance' is clicked, Then the system applies predefined improvements.",
                            ],
                            "priority": "Could",
                        },
                    ],
                    "assumptions": [
                        "Users have network access on all devices.",
                        "Mobile App account is linked to the same user identity.",
                    ],
                    "risks": [
                        "Ambiguity around 'Enhance in one click' exact transformations.",
                        "Large-file sending limits and quotas may impact UX.",
                    ],
                }
                generated = json.dumps(payload)
            # Analyst: intents schema or instructions
            elif ('\"schema\": [\"string\"]' in user_content) or ("instructions" in user_content):
                generated = json.dumps([
                    "Upload files",
                    "Enable optional Storage for processed files",
                    "Access files across devices",
                    "Enhance documents in one click",
                    "Collaborate and request e-signatures",
                ])
            
            if not generated:
                # Fallback to existing graph JSON behavior
                max_tokens_raw: Optional[int] = None
                try:
                    if isinstance(body, dict) and body.get("max_tokens") is not None:
                        max_tokens_raw = int(body.get("max_tokens"))
                except Exception:
                    max_tokens_raw = None

                generated = _generate_text_locally(
                    user_prompt,
                    max_new_tokens=max_tokens_raw,
                )
        else:
            max_tokens_raw: Optional[int] = None
            try:
                if isinstance(body, dict) and body.get("max_tokens") is not None:
                    max_tokens_raw = int(body.get("max_tokens"))
            except Exception:
                max_tokens_raw = None

            generated = _generate_text_locally(
                user_prompt,
                max_new_tokens=max_tokens_raw,
            )
    except Exception as exc:  # Fallback to deterministic string
        logger.warning("chat_generation_failed", error=str(exc))
        generated = "{\n  \"nodes\": [],\n  \"relationships\": []\n}"

    logger.info("completion_output", text=generated)
    return {
        "id": "cmpl-mock-000",
        "object": "chat.completion",
        "created": 1700000000,
        "model": config["OAI_MODEL"],
        "choices": [
            {
                "index": 0,
                "message": {"role": "assistant", "content": generated},
                "finish_reason": "stop",
            }
        ],
        "usage": {"prompt_tokens": 3, "completion_tokens": 2, "total_tokens": 5},
    }


@app.post("/embeddings", dependencies=[Depends(require_authentication)])
@app.post("/v1/embeddings", dependencies=[Depends(require_authentication)])
async def embeddings(body: Dict[str, Any]) -> Dict[str, Any]:
    config = get_config()
    model = body.get("model")
    input_value = body.get("input")
    if not model or input_value is None:
        raise HTTPException(status_code=400, detail="Bad Request")

    items: List[str]
    if isinstance(input_value, list):
        items = input_value
    else:
        items = [input_value]
    try:
        vectors = _embed_texts_locally(items)
        data = [
            {"object": "embedding", "index": idx, "embedding": vec}
            for idx, vec in enumerate(vectors)
        ]
    except Exception as exc:
        logger.error("embedding_failed", error=str(exc))
        # Explicitly surface failure instead of returning small dummy vectors
        raise HTTPException(status_code=500, detail=f"Embedding model unavailable: {exc}")

    return {
        "object": "list",
        "data": data,
        "model": config["OAI_EMBED_MODEL"],
        "usage": {"prompt_tokens": 0, "total_tokens": 0},
    }


def _load_embedder():
    """Lazy-load a local Jina embeddings model from models/embeddings/jina-embeddings-v2-base-en.

    Tries Sentence-Transformers first. If that fails (e.g., local snapshot lacks
    sentence-transformers specific files), falls back to raw Transformers with
    mean pooling. Returns an object exposing a callable "encode(texts: List[str]) -> List[List[float]]".
    Raises RuntimeError if the model cannot be loaded locally.
    """
    model_dir = os.path.join(os.getcwd(), "models", "embeddings", "jina-embeddings-v2-base-en")
    if not os.path.isdir(model_dir):
        raise RuntimeError(f"Embedding model directory not found: {model_dir}")

    # Attempt Sentence-Transformers loading path
    try:
        from sentence_transformers import SentenceTransformer  # type: ignore

        st_model = SentenceTransformer(
            model_dir,
            model_kwargs={"attn_implementation": "eager"},
        )
        # Truncate long inputs to speed up and avoid OOM
        st_model.max_seq_length = 10000
        logger.info("embedder_loaded", backend="sentence-transformers", path=model_dir)
        return st_model
    except Exception as exc:
        logger.warning("embedder_st_load_failed", error=str(exc))

    # Fallback: load with raw Transformers and implement mean pooling
    try:
        import torch  # type: ignore
        from transformers import AutoModel, AutoTokenizer  # type: ignore

        class TransformersMeanPoolEmbedder:
            def __init__(self, directory: str) -> None:
                self.tokenizer = AutoTokenizer.from_pretrained(directory, local_files_only=True)
                # trust_remote_code is not required for standard architectures
                self.model = AutoModel.from_pretrained(directory, local_files_only=True)
                self.model.eval()

            def _mean_pool(self, last_hidden_state, attention_mask):
                # Mean pooling excluding padding tokens
                input_mask_expanded = attention_mask.unsqueeze(-1).expand(last_hidden_state.size()).float()
                sum_embeddings = torch.sum(last_hidden_state * input_mask_expanded, dim=1)
                sum_mask = torch.clamp(input_mask_expanded.sum(dim=1), min=1e-9)
                embeddings = sum_embeddings / sum_mask
                # L2 normalize
                embeddings = torch.nn.functional.normalize(embeddings, p=2, dim=1)
                return embeddings

            def encode(self, texts: List[str], show_progress_bar: bool = False, convert_to_numpy: bool = False, normalize_embeddings: bool = False):  # type: ignore[override]
                # normalize_embeddings handled inherently by L2 normalization above
                with torch.no_grad():
                    inputs = self.tokenizer(
                        texts,
                        padding=True,
                        truncation=True,
                        max_length=10000,
                        return_tensors="pt",
                    )
                    outputs = self.model(**inputs)
                    last_hidden_state = outputs.last_hidden_state
                    pooled = self._mean_pool(last_hidden_state, inputs["attention_mask"])  # [batch, hidden]
                    if convert_to_numpy:
                        return pooled.cpu().numpy()
                    return pooled.cpu().tolist()

        hf_embedder = TransformersMeanPoolEmbedder(model_dir)
        # Probe once to confirm it works
        _ = hf_embedder.encode(["test"], show_progress_bar=False, convert_to_numpy=False)
        logger.info("embedder_loaded", backend="transformers-mean-pool", path=model_dir)
        return hf_embedder
    except Exception as exc:
        logger.error("embedder_transformers_load_failed", error=str(exc))
        raise RuntimeError(f"Failed to load local embedding model from {model_dir}: {exc}")

def _embed_texts_locally(texts: List[str]) -> List[List[float]]:
    embedder = _load_embedder()
    # Enforce target dimension from env
    try:
        target_dim = int(get_config()["EMBEDDINGS_SIZE"] or "1536")
    except Exception:
        target_dim = 1536
    # Return plain python lists to avoid numpy dependency overhead
    embeddings = embedder.encode(  # type: ignore[attr-defined]
        texts,
        show_progress_bar=False,
        convert_to_numpy=False,
        normalize_embeddings=False,
    )
    # Ensure primitives
    vectors: List[List[float]] = []
    for vec in embeddings:
        lst = [float(x) for x in vec]
        # Pad or truncate to target_dim
        if len(lst) < target_dim:
            lst = lst + [0.0] * (target_dim - len(lst))
        elif len(lst) > target_dim:
            lst = lst[:target_dim]
        vectors.append(lst)
    return vectors


def _generate_text_locally(
    prompt: str,
    *,
    max_new_tokens: Optional[int] = None,
) -> str:
    # Return a deterministic JSON graph with placeholders filled
    del max_new_tokens  # unused in mock

    # Check if this is a community analysis prompt
    if prompt and prompt.strip().startswith("You are an expert analyst tasked with summarizing communities in a knowledge graph."):
        logger.info("community_analysis_prompt_detected")
        return "some community summary"

    # Try to extract a UUID from the prompt; fall back to a new UUID
    project_id: str
    try:
        uuid_match = re.search(
            r"\b[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[1-5][0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}\b",
            prompt or "",
        )
        project_id = uuid_match.group(0) if uuid_match else str(uuid.uuid4())
    except Exception:
        project_id = str(uuid.uuid4())

    now_iso = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    # Build Smallpdf-grounded graph: PROJECT -> DOCUMENT -> REQUIREMENTs
    doc_title = "smallpdf_intro.txt"
    requirements: List[Dict[str, Any]] = [
        {"id": "R1", "title": "Upload and organize digital documents", "description": "Users can freely upload and organize digital documents within the Smallpdf experience."},
        {"id": "R2", "title": "Optional Storage for processed files", "description": "When 'Storage' is enabled, processed files are stored and accessible later."},
        {"id": "R3", "title": "Access files across devices", "description": "Users can access files on computer, phone, or tablet, with Mobile App sync to the portal."},
        {"id": "R4", "title": "Right-click context actions", "description": "Provide right-click actions to convert, compress, or modify files."},
        {"id": "R5", "title": "Enhance documents in one click", "description": "Provide one-click enhancements."},
        {"id": "R6", "title": "Collaboration features", "description": "Support requesting e-signatures, sending large files, and enabling the Smallpdf G Suite App."},
    ]

    nodes: List[Dict[str, Any]] = [
        {
            "id": "P",
            "label": "PROJECT",
            "properties": {
                "project_id": project_id,
                "title": f"Project {project_id[:8]}",
                "creation_date": now_iso,
                "modification_date": now_iso,
            },
        },
        {
            "id": "D0",
            "label": "DOCUMENT",
            "properties": {
                "project_id": project_id,
                "title": "Smallpdf Introduction",
                "file_name": doc_title,
                "file_type": "txt",
                "content_type": "text/plain",
                "creation_date": now_iso,
                "modification_date": now_iso,
            },
        },
    ]
    # Append requirement nodes
    for r in requirements:
        nodes.append({
            "id": r["id"],
            "label": "REQUIREMENT",
            "properties": {
                "project_id": project_id,
                "title": r["title"],
                "description": r["description"],
                "creation_date": now_iso,
                "modification_date": now_iso,
            },
        })

    relationships: List[Dict[str, Any]] = [
        {
            "type": "PROJECT_HAS_DOCUMENT",
            "start_node_id": "P",
            "end_node_id": "D0",
            "properties": {"source": "mock"},
        },
    ]
    for r in requirements:
        relationships.append({
            "type": "DOCUMENT_HAS_REQUIREMENT",
            "start_node_id": "D0",
            "end_node_id": r["id"],
            "properties": {"source": "mock"},
        })

    # Extracted entities from the Smallpdf text
    entity_specs: List[Dict[str, str]] = [
        {"id": "E1", "title": "Smallpdf"},
        {"id": "E2", "title": "Storage"},
        {"id": "E3", "title": "Mobile App"},
        {"id": "E4", "title": "G Suite App"},
        {"id": "E5", "title": "e-signatures"},
        {"id": "E6", "title": "convert"},
        {"id": "E7", "title": "compress"},
        {"id": "E8", "title": "modify"},
        {"id": "E9", "title": "computer"},
        {"id": "E10", "title": "phone"},
        {"id": "E11", "title": "tablet"},
        {"id": "E12", "title": "Enhance"},
    ]
    for e in entity_specs:
        nodes.append({
            "id": e["id"],
            "label": "ENTITY",
            "properties": {
                "project_id": project_id,
                "title": e["title"],
                "creation_date": now_iso,
                "modification_date": now_iso,
            },
        })

    # Map requirements to mentioned entities
    req_to_entities: Dict[str, List[str]] = {
        "R1": ["E1"],  # Smallpdf platform
        "R2": ["E2"],  # Storage option
        "R3": ["E3", "E9", "E10", "E11"],  # Mobile App and devices
        "R4": ["E6", "E7", "E8"],  # convert/compress/modify
        "R5": ["E12"],  # Enhance action
        "R6": ["E5", "E4"],  # e-signatures, G Suite App
    }
    for rid, eids in req_to_entities.items():
        for eid in eids:
            relationships.append({
                "type": "REQUIREMENT_MENTIONS_ENTITY",
                "start_node_id": rid,
                "end_node_id": eid,
                "properties": {"source": "mock"},
            })

    payload: Dict[str, Any] = {
        "nodes": nodes,
        "relationships": relationships,
    }

    return json.dumps(payload, ensure_ascii=False, indent=2)


if __name__ == "__main__":
    cfg = get_config()
    uvicorn.run(app, host="0.0.0.0", port=int(cfg["API_PORT"]))


