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
    }


def require_authentication(request: Request) -> None:
    config = get_config()
    required_key = config["OAI_KEY"]
    if not required_key:
        return
    auth_header = request.headers.get("authorization")
    api_key_header = request.headers.get("api-key") or request.headers.get("x-api-key")
    expected = f"Bearer {required_key}"
    if auth_header == expected:
        return
    if api_key_header == required_key:
        return
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
    user_prompt: str = ""
    try:
        # Build a tiny prompt from the last user message if available
        if isinstance(messages, list) and messages:
            for msg in reversed(messages):
                if isinstance(msg, dict) and msg.get("role") == "user":
                    user_prompt = str(msg.get("content", ""))
                    break
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
            # GraphRAG: detect extraction / continuation / loop prompts and emit delimiter-formatted tuples
            try:
                last_msg = messages[-1] if messages else {}
                last_content = str(last_msg.get("content", "")) if isinstance(last_msg, dict) else ""
            except Exception:
                last_content = ""

            # Handle gleaning continuation prompt: return no additional records
            if (
                "MANY entities and relationships were missed" in last_content
                and "Add them below using the same format" in last_content
            ):
                generated = ""
            # Handle loop-check prompt: answer with a single letter to stop gleaning
            elif (
                "Answer Y if there are still entities or relationships" in last_content
                and "Please answer with a single letter Y or N" in last_content
            ):
                generated = "N"
            else:
                # Detect GraphRAG extraction prompt structure
                combined_content = f"{system_content}\n{user_content}".lower()
                is_graphrag_prompt = (
                    ("entity_types:" in combined_content)
                    and ("text:" in combined_content)
                    and (
                        "format each entity as (\"entity\"" in combined_content
                        or "when finished, output" in combined_content
                    )
                )
                if is_graphrag_prompt:
                    # Parse delimiters with safe fallbacks
                    full_text = f"{system_content}\n{user_content}"
                    def _extract(pattern: str, text: str) -> Optional[str]:
                        m = re.search(pattern, text, flags=re.IGNORECASE | re.DOTALL)
                        return m.group(1) if m else None

                    tuple_delim = _extract(r"Format each entity as \(\"entity\"(.*?)<entity_name>", full_text)
                    if not tuple_delim:
                        # Also try examples
                        tuple_delim = _extract(r"\(\"entity\"(.*?)<entity_name>", full_text)
                    record_delim = _extract(r"Use \*\*(.+?)\*\* as the list delimiter", full_text)
                    completion_delim = _extract(r"When finished, output\s+(\S+)", full_text)

                    # Defaults from GraphRAG
                    td = tuple_delim or "<|>"
                    rd = record_delim or "##"
                    cd = completion_delim or "<|COMPLETE|>"

                    # Parse entity types list
                    entity_types_line = _extract(r"Entity_types:\s*([^\n\r]+)", full_text) or "ORGANIZATION,PERSON,GEO,EVENT"
                    parsed_types = [t.strip().upper() for t in entity_types_line.split(",") if t.strip()]
                    if not parsed_types:
                        parsed_types = ["ORGANIZATION", "PERSON", "GEO", "EVENT"]

                    # Build Smallpdf-grounded records
                    def ent(name: str, etype: str, desc: str) -> str:
                        return f"(\"entity\"{td}{name.upper()}{td}{etype.upper()}{td}{desc})"

                    def rel(src: str, tgt: str, desc: str, weight: float | int = 2) -> str:
                        return f"(\"relationship\"{td}{src.upper()}{td}{tgt.upper()}{td}{desc}{td}{weight})"

                    # Choose safe types from allowed set
                    ORG = "ORGANIZATION"
                    EVT = "EVENT"

                    entity_records = [
                        ent("Smallpdf", ORG, "Smallpdf is a platform for managing and transforming digital documents."),
                        ent("Storage", ORG, "Optional feature that stores processed files when enabled."),
                        ent("Mobile App", ORG, "Smallpdf Mobile App that syncs files to the online portal."),
                        ent("G Suite App", ORG, "Integration that can be enabled for an organization."),
                        ent("E-signatures", EVT, "Request and collect electronic signatures on documents."),
                        ent("Enhance", EVT, "One-click enhancements for uploaded documents."),
                        ent("Convert", EVT, "Action to convert files to other formats."),
                        ent("Compress", EVT, "Action to compress files to smaller size."),
                        ent("Modify", EVT, "Action to modify documents using available tools."),
                    ]

                    relationship_records = [
                        rel("Smallpdf", "Storage", "Smallpdf stores processed files when Storage is enabled.", 2),
                        rel("Mobile App", "Smallpdf", "The Mobile App syncs files to the Smallpdf online portal.", 2),
                        rel("Smallpdf", "E-signatures", "Smallpdf supports requesting electronic signatures.", 2),
                        rel("Smallpdf", "G Suite App", "Smallpdf can enable the G Suite App for organizations.", 2),
                        rel("Smallpdf", "Enhance", "Smallpdf provides one-click enhancements.", 1),
                        rel("Smallpdf", "Convert", "Smallpdf offers right-click convert actions.", 1),
                        rel("Smallpdf", "Compress", "Smallpdf offers right-click compress actions.", 1),
                        rel("Smallpdf", "Modify", "Smallpdf offers right-click modify actions.", 1),
                    ]

                    # Join with record delimiter on its own line, then finish with completion delimiter
                    generated = ("\n" + rd + "\n").join([*entity_records, *relationship_records]) + f"\n{rd}\n{cd}"

                # Community report (graph-based) → strict JSON
                elif (
                    "write a comprehensive report of a community" in combined_content
                    and '"findings"' in combined_content
                ):
                    generated = json.dumps({
                        "title": "Smallpdf Workspace and Tools",
                        "summary": (
                            "Smallpdf provides a unified workspace for digital documents, enabling upload, organization, and sharing. "
                            "When Storage is enabled, processed files are retained and accessible across devices, including sync from the Mobile App to the web portal. "
                            "Right‑click actions support Convert/Compress/Modify, and collaboration features include e‑signatures, large file sending, and a G Suite App."
                        ),
                        "rating": 3.5,
                        "rating_explanation": "Moderate impact: broad feature set with low inherent risk in the provided text.",
                        "findings": [
                            {
                                "summary": "Unified document management",
                                "explanation": "Users can upload, organize, and share documents in one place, reducing fragmentation across tools."
                            },
                            {
                                "summary": "Optional persistent Storage",
                                "explanation": "Processed files are stored when Storage is enabled, improving continuity and retrieval."
                            },
                            {
                                "summary": "Cross‑device access and sync",
                                "explanation": "Files are accessible on computer, phone, or tablet; the Mobile App syncs to the online portal."
                            },
                            {
                                "summary": "Productivity actions and collaboration",
                                "explanation": "Right‑click actions accelerate file conversions and edits; collaboration includes e‑signatures and large file transfer."
                            }
                        ]
                    })

                # Community report (text-based) → JSON with date_range
                elif (
                    "date range" in combined_content
                    and "Return output as a well-formed JSON-formatted string" in combined_content
                    and "importance rating" in combined_content
                ):
                    generated = json.dumps({
                        "title": "Smallpdf Document Experience Overview",
                        "summary": (
                            "Smallpdf centralizes digital document tasks: upload, organize, share, and enhance. "
                            "Storage (optional) persists processed files. Cross‑device access with Mobile App sync extends availability. "
                            "Actions include Convert/Compress/Modify; collaboration features include e‑signatures, large file sending, and G Suite integration."
                        ),
                        "rating": 3.8,
                        "rating_explanation": "Important for productivity; the text highlights core capabilities without risk events.",
                        "findings": [
                            {"summary": "Central hub", "explanation": "Digital documents managed end‑to‑end in one place."},
                            {"summary": "Sync and access", "explanation": "Mobile App sync to portal; access on computer/phone/tablet."}
                        ],
                        "date_range": ["", ""]
                    })

                # Extract claims (extract_claims prompt) → tuple/record/completion delimiters (tight detection)
                elif (
                    "-target activity-" in combined_content.lower()
                    and "extract all entities that match the predefined entity specification" in combined_content
                    and "Format each claim as (" in combined_content
                    and "When finished, output" in combined_content
                ):
                    full_text = f"{system_content}\n{user_content}"
                    def _extract(pattern: str, text: str) -> Optional[str]:
                        m = re.search(pattern, text, flags=re.IGNORECASE | re.DOTALL)
                        return m.group(1) if m else None

                    record_delim = _extract(r"Use \*\*(.+?)\*\* as the list delimiter", full_text) or "##"
                    completion_delim = _extract(r"When finished, output\s+(\S+)", full_text) or "<|COMPLETE|>"
                    tuple_delim = "<|>"

                    def claim(subject: str, obj: str, ctype: str, status: str, start: str, end: str, desc: str, source: str) -> str:
                        return (
                            f"{subject.upper()}{tuple_delim}{obj.upper()}{tuple_delim}{ctype.upper()}"
                            f"{tuple_delim}{status.upper()}{tuple_delim}{start}{tuple_delim}{end}{tuple_delim}{desc}{tuple_delim}{source}"
                        )

                    c1 = claim(
                        "Smallpdf", "Storage", "Feature Availability", "TRUE",
                        "NONE", "NONE",
                        "Storage retains processed files when enabled.",
                        "\"When you enable the ‘Storage’ option, we’ll also store all processed files here.\"",
                    )
                    c2 = claim(
                        "Mobile App", "Smallpdf", "Synchronization", "TRUE",
                        "NONE", "NONE",
                        "Mobile App syncs files to the online portal.",
                        "\"We’ll also sync files from the Smallpdf Mobile App to our online portal\"",
                    )
                    generated = f"({c1})\n{record_delim}\n({c2})\n{completion_delim}"

                # Global search (map) → JSON points (explicit signature)
                elif (
                    "The response should be JSON formatted as follows" in combined_content
                    and '"points"' in combined_content
                    and "map" in combined_content.lower()
                ):
                    generated = json.dumps({
                        "points": [
                            {"description": "Smallpdf centralizes document upload, organization, and sharing.", "score": 85},
                            {"description": "Storage persists processed files when enabled, improving continuity.", "score": 70},
                        ]
                    })

                # Drift search (drift_search_system_prompt) → JSON with response/score/follow_up_queries
                elif (
                    "Format your response in JSON with the following keys" in combined_content
                    and "Additionally provide a score between 0 and 100" in combined_content
                    and "follow_up_queries" in combined_content
                ):
                    generated = json.dumps({
                        "response": (
                            "### Smallpdf capabilities\n\n"
                            "- Upload, organize, and share digital documents\n"
                            "- Optional Storage for processed files\n"
                            "- Cross‑device access with Mobile App sync\n"
                            "- Right‑click actions: Convert, Compress, Modify\n"
                            "- Collaboration: e‑signatures, large file sending, G Suite App\n"
                        ),
                        "score": 78,
                        "follow_up_queries": [
                            "What file types are supported for Convert and Modify?",
                            "Are Storage retention and encryption configurable?",
                            "What size limits apply to large file sending?",
                        ],
                    })

                # Drift reduce (drift_reduce_prompt) → markdown summary (explicit 'reports provided')
                elif (
                    "---Role---" in system_content
                    and "responding to questions about data in the reports provided" in system_content
                    and "---Data Reports---" in combined_content
                ):
                    generated = (
                        "### Summary\n\n"
                        "Smallpdf centralizes document work: upload, organize, and share. When Storage is enabled, processed files are retained and accessible across devices, with Mobile App sync to the portal. Right‑click actions (Convert, Compress, Modify) and collaboration features (e‑signatures, large file sending, G Suite App) streamline workflows.\n\n"
                        "### Notes\n\n"
                        "- Capabilities emphasize convenience and collaboration.\n"
                        "- Storage behavior may warrant retention/security clarifications."
                    )

                # Basic search (basic_search_system_prompt) → markdown summary (explicit 'tables provided')
                elif (
                    "---Role---" in system_content
                    and "responding to questions about data in the tables provided" in system_content
                    and "---Data tables---" in combined_content
                    and "Style the response in markdown" in combined_content
                ):
                    generated = (
                        "### Overview\n\n"
                        "Smallpdf offers a unified document experience: upload, organize, and share in one place. When Storage is enabled, processed files are retained. Users can access files across devices with Mobile App sync. Right‑click actions (Convert, Compress, Modify) and collaboration (e‑signatures, large file sending, G Suite App) are supported."
                    )

                # Question generation → '- ' bullets
                elif "generate a bulleted list of" in combined_content and "Use - marks as bullet points" in combined_content:
                    generated = (
                        "- What retention and security apply when Storage is enabled?\n"
                        "- Which file formats are supported for Convert/Compress/Modify?\n"
                        "- How does Mobile App sync frequency and conflict resolution work?\n"
                        "- What are limits for large file sending (size, expiry)?\n"
                        "- Can admins manage the G Suite App organization‑wide settings?"
                    )

                # Summarize descriptions (summarize_descriptions prompt) → concise synthesized paragraph (tight detection)
                elif (
                    "You are a helpful assistant responsible for generating a comprehensive summary of the data" in combined_content
                    and "Description List:" in combined_content
                ):
                    generated = (
                        "Smallpdf centralizes digital document tasks, enabling users to upload, organize, share, and enhance files in one place. "
                        "With optional Storage, processed files remain available for later use. Files are accessible across devices, and the Mobile App syncs to the online portal. "
                        "Right‑click actions (Convert, Compress, Modify) and collaboration features (e‑signatures, large file sending, G Suite App) streamline common workflows."
                    )

            if not generated:
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


# Azure OpenAI-compatible endpoints
@app.get("/openai/deployments", dependencies=[Depends(require_authentication)])
async def list_deployments() -> Dict[str, Any]:
    cfg = get_config()
    return {
        "data": [
            {"id": cfg["OAI_MODEL"], "model": cfg["OAI_MODEL"], "object": "deployment"},
            {"id": cfg["OAI_EMBED_MODEL"], "model": cfg["OAI_EMBED_MODEL"], "object": "deployment"},
        ]
    }


@app.post("/openai/deployments/{deployment}/chat/completions", dependencies=[Depends(require_authentication)])
@app.post("/v1/openai/deployments/{deployment}/chat/completions", dependencies=[Depends(require_authentication)])
async def azure_chat_completions(deployment: str, body: Dict[str, Any], request: Request) -> Dict[str, Any]:
    # Ensure model is present for downstream logic
    if "model" not in body:
        body = {**body, "model": deployment}
    return await chat_completions(body)


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
        # Normalize vector dimensions to align with requested model where applicable
        target_dim = _target_dim_for_model(str(model))
        if target_dim is not None:
            vectors = [_fit_dim(vec, target_dim) for vec in vectors]
        data = [
            {"object": "embedding", "index": idx, "embedding": vec}
            for idx, vec in enumerate(vectors)
        ]
    except Exception as exc:
        logger.warning("embedding_failed_fallback", error=str(exc))
        vector = [0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008]
        data = [
            {"object": "embedding", "index": idx, "embedding": vector}
            for idx, _ in enumerate(items)
        ]

    return {
        "object": "list",
        "data": data,
        "model": config["OAI_EMBED_MODEL"],
        "usage": {"prompt_tokens": 0, "total_tokens": 0},
    }


@app.post("/openai/deployments/{deployment}/embeddings", dependencies=[Depends(require_authentication)])
@app.post("/v1/openai/deployments/{deployment}/embeddings", dependencies=[Depends(require_authentication)])
async def azure_embeddings(deployment: str, body: Dict[str, Any]) -> Dict[str, Any]:
    if "model" not in body:
        body = {**body, "model": deployment}
    return await embeddings(body)


def _load_embedder():
    """Lazy-load sentence-transformers model from local directory.

    Returns the model or None if unavailable.
    """
    try:
        from sentence_transformers import SentenceTransformer  # type: ignore
    except Exception:
        return None

    # Hardcoded long-context embedding model directory
    model_dir = os.path.join(os.getcwd(), "models", "embeddings", "jina-embeddings-v2-base-en")
    # Load from the locally downloaded snapshot; suppress attention warning via model_kw
    model = SentenceTransformer(
        model_dir,
        model_kwargs={"attn_implementation": "eager"},
    )
    # Truncate long inputs to speed up and avoid OOM
    # Hardcode 10k token window for embeddings
    model.max_seq_length = 10000
    return model

def _embed_texts_locally(texts: List[str]) -> List[List[float]]:
    embedder = _load_embedder()
    if embedder is None:
        raise RuntimeError("Embedder not available")
    # Return plain python lists to avoid numpy dependency overhead
    embeddings = embedder.encode(
        texts,
        show_progress_bar=False,
        convert_to_numpy=False,
        normalize_embeddings=False,
    )
    return [[float(x) for x in vec] for vec in embeddings]


def _target_dim_for_model(model_name: str) -> Optional[int]:
    name = (model_name or "").lower()
    # Align with common OpenAI embedding dims
    if "text-embedding-3-small" in name:
        return 1536
    if "text-embedding-3-large" in name:
        return 3072
    return None


def _fit_dim(vec: List[float], target_dim: int) -> List[float]:
    if target_dim <= 0:
        return vec
    length = len(vec)
    if length == target_dim:
        return vec
    if length > target_dim:
        return vec[:target_dim]
    # pad with zeros to target length
    return vec + [0.0] * (target_dim - length)


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


