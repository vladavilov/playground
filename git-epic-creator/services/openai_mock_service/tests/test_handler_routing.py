from fastapi.testclient import TestClient

from main import app
from configuration.common_config import get_app_settings


def _auth_headers() -> dict[str, str]:
    settings = get_app_settings()
    key = getattr(settings.llm, "OAI_KEY", None)
    if not key:
        # auth disabled
        return {}
    return {"Authorization": f"Bearer {key}"}


def _api_key_headers() -> dict[str, str]:
    settings = get_app_settings()
    key = getattr(settings.llm, "OAI_KEY", None)
    if not key:
        return {}
    return {"api-key": str(key)}


def test_ai_tasks_requirements_analyst_routes_to_single_handler() -> None:
    client = TestClient(app)
    payload = {
        "model": "gpt-4o-mini",
        "messages": [
            {
                "role": "system",
                "content": (
                    "Role: Senior Technical Architect\n"
                    "Objective: Extract 1-5 intents, entities, constraints EXACTLY AS STATED in requirements.\n"
                    "Output Contract (JSON only):\n"
                    '{"intents": ["intent1"], "entities": ["Entity1"], "constraints": ["constraint1"]}'
                ),
            },
            {"role": "user", "content": "Some requirements text"},
        ],
    }
    resp = client.post("/v1/chat/completions", json=payload, headers=_auth_headers())
    assert resp.status_code == 200, resp.text
    out = resp.json()
    assert out["model"] == "gpt-4o-mini"
    content = out["choices"][0]["message"]["content"]
    # Should be JSON string
    assert '"intents"' in content and '"entities"' in content and '"constraints"' in content


def test_ai_requirements_prompt_analyst_routes_to_single_handler() -> None:
    client = TestClient(app)
    payload = {
        "model": "gpt-4.1",
        "messages": [
            {
                "role": "system",
                "content": (
                    "Role: Senior Requirements Analyst (Wealth Management & Banking)\n"
                    "Objective: Extract 1-5 atomic, actionable intents from user prompts.\n"
                    "Output Contract (JSON only):\n"
                    '{"intents": ["intent1", "intent2"]}'
                ),
            },
            {"role": "user", "content": "User prompt text"},
        ],
    }
    resp = client.post("/v1/chat/completions", json=payload, headers=_auth_headers())
    assert resp.status_code == 200, resp.text
    out = resp.json()
    content = out["choices"][0]["message"]["content"]
    assert '"intents"' in content


def test_docling_vlm_routes_to_docling_handler() -> None:
    client = TestClient(app)
    payload = {
        "model": "llama-32-vision",
        "messages": [
            {"role": "system", "content": "You are a helpful assistant."},
            {
                "role": "user",
                "content": "Convert this page to docling format with detailed descriptions.",
            },
        ],
    }
    resp = client.post("/v1/chat/completions", json=payload, headers=_auth_headers())
    assert resp.status_code == 200, resp.text
    out = resp.json()
    content = out["choices"][0]["message"]["content"]
    assert "<docling>" in content


def test_ambiguous_match_returns_409_with_matches() -> None:
    client = TestClient(app)
    payload = {
        "model": "gpt-4o-mini",
        "messages": [
            {
                "role": "system",
                "content": (
                    # Intentionally include markers for TWO handlers:
                    # - TasksRequirementsAnalystHandler: "Senior Technical Architect" + intents/entities/constraints
                    # - AnalystHandler: "Senior Requirements Analyst" + json only + intents
                    "Role: Senior Technical Architect\n"
                    "Role: Senior Requirements Analyst\n"
                    "Output Contract (JSON only):\n"
                    '{"intents": ["intent1"], "entities": ["Entity1"], "constraints": ["constraint1"]}'
                ),
            },
            {"role": "user", "content": "Requirements."},
        ],
    }
    resp = client.post("/v1/chat/completions", json=payload, headers=_auth_headers())
    assert resp.status_code == 409, resp.text
    body = resp.json()
    assert body["status"] == "error"
    detail = body["detail"]
    assert detail["error"] == "Ambiguous handler match"
    assert isinstance(detail["matches"], list)
    assert len(detail["matches"]) >= 2


def test_azure_deployment_route_injects_model() -> None:
    client = TestClient(app)
    payload = {
        "messages": [{"role": "user", "content": "Convert this page to docling format with detailed descriptions."}],
    }
    resp = client.post(
        "/openai/deployments/my-deployment/chat/completions",
        json=payload,
        headers=_api_key_headers(),
    )
    assert resp.status_code == 200, resp.text
    out = resp.json()
    assert out["model"] == "my-deployment"


def test_embeddings_echo_requested_model() -> None:
    client = TestClient(app)
    payload = {"model": "text-embedding-3-large", "input": ["a", "b"]}
    resp = client.post("/v1/embeddings", json=payload, headers=_auth_headers())
    assert resp.status_code == 200, resp.text
    out = resp.json()
    assert out["model"] == "text-embedding-3-large"
    assert len(out["data"]) == 2


