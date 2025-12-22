import sys
from pathlib import Path
import json
from typing import Any, Dict, List

import httpx
import pytest

# Ensure `src/` is importable without external pytest plugins.
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))


class _Msg:
    def __init__(self, content: str):
        self.content = content


class FakeLLM:
    async def ainvoke(self, messages):
        text = "\n".join([str(getattr(m, "content", m)) for m in messages])
        t = text.lower()
        if "hypothetical answer paragraph" in t:
            return _Msg("hypothetical paragraph")
        if "you are drift-search primer" in t:
            return _Msg(json.dumps({
                "initial_answer": "init",
                "followups": [{"question": "q1", "target_communities": [1]}],
                "rationale": "r",
            }))
        if "you are drift-search local executor" in t:
            return _Msg(json.dumps({
                "answer": "a1",
                "citations": [{"chunk_id": "c10", "span": "deck overview"}],
                "new_followups": [],
                "confidence": 0.9,
                "should_continue": False,
            }))
        if "you are drift-search aggregator" in t:
            return _Msg(json.dumps({
                "final_answer": "final",
                "key_facts": [{"fact": "f", "citations": ["c10"]}],
                "residual_uncertainty": "u",
            }))
        return _Msg("{}")


class FakeEmbedder:
    async def aembed_documents(self, texts):
        return [[0.1] * 8 for _ in texts]


@pytest.fixture(autouse=True)
def _env(monkeypatch):
    # Keep test vectors small.
    monkeypatch.setenv("VECTOR_INDEX_DIMENSIONS", "8")
    monkeypatch.setenv("CHUNK_VECTOR_INDEX_NAME", "chunk_idx")
    monkeypatch.setenv("COMMUNITY_VECTOR_INDEX_NAME", "comm_idx")
    monkeypatch.setenv("NEO4J_REPOSITORY_SERVICE_URL", "http://mock-repo")
    # Settings are cached; clear to pick up env overrides.
    from config import get_retrieval_settings
    get_retrieval_settings.cache_clear()
    yield


@pytest.mark.asyncio
async def test_service_retrieve_returns_aggregated_json(monkeypatch):
    from services.retrieval_service import Neo4jRetrievalService
    from retrieval_ms.nodes import primer_node as primer_mod
    from retrieval_ms.nodes import followups_node as followups_mod

    calls: List[Dict[str, Any]] = []

    async def fake_post_json(_client, path: str, payload: Dict[str, Any]) -> Dict[str, Any]:
        calls.append({"path": path, "payload": payload})
        if path == "/v1/retrieval/primer-context":
            return {
                "communities": [1, 2],
                "community_brief": [{"id": 1, "summary": "s1"}, {"id": 2, "summary": "s2"}],
            }
        if path == "/v1/retrieval/followup-context":
            return {
                "chunk_ids": ["c10", "c11"],
                "neighborhoods": [
                    {
                        "chunk_id": "c10",
                        "text": "deck overview",
                        "document_name": "doc1",
                        "neighbours": [{"properties": {"name": "deck", "description": "bridge deck"}}],
                    },
                    {"chunk_id": "c11", "text": "other", "document_name": "doc2", "neighbours": []},
                ],
            }
        raise AssertionError(f"Unexpected path: {path}")

    # Nodes import `post_json` directly, so patch their module-level symbol.
    monkeypatch.setattr(primer_mod, "post_json", fake_post_json, raising=True)
    monkeypatch.setattr(followups_mod, "post_json", fake_post_json, raising=True)

    service = Neo4jRetrievalService(
        get_repo=lambda: httpx.AsyncClient(base_url="http://mock-repo"),
        get_llm=lambda: FakeLLM(),
        get_embedder=lambda: FakeEmbedder(),
    )

    result = await service.retrieve(
        question="what are the main components of the bridge?",
        top_k=2,
        project_id="test-project",
    )

    assert result.get("no_data_found") is not True
    assert "final_answer" in result
    assert isinstance(result.get("key_facts"), list)
    assert "residual_uncertainty" in result

    # Precision: followup should respect target_communities=[1]
    followup_calls = [c for c in calls if c["path"] == "/v1/retrieval/followup-context"]
    assert followup_calls, "Expected followup-context calls"
    assert followup_calls[0]["payload"]["community_ids"] == [1]


