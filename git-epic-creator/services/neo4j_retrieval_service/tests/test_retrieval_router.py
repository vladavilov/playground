import json
import types
from typing import Any, Dict, List

import os
import importlib
import pytest
from fastapi import FastAPI
from fastapi.testclient import TestClient


class FakeNeo4jSession:
    def __init__(self):
        self.closed = False

    def run(self, query: str, **params: Any):
        # Simulate SHOW INDEXES returning both required indexes
        if query.strip().lower().startswith("show indexes"):
            return [{"name": "graphrag_comm_index"}, {"name": "chunk_idx"}]
        # Community vector search → return rows with nodes having id attributes
        if "db.index.vector.queryNodes" in query and "graphrag_comm_index" in query:
            class Node:
                def __init__(self, nid: int):
                    self.id = nid
            # Return two communities with ids 1, 2
            return [{"node": Node(1), "score": 0.9}, {"node": Node(2), "score": 0.8}]
        # Sample chunks per community (primer phase)
        if "RETURN c.community AS cid, collect(distinct chunk.id) AS chunk_ids" in query:
            return [{"cid": 1, "chunk_ids": [10, 11, 12]}, {"cid": 2, "chunk_ids": [20, 21, 22]}]
        # Scoped retrieval for followups
        if "RETURN ch.id AS cid ORDER BY score DESC LIMIT" in query:
            return [{"cid": 10}, {"cid": 11}, {"cid": 12}]
        # Neighborhood expansion
        if "OPTIONAL MATCH (n)-[:FROM_CHUNK]->(ch) RETURN cid, count(n) AS ncnt" in query:
            return [{"cid": 10, "ncnt": 1}]
        return []

    def close(self):
        self.closed = True

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        self.close()


class FakeHttpClient:
    def __init__(self, *args, **kwargs):
        pass

    def post(self, path: str, json: Dict[str, Any]):
        messages: List[Dict[str, Any]] = json.get("messages", [])
        text = "\n".join([str(m.get("content", "")).lower() for m in messages if isinstance(m, dict)])
        content: Dict[str, Any]
        if "hypothetical answer paragraph" in text or "hyde" in text:
            # HyDE ignored by pipeline for embedding in tests
            return types.SimpleNamespace(json=lambda: {"choices": [{"message": {"content": "hypothetical paragraph"}}]})
        if "you are drift-search primer" in text:
            content = {
                "initial_answer": "Bridges consist of deck, supports, and load-bearing structures.",
                "followups": [
                    {"question": "Clarify deck materials and structural role.", "target_communities": [1]},
                    {"question": "Explain arch mechanics in load distribution.", "target_communities": [2]},
                ],
                "rationale": "Primer synthesized",
            }
            return types.SimpleNamespace(json=lambda: {"choices": [{"message": {"content": json_dumps(content)}}]})
        if "you are drift-search local executor" in text:
            content = {
                "answer": "The deck carries traffic; arches transfer loads to supports.",
                "citations": [{"chunk_id": 10, "span": "deck overview"}],
                "new_followups": [],
                "confidence": 0.9,
                "should_continue": False,
            }
            return types.SimpleNamespace(json=lambda: {"choices": [{"message": {"content": json_dumps(content)}}]})
        if "you are drift-search aggregator" in text:
            content = {
                "final_answer": "Bridges comprise the deck, supports, and load-bearing structures such as arches or cables.",
                "key_facts": [
                    {"fact": "Deck carries traffic and distributes loads.", "citations": [0]},
                    {"fact": "Arches channel forces into supports.", "citations": [1]},
                ],
                "residual_uncertainty": "Specific materials and design vary by bridge type.",
            }
            return types.SimpleNamespace(json=lambda: {"choices": [{"message": {"content": json_dumps(content)}}]})
        # default noop
        return types.SimpleNamespace(json=lambda: {"choices": [{"message": {"content": "{}"}}]})

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        return False


def json_dumps(obj: Dict[str, Any]) -> str:
    return json.dumps(obj)


@pytest.fixture(autouse=True)
def _env_vars(monkeypatch):
    monkeypatch.setenv("OAI_BASE_URL", "http://mock:8000/v1")
    monkeypatch.setenv("OAI_KEY", "key")
    monkeypatch.setenv("NEO4J_URI", "bolt://mock:7687")
    monkeypatch.setenv("NEO4J_USERNAME", "neo4j")
    monkeypatch.setenv("NEO4J_PASSWORD", "pass")
    monkeypatch.setenv("NEO4J_DATABASE", "neo4j")
    yield


def mount_app(router_module):
    app = FastAPI()
    app.include_router(router_module.retrieval_router, prefix="/retrieve")
    return app


def test_retrieve_returns_aggregated_json(monkeypatch):
    # Import module fresh to ensure monkeypatch applies cleanly
    mod = importlib.import_module("services.neo4j_retrieval_service.src.routers.retrieval_router".replace("/", "."))

    # Patch session and http client factories
    monkeypatch.setattr(mod, "_neo4j_session", lambda: FakeNeo4jSession())
    monkeypatch.setattr(mod, "_oai_client", lambda: FakeHttpClient())

    app = mount_app(mod)
    client = TestClient(app)

    resp = client.post("/retrieve", json={"query": "what are the main components of the bridge?", "top_k": 2})
    assert resp.status_code == 200
    body = resp.json()

    # Expect aggregator-shaped response per README
    assert "final_answer" in body
    assert isinstance(body.get("key_facts"), list)
    assert "residual_uncertainty" in body
