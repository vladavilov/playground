import json
from typing import Any, Dict, List

import pytest
import types


class FakeNeo4jSession:
    def __init__(self):
        self.closed = False

    def run(self, query: str, **params: Any):
        if query.strip().lower().startswith("show indexes"):
            return [{"name": "graphrag_comm_index"}, {"name": "chunk_idx"}]
        if "db.index.vector.queryNodes" in query and "graphrag_comm_index" in query:
            class Node:
                def __init__(self, nid: int):
                    self.id = nid
            return [{"node": Node(1), "score": 0.9}, {"node": Node(2), "score": 0.8}]
        if "RETURN c.community AS cid, collect(distinct chunk.id) AS chunk_ids" in query:
            return [{"cid": 1, "chunk_ids": [10, 11, 12]}, {"cid": 2, "chunk_ids": [20, 21, 22]}]
        if "RETURN ch.id AS cid ORDER BY score DESC LIMIT" in query:
            return [{"cid": 10}, {"cid": 11}, {"cid": 12}]
        if "OPTIONAL MATCH (n)-[:FROM_CHUNK]->(ch) RETURN cid, count(n) AS ncnt" in query:
            return [{"cid": 10, "ncnt": 1}]
        if "MATCH (c:__Community__)-[:IN_PROJECT]->(:__Project__ {id: $projectId}) WHERE c.community IN $ids RETURN c.community AS id, c.summary AS summary" in query:
            return [{"id": 1, "summary": "community 1"}, {"id": 2, "summary": "community 2"}]
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
        if "you are drift-search primer" in text:
            content = {
                "initial_answer": "init",
                "followups": [
                    {"question": "q1", "target_communities": [1]},
                ],
                "rationale": "r",
            }
            return types.SimpleNamespace(json=lambda: {"choices": [{"message": {"content": json_dumps(content)}}]})
        if "you are drift-search local executor" in text:
            content = {
                "answer": "a1",
                "citations": [{"chunk_id": 10, "span": "s"}],
                "new_followups": [],
                "confidence": 0.9,
                "should_continue": False,
            }
            return types.SimpleNamespace(json=lambda: {"choices": [{"message": {"content": json_dumps(content)}}]})
        if "you are drift-search aggregator" in text:
            content = {
                "final_answer": "final",
                "key_facts": [{"fact": "f", "citations": [10]}],
                "residual_uncertainty": "u",
            }
            return types.SimpleNamespace(json=lambda: {"choices": [{"message": {"content": json_dumps(content)}}]})
        if path.endswith("/embeddings"):
            return types.SimpleNamespace(json=lambda: {"data": [{"embedding": [0.1] * 10}]})
        return types.SimpleNamespace(json=lambda: {"choices": [{"message": {"content": "{}"}}]})

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        return False


def json_dumps(o: Dict[str, Any]) -> str:
    return json.dumps(o)


@pytest.fixture(autouse=True)
def _env(monkeypatch):
    monkeypatch.setenv("OAI_BASE_URL", "http://mock:8000/v1")
    monkeypatch.setenv("OAI_KEY", "key")
    monkeypatch.setenv("GRAPHRAG_COMM_INDEX", "graphrag_comm_index")
    monkeypatch.setenv("GRAPHRAG_CHUNK_INDEX", "chunk_idx")
    yield


def test_service_retrieve_returns_aggregated_json(monkeypatch):
    # Lazy import after monkeypatch paths
    import importlib
    mod = importlib.import_module("services.neo4j_retrieval_service.src.services.retrieval_service".replace("/", "."))

    # Build service with injected factories
    service = mod.Neo4jRetrievalService(
        get_session=lambda: FakeNeo4jSession(),
        get_oai=lambda: FakeHttpClient(),
    )

    result = pytest.run(async_fn=service.retrieve("q", top_k=2)) if hasattr(pytest, "run") else __import__("asyncio").get_event_loop().run_until_complete(service.retrieve("q", top_k=2))

    assert "final_answer" in result
    assert isinstance(result.get("key_facts"), list)
    assert "residual_uncertainty" in result


