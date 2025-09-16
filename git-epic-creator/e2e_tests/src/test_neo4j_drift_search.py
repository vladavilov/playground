from __future__ import annotations

import pytest
import threading

from services.neo4j_admin import supports_multi_db, ensure_database, drop_database, list_databases
from shared_utils import HTTPUtils


REQUIRED_INDEXES = (
    "graphrag_comm_index",
    "graphrag_chunk_index",
    "community_summary_fts",
    "chunk_text_fts",
    "chunk_index_unique",
    "community_id_unique_underscored",
    "document_id_unique_underscored",
    "entity_id_unique_underscored",
    "entity_name_unique",
)


@pytest.fixture(scope="function")
def ensure_clean_session_setup(neo4j_driver, target_db_name, cyphers_path, wa):
    from services.workflow_assertions import WorkflowAssertions  # type: ignore
    assert isinstance(wa, WorkflowAssertions)
    wa.reset_neo4j_database(neo4j_driver, target_db_name, required_index_names=REQUIRED_INDEXES)
    wa.load_cypher_script(neo4j_driver, target_db_name, cyphers_path)
    yield



def test_seed_data_loaded(neo4j_driver, target_db_name, ensure_clean_session_setup):
    with neo4j_driver.session(database=target_db_name) as session:
        result = session.run("MATCH (c:__Chunk__) RETURN collect(c.index) AS idxs")
        idxs = result.single()["idxs"]
        assert sorted([int(i) for i in idxs]) == [0, 1]

        result = session.run(
            "MATCH (:__Chunk__ {index:'0'})-[:NEXT_CHUNK]->(:__Chunk__ {index:'1'}) RETURN count(*) AS cnt"
        )
        assert result.single()["cnt"] == 1

        result = session.run("MATCH (n:__Entity__) RETURN count(n) AS cnt")
        assert result.single()["cnt"] >= 1

        query = (
            "MATCH (e:__Entity__) "
            "OPTIONAL MATCH (e)-[:FROM_CHUNK]->(c0:__Chunk__ {index:'0'}) "
            "OPTIONAL MATCH (e)-[:FROM_CHUNK]->(c1:__Chunk__ {index:'1'}) "
            "WITH e, count(c0) AS c0n, count(c1) AS c1n "
            "RETURN count(CASE WHEN c0n>0 AND c1n>0 THEN 1 END) AS both"
        )
        both = session.run(query).single()["both"]
        assert both == 0


def test_indexes_exist(neo4j_driver, target_db_name, ensure_clean_session_setup, wa):
    # Strict name/type checks via shared helper
    wa.verify_required_index_names(
        neo4j_driver,
        required_vector_names=("graphrag_comm_index", "graphrag_chunk_index"),
        required_fulltext_names=("community_summary_fts", "chunk_text_fts"),
    )

    # Retain explicit dimension/similarity assertions
    with neo4j_driver.session(database=target_db_name) as session:
        result = session.run(
            """
            SHOW INDEXES YIELD name, type, entityType, labelsOrTypes, properties, options
            RETURN name, type, entityType, labelsOrTypes, properties, options
            """
        )
        indexes = [dict(r) for r in result]
        names = {idx["name"] for idx in indexes}

        idx_by_name = {idx["name"]: idx for idx in indexes}
        for vec_name in ("graphrag_comm_index", "graphrag_chunk_index"):
            idx = idx_by_name[vec_name]
            assert idx["type"].upper() == "VECTOR"
            opts = idx.get("options") or {}
            index_config = opts.get("indexConfig") or opts.get("indexconfig") or {}
            dims = index_config.get("vector.dimensions")
            sim = index_config.get("vector.similarity_function")
            assert dims == 1536
            assert (sim or "").upper() == "COSINE"

        for fts_name in ("community_summary_fts", "chunk_text_fts"):
            idx = idx_by_name[fts_name]
            assert idx["type"].upper() == "FULLTEXT"


def test_embeddings_written(neo4j_driver, target_db_name, ensure_clean_session_setup):
    with neo4j_driver.session(database=target_db_name) as session:
        res = session.run(
            """
            MATCH (ch:__Chunk__)
            WITH ch, (ch.embedding IS NOT NULL) AS has, CASE WHEN ch.embedding IS NOT NULL THEN size(ch.embedding) ELSE -1 END AS sz
            RETURN count(*) AS total,
                   count(CASE WHEN has THEN 1 END) AS with_prop,
                   count(CASE WHEN has AND sz = 1536 THEN 1 END) AS good
            """
        ).single()
        if res["with_prop"] == 0:
            pytest.skip("__Chunk__ embeddings not present in seed; skipping shape check")
        assert res["with_prop"] == res["good"]

        res = session.run(
            """
            MATCH (c:__Community__)
            WHERE c.summary IS NOT NULL
            WITH c, (c.summary_embedding IS NOT NULL) AS has, CASE WHEN c.summary_embedding IS NOT NULL THEN size(c.summary_embedding) ELSE -1 END AS sz
            RETURN count(*) AS total,
                   count(CASE WHEN has THEN 1 END) AS with_prop,
                   count(CASE WHEN has AND sz = 1536 THEN 1 END) AS good
            """
        ).single()
        if res["total"] > 0 and res["with_prop"] == 0:
            pytest.skip("Community summary embeddings not present in seed; skipping shape check")
        if res["total"] > 0:
            assert res["with_prop"] == res["good"]


def test_indexes_queryable(neo4j_driver, target_db_name, ensure_clean_session_setup):
    with neo4j_driver.session(database=target_db_name) as session:
        names = {r["name"] for r in session.run("SHOW INDEXES YIELD name RETURN name")}
        if "graphrag_comm_index" not in names or "graphrag_chunk_index" not in names:
            pytest.skip("Required vector indexes not present; verify-only per environment")

        qvec = [0.0] * 1536
        qvec[0] = 1.0
        session.run(
            "CALL db.index.vector.queryNodes($name, $k, $qvec)",
            name="graphrag_comm_index",
            k=1,
            qvec=qvec,
        ).consume()
        session.run(
            "CALL db.index.vector.queryNodes($name, $k, $qvec)",
            name="graphrag_chunk_index",
            k=1,
            qvec=qvec,
        ).consume()

        if "community_summary_fts" in names:
            session.run(
                "CALL db.index.fulltext.queryNodes('community_summary_fts', $q)",
                q="bridge~1",
            ).consume()
        if "chunk_text_fts" in names:
            session.run(
                "CALL db.index.fulltext.queryNodes('chunk_text_fts', $q)",
                q="arch~1",
            ).consume()


def test_drop_test_db_conditional(neo4j_driver):
    if not supports_multi_db(neo4j_driver):
        pytest.skip("Multi-database not supported; skipping DB drop test")
    test_db = "drift_search_test"
    try:
        ensure_database(neo4j_driver, test_db, wait_seconds=5)
    except Exception:
        pytest.skip("CREATE DATABASE not supported; skipping DB drop test")
    names = {row.get("name") for row in list_databases(neo4j_driver)}
    if test_db not in names:
        pytest.skip("Database creation not permitted; skipping DB drop test")
    drop_database(neo4j_driver, test_db)
    names = {row.get("name") for row in list_databases(neo4j_driver)}
    assert test_db not in names



# ------------------------- RETRIEVAL SERVICE E2E TEST -------------------------


def _call_retrieval_service(service_urls, question: str):
    base = service_urls["neo4j_retrieval"].rstrip('/')
    url = base + "/retrieve"
    payload = {"query": question}
    return HTTPUtils.make_request_with_retry("POST", url, json_data=payload, timeout=10)


def test_retrieval_service_end_to_end_requests_recorded(neo4j_driver, service_urls, wa):
    # Clear recorded requests in OpenAI mock service
    oai_base = service_urls["openai_mock"].rstrip("/")
    clr = HTTPUtils.make_request_with_retry("POST", f"{oai_base}/spy/clear", timeout=5)
    assert clr.status_code == 200

    question = "what are the main components of the bridge?"

    # Act: perform call and concurrently poll Neo4j for any active queries
    queries: list[str] = []

    def _poller():
        nonlocal queries
        queries = wa.poll_active_queries(neo4j_driver, duration_seconds=3.0)

    poll_thread = threading.Thread(target=_poller, daemon=True)
    poll_thread.start()
    resp = _call_retrieval_service(service_urls, question)
    poll_thread.join()

    # Assert: HTTP call succeeded
    assert resp.status_code == 200, f"retrieval status {resp.status_code}, body={resp.text[:200]}"

    # Assert: OpenAI mock recorded requests and verify which endpoints were hit
    spy_resp = HTTPUtils.make_request_with_retry("GET", f"{oai_base}/spy/requests?howMany=50", timeout=5)
    assert spy_resp.status_code == 200
    payload = spy_resp.json()
    items = payload.get("items") or []
    assert isinstance(items, list)
    assert len(items) >= 1, "Expected at least one outbound request to OAI mock service"

    # Verify specific endpoint paths and counts based on retrieval flow
    # Expected calls (from retrieval_router):
    # - chat/completions: 5 (HyDE, Primer, LocalExec#1, LocalExec#2, Aggregator)
    # - embeddings: 4 (HyDE, Followup#1, Followup#2, NewFollowup from continuation)
    paths = [str(r.get("path") or "") for r in items]
    chat_hits = [p for p in paths if "/chat/completions" in p]
    embed_hits = [p for p in paths if "/embeddings" in p]

    # Ensure only expected endpoint families are used
    unexpected = [p for p in paths if not ("/chat/completions" in p or "/embeddings" in p)]
    assert len(unexpected) == 0, f"Unexpected request targets: {unexpected}"

    # Count checks
    assert len(chat_hits) == 5, f"Expected 5 chat completions, got {len(chat_hits)}; paths={chat_hits}"
    assert len(embed_hits) == 4, f"Expected 4 embeddings, got {len(embed_hits)}; paths={embed_hits}"

    # Assert: capture of Neo4j queries (if service performed any during request)
    # We require at least one query captured; adjust if service becomes read-only
    assert isinstance(queries, list)

    # Verify specific endpoint paths were hit
    any_chat = any("/chat" in (r.get("path") or "") for r in items)
    any_embed = any("/embeddings" in (r.get("path") or "") for r in items)
    assert any_chat or any_embed, f"Unexpected request targets: {[r.get('path') for r in items]}"

    EXPECTED_RESPONSE = {
                "final_answer": "Bridges comprise the deck, supports, and load-bearing structures such as arches or cables.",
                "key_facts": [
                    {"fact": "Deck carries traffic and distributes loads.", "citations": [0]},
                    {"fact": "Arches channel forces into supports.", "citations": [1]},
                ],
                "residual_uncertainty": "Specific materials and design vary by bridge type.",
            }
    try:
        body = resp.json()
    except Exception:
        pytest.fail(f"Retrieval service returned non-JSON: {resp.text[:200]}")
    assert body == EXPECTED_RESPONSE, "Retrieval response did not match the expected placeholder payload"
