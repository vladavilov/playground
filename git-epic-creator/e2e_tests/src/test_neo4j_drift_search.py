from __future__ import annotations

from pathlib import Path

import pytest
from neo4j import GraphDatabase
import os
import requests

from services.cypher_loader import execute_cypher_script
from services.neo4j_admin import supports_multi_db, ensure_database, drop_database, list_databases


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


@pytest.fixture(scope="session")
def cyphers_path() -> Path:
    # Try multiple locations to support both repo and container layouts
    candidates = [
        Path(__file__).parent / ".." / "resources" / "drift_search_cyphers.txt",
        Path("/e2e-tests/resources/drift_search_cyphers.txt"),
        Path("/e2e-tests") / "resources" / "drift_search_cyphers.txt",
        Path.cwd() / "resources" / "drift_search_cyphers.txt",
    ]
    for p in candidates:
        p = p.resolve()
        if p.exists():
            return p
    raise AssertionError(f"Cypher script not found in any known location. Tried: {candidates}")


@pytest.fixture(scope="session")
def driver(neo4j_config) -> GraphDatabase.driver:
    return GraphDatabase.driver(neo4j_config["uri"], auth=(neo4j_config["username"], neo4j_config["password"]))


@pytest.fixture(scope="session")
def target_db_name(neo4j_config) -> str:
    return neo4j_config.get("database", "neo4j")


@pytest.fixture(scope="session")
def ensure_clean_session_setup(driver, target_db_name, cyphers_path):
    cleanup_session(driver, target_db_name)
    execute_cypher_script(driver, target_db_name, cyphers_path)
    yield


def cleanup_session(driver, target_db_name):
    with driver.session(database=target_db_name) as session:
        session.run("MATCH (n) DETACH DELETE n").consume()
        session.run("CALL apoc.schema.assert({}, {})").consume()
        for idx in REQUIRED_INDEXES:
            session.run(f"DROP INDEX {idx} IF EXISTS").consume()


@pytest.fixture(scope="session", autouse=True)
def cleanup_after_session(driver, target_db_name):
    yield
    cleanup_session(driver, target_db_name)


def test_seed_data_loaded(driver, target_db_name, ensure_clean_session_setup):
    with driver.session(database=target_db_name) as session:
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


def test_indexes_exist(driver, target_db_name, ensure_clean_session_setup):
    with driver.session(database=target_db_name) as session:
        result = session.run(
            """
            SHOW INDEXES YIELD name, type, entityType, labelsOrTypes, properties, options
            RETURN name, type, entityType, labelsOrTypes, properties, options
            """
        )
        indexes = [dict(r) for r in result]
        names = {idx["name"] for idx in indexes}

        required_always = {"graphrag_comm_index", "community_summary_fts", "chunk_text_fts", "graphrag_chunk_index"}
        missing_always = required_always - names
        assert not missing_always, f"Missing indexes: {missing_always}. Present: {names}"

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


def test_embeddings_written(driver, target_db_name, ensure_clean_session_setup):
    with driver.session(database=target_db_name) as session:
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


def test_indexes_queryable(driver, target_db_name, ensure_clean_session_setup):
    with driver.session(database=target_db_name) as session:
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


def test_drop_test_db_conditional(driver):
    if not supports_multi_db(driver):
        pytest.skip("Multi-database not supported; skipping DB drop test")
    test_db = "drift_search_test"
    try:
        ensure_database(driver, test_db, wait_seconds=5)
    except Exception:
        pytest.skip("CREATE DATABASE not supported; skipping DB drop test")
    names = {row.get("name") for row in list_databases(driver)}
    if test_db not in names:
        pytest.skip("Database creation not permitted; skipping DB drop test")
    drop_database(driver, test_db)
    names = {row.get("name") for row in list_databases(driver)}
    assert test_db not in names


def _call_retrieval_service(question: str) -> requests.Response:
    base = os.getenv("RETRIEVAL_BASE_URL", "http://neo4j-retrieval-service:8000")
    url = base.rstrip('/') + "/retrieve"
    payload = {"query": question}
    resp = requests.post(url, json=payload, timeout=10)
    return resp


def test_retrieval_service_end_to_end_requests_recorded(driver):
    question = "what are the main components of the bridge?"

    resp = _call_retrieval_service(question)

    assert resp.status_code == 200, f"retrieval status {resp.status_code}, body={resp.text[:200]}"

    try:
        body = resp.json()
    except Exception:
        pytest.fail(f"Retrieval service returned non-JSON: {resp.text[:200]}")

    assert "final_answer" in body
    assert "key_facts" in body
    assert "residual_uncertainty" in body

    assert "Bridges comprise the deck, supports, and load-bearing structures such as arches or cables." in body['final_answer']

    citations = body['key_facts'][0]['citations']

    assert "citations" in citations
    assert "neighbours" in citations
    assert "4. Types of Bridges:" in citations
    assert "1. Introduction:" in citations
    assert "Clarify deck materials and structural role." in citations
    assert "Explain arch mechanics in load distribution." in citations
    assert "List common deck materials and how they influence load distribution." in citations

    assert "Arches carry deck loads p" in citations
    assert "Bridge decks are commonly built from reinforced" in citations
    assert "Reinforced concrete decks spread loa" not in citations # no answers for follow-ups of the second level

    assert "ERROR FETCHING LOCAL EXECUTOR" not in citations # no errors