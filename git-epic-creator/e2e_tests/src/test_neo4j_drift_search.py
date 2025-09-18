from __future__ import annotations

import pytest

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
