"""
Neo4j repository service integration tests for project-scoped GraphRAG IDs.

This test guards against cross-project collisions when two projects ingest the same
upstream IDs (document/chunk/entity). The expected behavior is isolation by
(project_id, id) composite keys.
"""

from __future__ import annotations

import pytest

from shared_utils import HTTPUtils
from config import TestConstants


@pytest.mark.integration
def test_repository_bundle_ingest_is_project_scoped(
    neo4j_driver,
    target_db_name,
    service_urls,
    auth_headers,
    services_ready,
):
    """
    Ingest two bundles with identical IDs into two different projects and assert:
    - Nodes are not overwritten (two docs/chunks/entities exist).
    - Relationships do not cross-link between projects.
    """
    repo = service_urls["neo4j_repository"].rstrip("/")

    # NOTE: the ensure_clean_session_setup fixture already resets Neo4j and calls init-schema.

    # Two distinct projects but same upstream IDs.
    p1 = "p_scope_1"
    p2 = "p_scope_2"

    shared_doc_id = "d1"
    shared_chunk_id = "c1"
    shared_entity_id = "e1"

    # Minimal GraphRAG-ish bundle (documents/chunks/entities + basic community scaffolding).
    def bundle(project_id: str) -> dict:
        return {
            "project_id": project_id,
            "documents": [{"id": shared_doc_id, "text": f"doc text for {project_id}", "metadata": {"file_name": "x.pdf"}}],
            "chunks": [{"id": shared_chunk_id, "text": f"chunk text for {project_id}", "document_ids": [shared_doc_id]}],
            "entities": [{"id": shared_entity_id, "title": "A", "norm_title": "A", "description": f"desc {project_id}", "text_unit_ids": [shared_chunk_id]}],
            "relationships": [],
            "community_reports": [{"community": 1, "level": 0, "title": "t", "summary": "s"}],
            "communities": [{"community": 1, "entity_ids": [shared_entity_id], "text_unit_ids": [shared_chunk_id]}],
        }

    # Ingest bundle for p1 and p2.
    for pid in (p1, p2):
        resp = HTTPUtils.make_request_with_retry(
            method="POST",
            url=f"{repo}/v1/requirements-graph/merge/bundle",
            timeout=TestConstants.DEFAULT_TIMEOUT,
            headers={**auth_headers, "Content-Type": "application/json"},
            json_data=bundle(pid),
        )
        assert resp.status_code == TestConstants.HTTP_OK, f"bundle ingest failed: {resp.status_code} {resp.text[:200]}"
        body = resp.json()
        # Validate response shape (counts returned by merge_bundle_full).
        for k in [
            "documents_created",
            "chunks_created",
            "entities_created",
            "relationships_processed",
            "community_reports_created",
            "communities_created",
        ]:
            assert k in body, f"bundle response missing {k}"
        assert int(body["documents_created"]) >= 1
        assert int(body["chunks_created"]) >= 1
        assert int(body["entities_created"]) >= 1
        assert int(body["community_reports_created"]) >= 1
        assert int(body["communities_created"]) >= 1

    # Validate in Neo4j directly (strongest signal).
    with neo4j_driver.session(database=target_db_name) as session:
        # Sanity: required project nodes exist.
        rec = session.run(
            "MATCH (p:__Project__) WHERE p.id IN [$p1, $p2] RETURN collect(DISTINCT p.id) AS ids",
            p1=p1,
            p2=p2,
        ).single()
        assert rec is not None
        assert set(rec["ids"]) == {p1, p2}

        # 1) Two documents with same id, different project_id.
        rec = session.run(
            """
            MATCH (d:__Document__ {id: $did})
            RETURN collect(DISTINCT d.project_id) AS pids, count(DISTINCT d) AS c
            """,
            did=shared_doc_id,
        ).single()
        assert rec is not None
        assert int(rec["c"]) == 2
        assert set(rec["pids"]) == {p1, p2}

        # 2) Two chunks with same id, different project_id.
        rec = session.run(
            """
            MATCH (c:__Chunk__ {id: $cid})
            RETURN collect(DISTINCT c.project_id) AS pids, count(DISTINCT c) AS c
            """,
            cid=shared_chunk_id,
        ).single()
        assert rec is not None
        assert int(rec["c"]) == 2
        assert set(rec["pids"]) == {p1, p2}

        # 3) Two entities with same id, different project_id.
        rec = session.run(
            """
            MATCH (e:__Entity__ {id: $eid})
            RETURN collect(DISTINCT e.project_id) AS pids, count(DISTINCT e) AS c
            """,
            eid=shared_entity_id,
        ).single()
        assert rec is not None
        assert int(rec["c"]) == 2
        assert set(rec["pids"]) == {p1, p2}

        # 4) No cross-project contamination: each project's document links only to its project's chunk.
        rec = session.run(
            """
            MATCH (p:__Project__ {id: $pid})
            MATCH (d:__Document__ {project_id: $pid, id: $did})-[:IN_PROJECT]->(p)
            MATCH (d)-[:HAS_CHUNK]->(c:__Chunk__)
            RETURN collect(DISTINCT c.project_id) AS chunk_pids, count(DISTINCT c) AS chunk_count
            """,
            pid=p1,
            did=shared_doc_id,
        ).single()
        assert rec is not None
        assert int(rec["chunk_count"]) == 1
        assert rec["chunk_pids"] == [p1]

        rec = session.run(
            """
            MATCH (p:__Project__ {id: $pid})
            MATCH (d:__Document__ {project_id: $pid, id: $did})-[:IN_PROJECT]->(p)
            MATCH (d)-[:HAS_CHUNK]->(c:__Chunk__)
            RETURN collect(DISTINCT c.project_id) AS chunk_pids, count(DISTINCT c) AS chunk_count
            """,
            pid=p2,
            did=shared_doc_id,
        ).single()
        assert rec is not None
        assert int(rec["chunk_count"]) == 1
        assert rec["chunk_pids"] == [p2]

        # 5) No cross-project contamination for HAS_ENTITY: each project's chunk links only to its project's entity.
        rec = session.run(
            """
            MATCH (c:__Chunk__ {project_id: $pid, id: $cid})
            MATCH (c)-[:HAS_ENTITY]->(e:__Entity__)
            RETURN collect(DISTINCT e.project_id) AS entity_pids, count(DISTINCT e) AS entity_count
            """,
            pid=p1,
            cid=shared_chunk_id,
        ).single()
        assert rec is not None
        assert int(rec["entity_count"]) == 1
        assert rec["entity_pids"] == [p1]

        rec = session.run(
            """
            MATCH (c:__Chunk__ {project_id: $pid, id: $cid})
            MATCH (c)-[:HAS_ENTITY]->(e:__Entity__)
            RETURN collect(DISTINCT e.project_id) AS entity_pids, count(DISTINCT e) AS entity_count
            """,
            pid=p2,
            cid=shared_chunk_id,
        ).single()
        assert rec is not None
        assert int(rec["entity_count"]) == 1
        assert rec["entity_pids"] == [p2]

        # 6) Communities exist per project and are linked correctly.
        rec = session.run(
            """
            MATCH (c:__Community__ {community: 1})
            RETURN collect(DISTINCT c.project_id) AS pids, count(DISTINCT c) AS c
            """,
        ).single()
        assert rec is not None
        assert int(rec["c"]) == 2
        assert set(rec["pids"]) == {p1, p2}

        # Ensure IN_COMMUNITY stays within project.
        rec = session.run(
            """
            MATCH (e:__Entity__ {project_id: $pid, id: $eid})-[:IN_COMMUNITY]->(c:__Community__ {project_id: $pid})
            RETURN count(*) AS cnt
            """,
            pid=p1,
            eid=shared_entity_id,
        ).single()
        assert rec is not None and int(rec["cnt"]) >= 1

        rec = session.run(
            """
            MATCH (e:__Entity__ {project_id: $pid, id: $eid})-[:IN_COMMUNITY]->(c:__Community__ {project_id: $pid})
            RETURN count(*) AS cnt
            """,
            pid=p2,
            eid=shared_entity_id,
        ).single()
        assert rec is not None and int(rec["cnt"]) >= 1


