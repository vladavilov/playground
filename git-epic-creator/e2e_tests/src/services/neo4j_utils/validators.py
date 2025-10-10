"""
Neo4j validation operations for e2e tests.

This module provides Neo4j graph validation functionality following
Single Responsibility Principle.
"""

from __future__ import annotations

from typing import Any, Dict, List, Iterable

from services.workflow_models import WorkflowTestFixtures


class Neo4jValidators:
    """
    Neo4j graph validation operations.
    
    Provides validation for:
    - Graph structure verification (projects, documents, chunks, entities)
    - Constraint validation
    - Vector index validation
    - Index name verification
    """

    @staticmethod
    def verify_project_and_documents(
        project_id: str,
        fixtures: WorkflowTestFixtures,
        *,
        min_docs: int = 1,
        comprehensive: bool = False
    ) -> None:
        """
        Verify Neo4j graph structure for GraphRAG-ingested data.
        
        Args:
            project_id: Project UUID to scope validation
            fixtures: Test fixtures with neo4j_driver
            min_docs: Minimum expected document count (default: 1)
            comprehensive: If True, performs deep validation of properties, 
                          embeddings, and relationships (default: False)
        
        Basic validation (comprehensive=False):
            - Project node exists
            - Documents exist (scoped to project)
            - HAS_CHUNK edges exist
            - HAS_ENTITY links exist
        
        Comprehensive validation (comprehensive=True):
            - All basic checks
            - Document properties (id, title, text, project_id)
            - Chunk embeddings with correct dimensions (1536)
            - Entity properties (id, title, description, type, norm_title)
            - RELATED relationships with properties
            - Communities with proper hierarchy
            - IN_COMMUNITY relationships
        """
        with fixtures.neo4j_driver.session() as session:
            # 1. Verify Project node exists (always check)
            project_result = session.run(
                "MATCH (p:__Project__ {id: $project_id}) RETURN p",
                project_id=project_id
            ).single()
            assert project_result is not None, f"Project {project_id} not found in Neo4j"
            
            # 2. Verify Documents (scoped to project)
            doc_count_record = session.run(
                """
                MATCH (d:__Document__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
                RETURN count(d) AS c
                """,
                project_id=project_id
            ).single()
            assert doc_count_record is not None, "Failed to query __Document__ nodes"
            doc_count = int(doc_count_record["c"] or 0)
            assert doc_count >= min_docs, (
                f"Expected at least {min_docs} __Document__ node(s) for project {project_id}, found {doc_count}"
            )
            
            if comprehensive and doc_count > 0:
                # Verify document properties
                docs = list(session.run(
                    """
                    MATCH (d:__Document__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
                    RETURN d.id AS id, d.title AS title, d.text AS text, d.project_id AS pid
                    LIMIT 5
                    """,
                    project_id=project_id
                ))
                for doc in docs:
                    assert doc['id'], "Document should have id"
                    assert doc['text'], "Document should have text"
                    assert doc['pid'] == project_id, f"Document project_id mismatch"

            # 3. Verify Document -> Chunk edges (scoped to project)
            chunk_edge_record = session.run(
                """
                MATCH (d:__Document__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
                MATCH (d)-[:HAS_CHUNK]->(c:__Chunk__)
                RETURN count(DISTINCT c) AS c
                """,
                project_id=project_id
            ).single()
            assert chunk_edge_record is not None, "Failed to query HAS_CHUNK edges to __Chunk__"
            chunk_count = int(chunk_edge_record["c"] or 0)
            if min_docs > 0:
                assert chunk_count > 0, (
                    f"Expected HAS_CHUNK edges from __Document__ to __Chunk__ for project {project_id}"
                )
            
            if comprehensive and chunk_count > 0:
                # Verify chunks have embeddings with correct dimensions
                chunks_with_embeddings = list(session.run(
                    """
                    MATCH (ch:__Chunk__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
                    WHERE ch.embedding IS NOT NULL
                    WITH count(ch) AS count, head(collect(ch.embedding)) AS sample_emb
                    RETURN count, size(sample_emb) AS dims
                    """,
                    project_id=project_id
                ))
                if len(chunks_with_embeddings) > 0 and chunks_with_embeddings[0]['count'] > 0:
                    dims = chunks_with_embeddings[0]['dims']
                    if dims is not None:
                        assert dims == 1536, f"Embeddings should be 1536 dimensions, got {dims}"

            # 4. Verify Chunk -> Entity links (scoped to project)
            entity_link_record = session.run(
                """
                MATCH (ch:__Chunk__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
                MATCH (ch)-[:HAS_ENTITY]->(e:__Entity__)
                RETURN count(e) AS c
                """,
                project_id=project_id
            ).single()
            assert entity_link_record is not None, "Failed to query HAS_ENTITY links from __Chunk__ to __Entity__"
            entity_count = int(entity_link_record["c"] or 0)
            
            if comprehensive and entity_count > 0:
                # Verify entity properties
                entities = list(session.run(
                    """
                    MATCH (e:__Entity__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
                    RETURN e.id AS id, e.title AS title, e.description AS description, 
                           e.type AS type, e.norm_title AS norm_title, e.project_id AS pid
                    LIMIT 5
                    """,
                    project_id=project_id
                ))
                for entity in entities:
                    assert entity['id'], f"Entity should have id"
                    assert entity['title'], f"Entity {entity['id']} should have title"
                    assert entity['description'], f"Entity {entity['id']} should have description"
                    assert entity['norm_title'], f"Entity {entity['id']} should have normalized title"
                    assert entity['pid'] == project_id, f"Entity project_id mismatch"
                    # norm_title should be uppercase version of title
                    if entity['title'] and entity['norm_title']:
                        assert entity['norm_title'] == entity['title'].upper(), \
                            f"norm_title should be uppercase of title"
                
                # Verify RELATED relationships exist between entities
                related_count = session.run(
                    """
                    MATCH (e1:__Entity__)-[r:RELATED]->(e2:__Entity__)
                    WHERE (e1)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
                    RETURN count(r) AS count
                    """,
                    project_id=project_id
                ).single()
                if related_count and related_count['count'] > 0:
                    # Verify RELATED relationships have required properties
                    related_props = list(session.run(
                        """
                        MATCH (e1:__Entity__)-[r:RELATED]->(e2:__Entity__)
                        WHERE (e1)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
                        RETURN r.description AS description, r.weight AS weight, r.id AS id
                        LIMIT 3
                        """,
                        project_id=project_id
                    ))
                    for rel in related_props:
                        assert rel['description'], "RELATED should have description"
                        assert rel['weight'] is not None, "RELATED should have weight"
                        assert 0.0 <= rel['weight'] <= 1.0, \
                            f"RELATED weight should be in [0,1], got {rel['weight']}"
                
                # Verify Communities were detected
                communities = list(session.run(
                    """
                    MATCH (c:__Community__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
                    RETURN c.community AS id, c.level AS level, c.summary AS summary, 
                           c.title AS title, c.project_id AS pid
                    LIMIT 5
                    """,
                    project_id=project_id
                ))
                if len(communities) > 0:
                    for comm in communities:
                        assert comm['id'] is not None, "Community should have id (community property)"
                        assert comm['level'] is not None, "Community should have level"
                        assert comm['summary'] or comm['title'], \
                            "Community should have summary or title"
                        assert comm['pid'] == project_id, f"Community project_id mismatch"
                    
                    # Verify IN_COMMUNITY relationships exist
                    in_community_count = session.run(
                        """
                        MATCH (:__Entity__)-[:IN_COMMUNITY]->(c:__Community__)
                        -[:IN_PROJECT]->(:__Project__ {id: $project_id})
                        RETURN count(*) AS count
                        """,
                        project_id=project_id
                    ).single()
                    assert in_community_count and in_community_count['count'] > 0, \
                        "Entities should be assigned to communities via IN_COMMUNITY"

    @staticmethod
    def verify_constraints_minimal(fixtures: WorkflowTestFixtures) -> None:
        """
        Verify minimal required constraints exist in Neo4j.
        
        Args:
            fixtures: Test fixtures with neo4j_driver
            
        Raises:
            AssertionError: If required constraints are missing
        """
        with fixtures.neo4j_driver.session() as session:
            constraints = list(session.run(
                """
                SHOW CONSTRAINTS
                YIELD name, entityType, labelsOrTypes, properties, type
                RETURN name, entityType, labelsOrTypes, properties, type
                """
            ))
            
            def has_node_unique(label: str) -> bool:
                for rec in constraints:
                    if (rec.get("entityType") == "NODE"
                        and label in (rec.get("labelsOrTypes") or [])
                        and "id" in (rec.get("properties") or [])
                        and "UNIQUENESS" in (rec.get("type") or "")):
                        return True
                return False
            
            assert has_node_unique("__Entity__"), "Missing unique constraint on __Entity__(id)"

    @staticmethod
    def verify_vector_index(fixtures: WorkflowTestFixtures) -> None:
        """
        Verify vector indexes exist and are ONLINE with correct settings.
        
        Args:
            fixtures: Test fixtures with neo4j_driver
            
        Raises:
            AssertionError: If vector indexes are missing or misconfigured
        """
        expected = [
            {"label": "__Chunk__", "property": "embedding"},
            {"label": "__Community__", "property": "embedding"},
        ]
        expected_dims = 1536
        expected_sim = "cosine"

        with fixtures.neo4j_driver.session() as session:
            records = list(session.run(
                """
                SHOW INDEXES
                YIELD name, state, type, entityType, labelsOrTypes, properties, options
                WHERE type = 'VECTOR'
                RETURN name, state, type, entityType, labelsOrTypes, properties, options
                """
            ))

            def find_vector_index(label: str, prop: str):
                for rec in records:
                    labels = rec.get("labelsOrTypes") or []
                    props = rec.get("properties") or []
                    if label in labels and prop in props:
                        return rec
                return None

            for cfg in expected:
                idx = find_vector_index(cfg["label"], cfg["property"])
                assert idx is not None, f"Missing vector index for {cfg['label']}({cfg['property']})"
                assert idx.get("state") == "ONLINE", f"Vector index for {cfg['label']} is not ONLINE"
                index_cfg = (idx.get("options") or {}).get("indexConfig", {}) or {}
                dims = index_cfg.get("vector.dimensions")
                sim = index_cfg.get("vector.similarity_function")
                if isinstance(sim, str):
                    sim = sim.lower()
                assert dims == expected_dims, (
                    f"Vector index dimension must be {expected_dims} for {cfg['label']}"
                )
                assert sim == expected_sim, (
                    f"Vector similarity must be {expected_sim} for {cfg['label']}"
                )

    @staticmethod
    def verify_required_index_names(
        driver: Any,
        *,
        required_vector_names: Iterable[str] = (),
        required_fulltext_names: Iterable[str] = (),
    ) -> None:
        """
        Verify specific indexes exist by name.
        
        Args:
            driver: Neo4j driver instance
            required_vector_names: Required vector index names
            required_fulltext_names: Required fulltext index names
            
        Raises:
            AssertionError: If required indexes are missing or have wrong type
        """
        with driver.session() as session:
            result = session.run(
                """
                SHOW INDEXES YIELD name, type, entityType, labelsOrTypes, properties, options
                RETURN name, type, entityType, labelsOrTypes, properties, options
                """
            )
            indexes: List[Dict[str, Any]] = [dict(r) for r in result]
            names = {idx.get("name") for idx in indexes}

            # Presence checks by family
            missing_vec = set(required_vector_names) - names
            missing_fts = set(required_fulltext_names) - names
            assert not missing_vec and not missing_fts, (
                f"Missing indexes: vector={missing_vec} fulltext={missing_fts}. Present: {names}"
            )

            # Additional sanity: types for provided names
            idx_by_name = {idx.get("name"): idx for idx in indexes}
            for nm in required_vector_names:
                if nm in idx_by_name:
                    assert str(idx_by_name[nm].get("type", "")).upper() == "VECTOR"
            for nm in required_fulltext_names:
                if nm in idx_by_name:
                    assert str(idx_by_name[nm].get("type", "")).upper() == "FULLTEXT"
