"""
Neo4j validation operations for e2e tests.

This module provides validation operations for Neo4j schema and data,
following Single Responsibility Principle.
"""

from __future__ import annotations

from typing import Any, Dict, Iterable, List

from .queries import Neo4jQueries
from services.workflow_models import WorkflowTestFixtures


class Neo4jValidators:
    """
    Validation operations for Neo4j schema and data integrity.
    
    Provides validators for:
    - Graph structure (projects, documents, chunks, entities)
    - Constraints and indexes
    - Vector indexes
    - Data quality and completeness
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
            - Chunk embeddings with correct dimensions (3072)
            - Entity properties (id, title, description, type, norm_title)
            - RELATED relationships with properties
            - Communities with proper hierarchy
            - IN_COMMUNITY relationships
        """
        with fixtures.neo4j_driver.session() as session:
            # 1. Verify Project node exists
            project_result = session.run(
                Neo4jQueries.FIND_PROJECT_BY_ID,
                project_id=project_id
            ).single()
            assert project_result is not None, f"Project {project_id} not found in Neo4j"
            
            # 2. Verify Documents (scoped to project)
            doc_count_record = session.run(
                Neo4jQueries.COUNT_DOCUMENTS_BY_PROJECT,
                project_id=project_id
            ).single()
            assert doc_count_record is not None, "Failed to query __Document__ nodes"
            doc_count = int(doc_count_record["c"] or 0)
            assert doc_count >= min_docs, (
                f"Expected at least {min_docs} __Document__ node(s) for project {project_id}, found {doc_count}"
            )
            
            if comprehensive and doc_count > 0:
                Neo4jValidators._validate_document_properties(session, project_id)
            
            # 3. Verify Document -> Chunk edges
            chunk_edge_record = session.run(
                Neo4jQueries.COUNT_CHUNKS_BY_PROJECT,
                project_id=project_id
            ).single()
            assert chunk_edge_record is not None, "Failed to query HAS_CHUNK edges to __Chunk__"
            chunk_count = int(chunk_edge_record["c"] or 0)
            if min_docs > 0:
                assert chunk_count > 0, (
                    f"Expected HAS_CHUNK edges from __Document__ to __Chunk__ for project {project_id}"
                )
            
            if comprehensive and chunk_count > 0:
                Neo4jValidators._validate_chunk_embeddings(session, project_id)
            
            # 4. Verify Chunk -> Entity links
            entity_link_record = session.run(
                Neo4jQueries.COUNT_ENTITIES_BY_PROJECT,
                project_id=project_id
            ).single()
            assert entity_link_record is not None, "Failed to query HAS_ENTITY links from __Chunk__ to __Entity__"
            entity_count = int(entity_link_record["c"] or 0)
            
            if comprehensive and entity_count > 0:
                Neo4jValidators._validate_entities_and_relationships(session, project_id)
                Neo4jValidators._validate_communities(session, project_id)
    
    @staticmethod
    def _validate_document_properties(session: Any, project_id: str) -> None:
        """Validate document properties for comprehensive checks."""
        docs = list(session.run(
            Neo4jQueries.GET_DOCUMENT_PROPERTIES,
            project_id=project_id,
            limit=5
        ))
        for doc in docs:
            assert doc['id'], "Document should have id"
            assert doc['text'], "Document should have text"
            assert doc['pid'] == project_id, f"Document project_id mismatch"
    
    @staticmethod
    def _validate_chunk_embeddings(session: Any, project_id: str) -> None:
        """Validate chunk embeddings have correct dimensions."""
        chunks_with_embeddings = list(session.run(
            Neo4jQueries.VALIDATE_CHUNK_EMBEDDINGS,
            project_id=project_id
        ))
        if len(chunks_with_embeddings) > 0 and chunks_with_embeddings[0]['count'] > 0:
            dims = chunks_with_embeddings[0]['dims']
            if dims is not None:
                assert dims == 3072, f"Embeddings should be 3072 dimensions, got {dims}"
    
    @staticmethod
    def _validate_entities_and_relationships(session: Any, project_id: str) -> None:
        """Validate entity properties and relationships."""
        entities = list(session.run(
            Neo4jQueries.GET_ENTITY_PROPERTIES,
            project_id=project_id,
            limit=5
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
            Neo4jQueries.COUNT_ENTITY_RELATIONSHIPS,
            project_id=project_id
        ).single()
        if related_count and related_count['count'] > 0:
            # Verify RELATED relationships have required properties
            related_props = list(session.run(
                Neo4jQueries.GET_RELATIONSHIP_PROPERTIES,
                project_id=project_id,
                limit=3
            ))
            for rel in related_props:
                assert rel['description'], "RELATED should have description"
                assert rel['weight'] is not None, "RELATED should have weight"
                assert 0.0 <= rel['weight'] <= 1.0, \
                    f"RELATED weight should be in [0,1], got {rel['weight']}"
    
    @staticmethod
    def _validate_communities(session: Any, project_id: str) -> None:
        """Validate community detection and relationships."""
        communities = list(session.run(
            Neo4jQueries.GET_COMMUNITIES_BY_PROJECT,
            project_id=project_id,
            limit=5
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
                Neo4jQueries.COUNT_IN_COMMUNITY_RELATIONSHIPS,
                project_id=project_id
            ).single()
            assert in_community_count and in_community_count['count'] > 0, \
                "Entities should be assigned to communities via IN_COMMUNITY"
    
    @staticmethod
    def verify_constraints_minimal(fixtures: WorkflowTestFixtures) -> None:
        """
        Verify minimal required constraints exist.
        
        Args:
            fixtures: Test fixtures with neo4j_driver
        """
        with fixtures.neo4j_driver.session() as session:
            constraints = list(session.run(Neo4jQueries.SHOW_CONSTRAINTS))
            
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
        """
        expected = [
            {"label": "__Chunk__", "property": "embedding"},
            {"label": "__Community__", "property": "embedding"},
        ]
        expected_dims = 3072
        expected_sim = "cosine"
        
        with fixtures.neo4j_driver.session() as session:
            records = list(session.run(Neo4jQueries.SHOW_INDEXES))
            
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
            required_vector_names: Expected vector index names
            required_fulltext_names: Expected fulltext index names
            
        Raises:
            AssertionError: If any required indexes are missing or wrong type
        """
        with driver.session() as session:
            result = session.run(Neo4jQueries.SHOW_INDEXES)
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

