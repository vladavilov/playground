"""
Neo4j cypher queries for e2e tests.

This module contains reusable cypher query constants and query builders,
following DRY principles to avoid query duplication across test files.
"""

from typing import Dict, Any


class Neo4jQueries:
    """Centralized repository of Neo4j cypher queries."""
    
    # ===== Database Management Queries =====
    
    DELETE_ALL_NODES = "MATCH (n) DETACH DELETE n"
    """Remove all nodes and relationships from database."""
    
    RESET_SCHEMA = "CALL apoc.schema.assert({}, {})"
    """Drop all constraints and indexes using APOC."""
    
    # ===== Index and Constraint Queries =====
    
    SHOW_INDEXES = """
        SHOW INDEXES
        YIELD name, state, type, entityType, labelsOrTypes, properties, options
        RETURN name, state, type, entityType, labelsOrTypes, properties, options
    """
    """List all indexes with detailed information."""
    
    SHOW_CONSTRAINTS = """
        SHOW CONSTRAINTS
        YIELD name, entityType, labelsOrTypes, properties, type
        RETURN name, entityType, labelsOrTypes, properties, type
    """
    """List all constraints with detailed information."""
    
    SHOW_TRANSACTIONS = """
        SHOW TRANSACTIONS
        YIELD currentQuery
        RETURN currentQuery
    """
    """List active transactions and their queries."""
    
    # ===== Project and Document Queries =====
    
    FIND_PROJECT_BY_ID = """
        MATCH (p:__Project__ {id: $project_id})
        RETURN p
    """
    """Find project node by ID."""
    
    COUNT_DOCUMENTS_BY_PROJECT = """
        MATCH (d:__Document__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
        RETURN count(d) AS c
    """
    """Count documents linked to a project."""
    
    GET_DOCUMENT_PROPERTIES = """
        MATCH (d:__Document__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
        RETURN d.id AS id, d.title AS title, d.text AS text, d.project_id AS pid
        LIMIT $limit
    """
    """Retrieve document properties for validation."""
    
    # ===== Chunk Queries =====
    
    COUNT_CHUNKS_BY_PROJECT = """
        MATCH (d:__Document__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
        MATCH (d)-[:HAS_CHUNK]->(c:__Chunk__)
        RETURN count(DISTINCT c) AS c
    """
    """Count chunks linked to project documents."""
    
    VALIDATE_CHUNK_EMBEDDINGS = """
        MATCH (ch:__Chunk__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
        WHERE ch.embedding IS NOT NULL
        WITH count(ch) AS count, head(collect(ch.embedding)) AS sample_emb
        RETURN count, size(sample_emb) AS dims
    """
    """Validate chunk embeddings exist and have correct dimensions."""
    
    # ===== Entity Queries =====
    
    COUNT_ENTITIES_BY_PROJECT = """
        MATCH (ch:__Chunk__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
        MATCH (ch)-[:HAS_ENTITY]->(e:__Entity__)
        RETURN count(e) AS c
    """
    """Count entities linked to project chunks."""
    
    GET_ENTITY_PROPERTIES = """
        MATCH (e:__Entity__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
        RETURN e.id AS id, e.title AS title, e.description AS description,
               e.type AS type, e.norm_title AS norm_title, e.project_id AS pid
        LIMIT $limit
    """
    """Retrieve entity properties for validation."""
    
    COUNT_ENTITY_RELATIONSHIPS = """
        MATCH (e1:__Entity__)-[r:RELATED]->(e2:__Entity__)
        WHERE (e1)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
        RETURN count(r) AS count
    """
    """Count RELATED relationships between entities in a project."""
    
    GET_RELATIONSHIP_PROPERTIES = """
        MATCH (e1:__Entity__)-[r:RELATED]->(e2:__Entity__)
        WHERE (e1)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
        RETURN r.description AS description, r.weight AS weight, r.id AS id
        LIMIT $limit
    """
    """Retrieve relationship properties for validation."""
    
    # ===== Community Queries =====
    
    GET_COMMUNITIES_BY_PROJECT = """
        MATCH (c:__Community__)-[:IN_PROJECT]->(:__Project__ {id: $project_id})
        RETURN c.community AS id, c.level AS level, c.summary AS summary,
               c.title AS title, c.project_id AS pid
        LIMIT $limit
    """
    """Retrieve community properties for validation."""
    
    COUNT_IN_COMMUNITY_RELATIONSHIPS = """
        MATCH (:__Entity__)-[:IN_COMMUNITY]->(c:__Community__)
        -[:IN_PROJECT]->(:__Project__ {id: $project_id})
        RETURN count(*) AS count
    """
    """Count IN_COMMUNITY relationships for a project."""
    
    # ===== Cleanup Queries =====
    
    DELETE_PROJECT_NODES = """
        MATCH (n) WHERE n.project_id = $project_id
        DETACH DELETE n
    """
    """Delete all nodes related to a specific project."""
    
    @staticmethod
    def build_parameterized_query(base_query: str, params: Dict[str, Any]) -> tuple[str, Dict[str, Any]]:
        """
        Helper to validate and return parameterized query.
        
        Args:
            base_query: Base cypher query string
            params: Query parameters
            
        Returns:
            Tuple of (query, params)
        """
        return base_query, params

