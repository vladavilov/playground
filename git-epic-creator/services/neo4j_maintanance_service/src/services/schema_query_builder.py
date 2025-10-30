"""
Schema Query Builder Service

Generates Neo4j constraint and index queries for the Graph RAG schema.
"""

from typing import List, Dict, Any
import structlog
from .constants import (
    LABEL_ENTITY,
    LABEL_DOCUMENT,
    LABEL_CHUNK,
    LABEL_COMMUNITY,
    LABEL_PROJECT,
    RELATIONSHIP_TYPES,
)
from configuration.vector_index_config import get_vector_index_env

logger = structlog.get_logger(__name__)


class SchemaQueryBuilder:
    """Builds Neo4j schema queries."""

    def __init__(self):
        """Initialize the query builder."""
        logger.info("Schema query builder initialized")

    def get_constraint_queries(self) -> List[str]:
        """
        Generate Neo4j constraint queries for the Graph RAG schema.

        Returns:
            List[str]: List of Cypher constraint queries
        """
        constraints = [
            (f"CREATE CONSTRAINT entity_id_unique_underscored IF NOT EXISTS FOR (e:`{LABEL_ENTITY}`) "
             f"REQUIRE e.id IS UNIQUE"),
            (f"CREATE CONSTRAINT document_id_unique_underscored IF NOT EXISTS FOR (d:`{LABEL_DOCUMENT}`) "
             f"REQUIRE d.id IS UNIQUE"),
            (f"CREATE CONSTRAINT chunk_id_unique_underscored IF NOT EXISTS FOR (c:`{LABEL_CHUNK}`) "
             f"REQUIRE c.id IS UNIQUE"),
            (f"CREATE CONSTRAINT project_id_unique_underscored IF NOT EXISTS FOR (p:`{LABEL_PROJECT}`) "
             f"REQUIRE p.id IS UNIQUE"),
            # Composite uniqueness scoped by project to avoid cross-project clashes
            (f"CREATE CONSTRAINT community_key_unique_underscored IF NOT EXISTS FOR (c:`{LABEL_COMMUNITY}`) "
             f"REQUIRE (c.project_id, c.community) IS UNIQUE"),
            ("CREATE CONSTRAINT related_id IF NOT EXISTS FOR ()-[rel:RELATED]->() REQUIRE rel.id IS UNIQUE"),
        ]

        logger.debug("Generated constraint queries", count=len(constraints))
        return constraints

    def get_index_queries(self) -> List[str]:
        """
        Generate Neo4j index queries for the Graph RAG schema.
        
        Index Categories:
        
        1. **Vector Indexes** (Read-Side):
           - Purpose: Semantic similarity search in DRIFT algorithm
           - Usage: db.index.vector.queryNodes() calls in retrieval service
           - Required: Yes (DRIFT search fails without them)
        
        2. **Community Indexes** (Read-Side):
           - Purpose: Fast community lookups and hierarchical filtering
           - Usage: fetch_community_summaries, vector_query_communities_by_level
           - Required: Yes for performance (queries slow without them)
        
        3. **Entity Indexes** (Write-Side):
           - Purpose: Entity deduplication during GraphRAG ingestion
           - Usage: merge_entity.cypher OPTIONAL MATCH lookups
           - Required: Yes for merge performance (ingestion 10x slower without)
        
        4. **Chunk/Document Indexes** (Write-Side):
           - Purpose: Deduplication and relationship traversal
           - Usage: merge_chunk.cypher, merge_document.cypher
           - Required: Recommended (improves ingestion performance)
        
        Performance Notes:
        - Write-side indexes: Trade write speed for merge correctness
        - Read-side indexes: Essential for query performance (100x+ speedup)
        - chunk_text_index: May cause issues on very large texts (>10KB)
        
        Returns:
            List[str]: List of Cypher index creation queries
        """
        env = get_vector_index_env()
        prop = env.VECTOR_INDEX_PROPERTY
        dims = int(env.VECTOR_INDEX_DIMENSIONS)
        sim = env.VECTOR_INDEX_SIMILARITY
        # Keep options minimal to satisfy 5.x requirements and avoid invalid argument issues
        vector_index_options = (
            f"OPTIONS {{indexConfig: {{`vector.dimensions`: {dims}, `vector.similarity_function`: '{sim}'}}}}"
        )

        indexes = [
            # ============================================================================
            # VECTOR INDEXES (Read-Side: DRIFT Search)
            # ============================================================================
            # Used by: db.index.vector.queryNodes() in DRIFT primer and follow-up phases
            # Purpose: Semantic similarity search for retrieval
            # Dimensions: 3072 (text-embedding-3-small), Similarity: cosine
            (f"CREATE VECTOR INDEX {env.CHUNK_VECTOR_INDEX_NAME} IF NOT EXISTS FOR (c:`{LABEL_CHUNK}`) "
             f"ON (c.{prop}) {vector_index_options}"),
            (f"CREATE VECTOR INDEX {env.COMMUNITY_VECTOR_INDEX_NAME} IF NOT EXISTS FOR (c:`{LABEL_COMMUNITY}`) "
             f"ON (c.{prop}) {vector_index_options}"),
            (f"CREATE VECTOR INDEX {env.ENTITY_VECTOR_INDEX_NAME} IF NOT EXISTS FOR (e:`{LABEL_ENTITY}`) "
             f"ON (e.{prop}) {vector_index_options}"),
            
            # ============================================================================
            # COMMUNITY INDEXES (Read-Side: DRIFT Search)
            # ============================================================================
            # community_composite_key: Composite index for (project_id, community) lookups
            # Used by: fetch_community_summaries, fetch_communities_brief
            # Purpose: Efficient filtering on composite unique constraint
            (f"CREATE INDEX community_composite_key IF NOT EXISTS FOR (c:`{LABEL_COMMUNITY}`) "
             f"ON (c.project_id, c.community)"),
            
            # community_level: Hierarchical search filtering
            # Used by: vector_query_communities_by_level (DRIFT primer phase)
            # Purpose: Filter communities by hierarchy level (0=leaf, N=aggregate)
            (f"CREATE INDEX community_level IF NOT EXISTS FOR (c:`{LABEL_COMMUNITY}`) ON (c.level)"),
            
            # community_summary_fts: Full-text search on summaries (future feature)
            # Currently unused - reserved for keyword search fallback
            (f"CREATE FULLTEXT INDEX community_summary_fts IF NOT EXISTS FOR (c:`{LABEL_COMMUNITY}`) ON EACH [c.summary]"),
            
            # ============================================================================
            # ENTITY INDEXES (Write-Side: Merge Deduplication)
            # ============================================================================
            # Used by: merge_entity.cypher for finding existing entities by various properties
            # Purpose: Entity deduplication during GraphRAG ingestion
            # - norm_title: Case-insensitive title matching (primary dedup key)
            # - title: Exact title matching (secondary dedup key)
            # - description: Content-based matching (tertiary dedup key)
            # Performance: Critical for merge performance (OPTIONAL MATCH lookups)
            (f"CREATE INDEX entity_norm_title_index IF NOT EXISTS FOR (e:`{LABEL_ENTITY}`) ON (e.norm_title)"),
            (f"CREATE INDEX entity_title_index IF NOT EXISTS FOR (e:`{LABEL_ENTITY}`) ON (e.title)"),
            (f"CREATE INDEX entity_description_index IF NOT EXISTS FOR (e:`{LABEL_ENTITY}`) ON (e.description)"),
            
            # ============================================================================
            # CHUNK INDEXES (Write-Side: Deduplication)
            # ============================================================================
            # chunk_text_hash: Hash-based deduplication during ingestion (optimized)
            # Used by: merge_chunk.cypher for efficient text-based deduplication
            # Uses SHA256 hash substring (16 chars) for fast lookups without large text indexes
            # Performance: 10-100x faster than full text matching on large chunks
            (f"CREATE INDEX chunk_text_hash_index IF NOT EXISTS FOR (c:`{LABEL_CHUNK}`) ON (c.text_hash)"),
            
            # ============================================================================
            # DOCUMENT INDEXES (Mixed: Ingestion + Citations)
            # ============================================================================
            # document_title: Used for:
            #   1. Write-side: Document deduplication (if needed)
            #   2. Read-side: Citation retrieval (expand_neighborhood_minimal line 124)
            # Note: Currently only used via relationship traversal in read queries,
            #       but indexed for potential future direct lookups
            (f"CREATE INDEX document_title_index IF NOT EXISTS FOR (d:`{LABEL_DOCUMENT}`) ON (d.title)"),
        ]

        logger.debug("Generated index queries", count=len(indexes))
        return indexes

    def get_node_types(self) -> List[str]:
        """
        Get all node types defined in the schema.

        Returns:
            List[str]: List of node type names
        """
        node_types = [LABEL_ENTITY, LABEL_DOCUMENT, LABEL_CHUNK, LABEL_COMMUNITY, LABEL_PROJECT]
        logger.debug("Retrieved node types", count=len(node_types))
        return node_types

    def get_relationship_types(self) -> List[str]:
        """
        Get all relationship types defined in the schema.

        Returns:
            List[str]: List of relationship type names
        """
        relationships = RELATIONSHIP_TYPES
        logger.debug("Retrieved relationship types", count=len(relationships))
        return relationships

    def get_relationship_type_queries(self) -> List[str]:
        """
        Generate Neo4j relationship type creation queries for the Graph RAG schema.

        Returns:
            List[str]: List of Cypher relationship type creation queries
        """
        relationship_types = self.get_relationship_types()
        queries = [
            f"CALL db.createRelationshipType('{rel_type}')"
            for rel_type in relationship_types
        ]

        logger.debug("Generated relationship type queries", count=len(queries))
        return queries

    def get_schema_info(self) -> Dict[str, Any]:
        """
        Get comprehensive schema information.

        Returns:
            Dict[str, Any]: Schema information including constraints, indexes, types, etc.
        """
        schema_info = {
            "constraints": self.get_constraint_queries(),
            "indexes": self.get_index_queries(),
            "relationship_type_queries": self.get_relationship_type_queries(),
            "node_types": self.get_node_types(),
            "relationships": self.get_relationship_types(),
            "vector_dimensions": 3072,
            "similarity_function": "cosine"
        }

        logger.debug("Generated schema info", components=list(schema_info.keys()))
        return schema_info
