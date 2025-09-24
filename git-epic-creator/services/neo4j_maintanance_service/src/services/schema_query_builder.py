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
            (f"CREATE CONSTRAINT community_key_unique_underscored IF NOT EXISTS FOR (c:`{LABEL_COMMUNITY}`) "
             f"REQUIRE c.community IS UNIQUE"),
            ("CREATE CONSTRAINT related_id IF NOT EXISTS FOR ()-[rel:RELATED]->() REQUIRE rel.id IS UNIQUE"),
        ]

        logger.debug("Generated constraint queries", count=len(constraints))
        return constraints

    def get_index_queries(self) -> List[str]:
        """
        Generate Neo4j index queries for the Graph RAG schema.

        Returns:
            List[str]: List of Cypher index queries
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
            (f"CREATE VECTOR INDEX {env.CHUNK_VECTOR_INDEX_NAME} IF NOT EXISTS FOR (c:`{LABEL_CHUNK}`) "
             f"ON (c.{prop}) {vector_index_options}"),
            (f"CREATE VECTOR INDEX {env.COMMUNITY_VECTOR_INDEX_NAME} IF NOT EXISTS FOR (c:`{LABEL_COMMUNITY}`) "
             f"ON (c.{prop}) {vector_index_options}"),
            (f"CREATE VECTOR INDEX {env.ENTITY_VECTOR_INDEX_NAME} IF NOT EXISTS FOR (e:`{LABEL_ENTITY}`) "
             f"ON (e.{prop}) {vector_index_options}"),
            # Community support BTREE/FTS indexes
            (f"CREATE INDEX community_id IF NOT EXISTS FOR (c:`{LABEL_COMMUNITY}`) ON (c.id)"),
            (f"CREATE INDEX community_level IF NOT EXISTS FOR (c:`{LABEL_COMMUNITY}`) ON (c.level)"),
            (f"CREATE FULLTEXT INDEX community_summary_fts IF NOT EXISTS FOR (c:`{LABEL_COMMUNITY}`) ON EACH [c.summary]"),
            (f"CREATE INDEX entity_norm_title_index IF NOT EXISTS FOR (e:`{LABEL_ENTITY}`) ON (e.norm_title)"),
            (f"CREATE INDEX entity_description_index IF NOT EXISTS FOR (e:`{LABEL_ENTITY}`) ON (e.description)"),
            (f"CREATE INDEX chunk_text_index IF NOT EXISTS FOR (c:`{LABEL_CHUNK}`) ON (c.text)"),
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
            "vector_dimensions": 1536,
            "similarity_function": "cosine"
        }

        logger.debug("Generated schema info", components=list(schema_info.keys()))
        return schema_info
