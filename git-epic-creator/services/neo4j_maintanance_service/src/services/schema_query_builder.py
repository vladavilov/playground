"""
Schema Query Builder Service

Generates Neo4j constraint and index queries for the Graph RAG schema.
"""

from typing import List, Dict, Any
import structlog

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
            ("CREATE CONSTRAINT entity_id_unique_underscored IF NOT EXISTS FOR (e:__Entity__) "
             "REQUIRE e.id IS UNIQUE"),
            ("CREATE CONSTRAINT document_id_unique_underscored IF NOT EXISTS FOR (d:__Document__) "
             "REQUIRE d.id IS UNIQUE"),
            ("CREATE CONSTRAINT community_id_unique_underscored IF NOT EXISTS FOR (c:__Community__) "
             "REQUIRE c.id IS UNIQUE"),
        ]

        logger.debug("Generated constraint queries", count=len(constraints))
        return constraints

    def get_index_queries(self) -> List[str]:
        """
        Generate Neo4j index queries for the Graph RAG schema.

        Returns:
            List[str]: List of Cypher index queries
        """
        # Keep options minimal to satisfy 5.x requirements and avoid invalid argument issues
        vector_index_options = (
            "OPTIONS {indexConfig: {"
            "`vector.dimensions`: 1536, "
            "`vector.similarity_function`: 'cosine'"  
            "}}"
        )

        indexes = [
            (f"CREATE VECTOR INDEX graphrag_chunk_index IF NOT EXISTS FOR (c:Chunk) "
             f"ON (c.embedding) {vector_index_options}"),
        ]

        logger.debug("Generated index queries", count=len(indexes))
        return indexes

    def get_all_queries(self) -> List[str]:
        """
        Get all schema queries (constraints + indexes + relationship types).

        Returns:
            List[str]: Combined list of all schema queries
        """
        constraints = self.get_constraint_queries()
        indexes = self.get_index_queries()
        relationship_types = self.get_relationship_type_queries()
        all_queries = constraints + indexes + relationship_types

        logger.debug(
            "Generated all queries",
            total=len(all_queries),
            constraints=len(constraints),
            indexes=len(indexes),
            relationship_types=len(relationship_types)
        )
        return all_queries

    def get_node_types(self) -> List[str]:
        """
        Get all node types defined in the schema.

        Returns:
            List[str]: List of node type names
        """
        node_types = ["__Entity__", "__Document__", "__Community__"]
        logger.debug("Retrieved node types", count=len(node_types))
        return node_types

    def get_relationship_types(self) -> List[str]:
        """
        Get all relationship types defined in the schema.

        Returns:
            List[str]: List of relationship type names
        """
        relationships = [
            "REFERENCED_BY", "EVIDENCED_BY", "MERGED_FROM", "RELATED_TO", "DESCRIBED_IN"
        ]
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
