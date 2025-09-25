"""
Shared constants for Neo4j maintenance service.

Centralizes labels and relationship types to avoid duplication across
schema builders and maintenance utilities.
"""

# Node labels
LABEL_ENTITY = "__Entity__"
LABEL_DOCUMENT = "__Document__"
LABEL_CHUNK = "__Chunk__"
LABEL_COMMUNITY = "__Community__"
LABEL_PROJECT = "__Project__"

# Relationship types used by the Graph RAG schema
RELATIONSHIP_TYPES = [
    "IN_PROJECT",
    "HAS_CHUNK",
    "HAS_ENTITY",
    "IN_COMMUNITY",
    "RELATED",
]


