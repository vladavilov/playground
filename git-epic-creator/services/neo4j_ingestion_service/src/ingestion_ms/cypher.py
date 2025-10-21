from pathlib import Path

_QUERIES_DIR = Path(__file__).parent / "queries"


def _load_query(filename: str) -> str:
    path = _QUERIES_DIR / filename
    return path.read_text(encoding="utf-8").strip()


def get_merge_document_query() -> str:
    return _load_query("merge_document.cypher")


def get_merge_chunk_query() -> str:
    return _load_query("merge_chunk.cypher")


def get_merge_entity_query() -> str:
    return _load_query("merge_entity.cypher")


def get_merge_community_report_query() -> str:
    return _load_query("merge_community_report.cypher")


def get_merge_community_query() -> str:
    return _load_query("merge_community.cypher")


def get_merge_relationship_query() -> str:
    return _load_query("merge_relationship.cypher")


def get_backfill_entity_rel_ids() -> str:
    return _load_query("backfill_entity_rel_ids.cypher")


def get_backfill_community_membership() -> str:
    return _load_query("backfill_community_membership.cypher")

def get_update_chunk_embeddings_query() -> str:
    return _load_query("update_chunk_embeddings.cypher")

def get_update_community_summary_embeddings_query() -> str:
    return _load_query("update_community_summary_embeddings.cypher")

def get_update_entity_description_embeddings_query() -> str:
    return _load_query("update_entity_description_embeddings.cypher")


def get_validate_relationships_query() -> str:
    return _load_query("validate_relationships.cypher")


def get_cleanup_duplicate_relationships_query() -> str:
    return _load_query("cleanup_duplicate_relationships.cypher")


__all__ = [
    "get_merge_document_query",
    "get_merge_chunk_query",
    "get_merge_entity_query",
    "get_merge_community_report_query",
    "get_merge_community_query",
    "get_merge_relationship_query",
    "get_backfill_entity_rel_ids",
    "get_backfill_community_membership",
    "get_update_chunk_embeddings_query",
    "get_update_community_summary_embeddings_query",
    "get_update_entity_description_embeddings_query",
    "get_validate_relationships_query",
    "get_cleanup_duplicate_relationships_query",
]
