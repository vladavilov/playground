import structlog
from typing import Any, Dict, List

from .cypher import (
    get_merge_document_query,
    get_merge_chunk_query,
    get_merge_entity_query,
    get_merge_community_report_query,
    get_merge_community_query,
    get_merge_relationship_query,
    get_backfill_entity_rel_ids,
    get_backfill_community_membership,
)


from configuration.vector_index_config import get_vector_index_env


logger = structlog.get_logger(__name__)


class Neo4jIngestor:
    """Handle all Neo4j ingestions and backfills.

    This class ingests pre-shaped rows from ParquetReader and LanceDBReader.
    """

    def __init__(self, driver: Any) -> None:
        self._driver = driver
        env = get_vector_index_env()
        self._embedding_property = env.VECTOR_INDEX_PROPERTY

    # -----------------------
    # Generic batch runner
    # -----------------------
    def _batched_run(self, statement: str, rows: List[dict], batch_size: int = 1000) -> int:
        total = len(rows)
        if total == 0:
            return 0
        with self._driver.session() as session:
            for start in range(0, total, batch_size):
                batch = rows[start : min(start + batch_size, total)]
                session.run(statement, rows=batch)
        return total

    # -----------------------
    # Parquet entity ingestions
    # -----------------------
    def ingest_documents(self, rows: List[dict], batch_size: int = 1000) -> int:
        return self._batched_run(get_merge_document_query(), rows, batch_size)

    def ingest_chunks(self, rows: List[dict], batch_size: int = 1000) -> int:
        return self._batched_run(get_merge_chunk_query(), rows, batch_size)

    def ingest_entities(self, rows: List[dict], batch_size: int = 1000) -> int:
        return self._batched_run(get_merge_entity_query(), rows, batch_size)

    def ingest_entity_relationships(self, rows: List[dict], batch_size: int = 1000) -> int:
        return self._batched_run(get_merge_relationship_query(), rows, batch_size)

    def ingest_community_reports(self, rows: List[dict], batch_size: int = 1000) -> int:
        return self._batched_run(get_merge_community_report_query(), rows, batch_size)

    def ingest_communities(self, rows: List[dict], batch_size: int = 1000) -> int:
        return self._batched_run(get_merge_community_query(), rows, batch_size)

    def ingest_all_parquet(self, records: Dict[str, List[dict]], batch_size: int = 1000) -> Dict[str, int]:
        return {
            "documents": self.ingest_documents(records.get("documents", []), batch_size),
            "chunks": self.ingest_chunks(records.get("chunks", []), batch_size),
            "entities": self.ingest_entities(records.get("entities", []), batch_size),
            "entity_relationships": self.ingest_entity_relationships(records.get("entity_relationships", []), batch_size),
            "community_reports": self.ingest_community_reports(records.get("community_reports", []), batch_size),
            "communities": self.ingest_communities(records.get("communities", []), batch_size),
        }

    # -----------------------
    # Vector ingestions
    # -----------------------
    def _build_update_embeddings_statement(self, node_label: str, text_property: str) -> str:
        return (
            "UNWIND $rows AS r\n"
            f"MATCH (n:`{node_label}` {{{text_property}: r.text}})\n"
            f"SET n.{self._embedding_property} = r.embedding\n"
            "RETURN 0\n"
        )

    def ingest_vectors_for_label(
        self,
        label: str,
        text_property: str,
        rows: List[Dict[str, Any]],
        batch_size: int = 1000,
    ) -> int:
        statement = self._build_update_embeddings_statement(label, text_property)
        return self._batched_run(statement, rows, batch_size)

    def ingest_all_vectors(self, vectors: Dict[str, List[Dict[str, Any]]], batch_size: int = 1000) -> Dict[str, int]:
        return {
            "community_summary": self.ingest_vectors_for_label("__Community__", "summary", vectors.get("community_summary", []), batch_size),
            "entity_description": self.ingest_vectors_for_label("__Entity__", "description", vectors.get("entity_description", []), batch_size),
            "chunk_text": self.ingest_vectors_for_label("__Chunk__", "text", vectors.get("chunk_text", []), batch_size),
        }

    # -----------------------
    # Backfills
    # -----------------------
    def backfill_entity_relationship_ids(self) -> None:
        try:
            with self._driver.session() as session:
                session.run(get_backfill_entity_rel_ids())
        except Exception as exc:
            logger.warning("Failed to backfill entity.relationship_ids", error=str(exc))

    def backfill_community_membership(self) -> None:
        try:
            with self._driver.session() as session:
                session.run(get_backfill_community_membership())
        except Exception as exc:
            logger.warning("Failed to backfill community.entity_ids and IN_COMMUNITY edges", error=str(exc))


