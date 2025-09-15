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
    get_dedup_has_chunk_relationships,
)


from configuration.vector_index_config import get_vector_index_env
from .callbacks import IngestionWorkflowCallbacks


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

    def ingest_all_vectors(
        self,
        vectors: Dict[str, List[Dict[str, Any]]],
        callbacks: IngestionWorkflowCallbacks,
        batch_size: int = 1000,
    ) -> Dict[str, int]:
        result: Dict[str, int] = {}

        def _run(label: str, prop: str, key: str) -> int:
            rows = vectors.get(key, [])
            callbacks.vectors_ingest_start(label, prop, len(rows))
            count = self.ingest_vectors_for_label(label, prop, rows, batch_size)
            return count

        # Align with LanceDB table based on community full_content
        result["community_summary"] = _run("__Community__", "full_content", "community_summary")
        result["entity_description"] = _run("__Entity__", "description", "entity_description")
        result["chunk_text"] = _run("__Chunk__", "text", "chunk_text")
        return result

    # -----------------------
    # Backfills
    # -----------------------
    def backfill_entity_relationship_ids(self, callbacks: IngestionWorkflowCallbacks) -> None:
        step = "backfill_entity_relationship_ids"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        try:
            with self._driver.session() as session:
                session.run(get_backfill_entity_rel_ids())
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to backfill entity.relationship_ids", error=err)
        finally:
            callbacks.backfill_end(step, ok, err)

    def backfill_community_membership(self, callbacks: IngestionWorkflowCallbacks) -> None:
        step = "backfill_community_membership"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        try:
            with self._driver.session() as session:
                session.run(get_backfill_community_membership())
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to backfill community.entity_ids and IN_COMMUNITY edges", error=err)
        finally:
            callbacks.backfill_end(step, ok, err)

    def dedup_has_chunk_relationships(self, callbacks: IngestionWorkflowCallbacks) -> None:
        step = "dedup_has_chunk_relationships"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        try:
            with self._driver.session() as session:
                session.run(get_dedup_has_chunk_relationships())
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to deduplicate HAS_CHUNK relationships", error=err)
        finally:
            callbacks.backfill_end(step, ok, err)


