from __future__ import annotations

import time
from typing import Any, Dict, List

import httpx
import structlog

from .callbacks import IngestionWorkflowCallbacks
from .neo4j_repository_service_client import post_json

logger = structlog.get_logger(__name__)


class Neo4jIngestor:
    """
    Ingest GraphRAG outputs into Neo4j via neo4j-repository-service (HTTP).

    This replaces direct Neo4j driver usage end-to-end for this service.
    """

    def __init__(self, client: httpx.Client, project_id: str | None = None) -> None:
        self._client = client
        self._project_id = project_id

    # -----------------------
    # Generic batch HTTP runner
    # -----------------------
    def _batched_post_rows(
        self,
        *,
        path: str,
        rows: List[dict],
        batch_size: int = 1000,
        response_count_field: str = "processed",
        extra_payload: Dict[str, Any] | None = None,
    ) -> tuple[int, int]:
        total_input = len(rows)
        if total_input == 0:
            return (0, 0)

        total_count = 0
        for start in range(0, total_input, batch_size):
            batch = rows[start : min(start + batch_size, total_input)]
            batch_num = (start // batch_size) + 1
            total_batches = (total_input + batch_size - 1) // batch_size

            payload: Dict[str, Any] = {"rows": batch}
            if extra_payload:
                payload.update(extra_payload)

            batch_start = time.time()
            data = post_json(self._client, path, payload)
            batch_count = int(data.get(response_count_field, 0))
            total_count += batch_count

            batch_duration = time.time() - batch_start
            logger.info(
                "batch_completed",
                endpoint=path,
                batch=f"{batch_num}/{total_batches}",
                input_rows=len(batch),
                processed=batch_count,
                duration_ms=round(batch_duration * 1000, 2),
            )

        return (total_count, total_input)

    # -----------------------
    # Parquet ingestions
    # -----------------------
    def ingest_documents(self, rows: List[dict], batch_size: int = 1000) -> tuple[int, int]:
        return self._batched_post_rows(path="/v1/requirements-graph/merge/documents", rows=rows, batch_size=batch_size)

    def ingest_chunks(self, rows: List[dict], batch_size: int = 1000) -> tuple[int, int]:
        return self._batched_post_rows(path="/v1/requirements-graph/merge/chunks", rows=rows, batch_size=batch_size)

    def ingest_entities(self, rows: List[dict], batch_size: int = 1000) -> tuple[int, int]:
        return self._batched_post_rows(path="/v1/requirements-graph/merge/entities", rows=rows, batch_size=batch_size)

    def ingest_entity_relationships(self, rows: List[dict], batch_size: int = 1000) -> tuple[int, int]:
        return self._batched_post_rows(path="/v1/requirements-graph/merge/relationships", rows=rows, batch_size=batch_size)

    def ingest_community_reports(self, rows: List[dict], batch_size: int = 1000) -> tuple[int, int]:
        return self._batched_post_rows(
            path="/v1/requirements-graph/merge/community-reports", rows=rows, batch_size=batch_size
        )

    def ingest_communities(self, rows: List[dict], batch_size: int = 1000) -> tuple[int, int]:
        return self._batched_post_rows(path="/v1/requirements-graph/merge/communities", rows=rows, batch_size=batch_size)

    def ingest_all_parquet(self, records: Dict[str, List[dict]], batch_size: int = 1000) -> Dict[str, Any]:
        result: Dict[str, Any] = {}
        total_created = 0
        total_input = 0

        def _run(name: str, fn, rows: List[dict]) -> None:
            nonlocal total_created, total_input
            created, inp = fn(rows, batch_size=batch_size)
            result[name] = {"created": created, "input": inp}
            total_created += created
            total_input += inp

        _run("documents", self.ingest_documents, records.get("documents", []))
        _run("chunks", self.ingest_chunks, records.get("chunks", []))
        _run("entities", self.ingest_entities, records.get("entities", []))
        _run("entity_relationships", self.ingest_entity_relationships, records.get("entity_relationships", []))
        _run("community_reports", self.ingest_community_reports, records.get("community_reports", []))
        _run("communities", self.ingest_communities, records.get("communities", []))

        success_rate = (total_created / total_input * 100.0) if total_input > 0 else 0.0
        logger.info(
            "parquet_ingestion_completed",
            total_created=total_created,
            total_input=total_input,
            success_rate=round(success_rate, 2),
        )
        return result

    # -----------------------
    # Vector ingestions (scoped to project)
    # -----------------------
    def ingest_all_vectors(
        self,
        vectors: Dict[str, List[Dict[str, Any]]],
        callbacks: IngestionWorkflowCallbacks,
        batch_size: int = 1000,
    ) -> Dict[str, int]:
        if not self._project_id:
            logger.warning("No project_id set, skipping vectors ingestion")
            return {"community_summary": 0, "entity_description": 0, "chunk_text": 0}

        mapping = {
            "community_summary": ("/v1/requirements-graph/embeddings/community-full-content", "updated"),
            "entity_description": ("/v1/requirements-graph/embeddings/entity-description", "updated"),
            "chunk_text": ("/v1/requirements-graph/embeddings/chunk-text", "updated"),
        }

        out: Dict[str, int] = {}
        for key, (path, count_field) in mapping.items():
            rows = vectors.get(key, [])
            callbacks.vectors_ingest_start(key, "text", len(rows))
            updated, _ = self._batched_post_rows(
                path=path,
                rows=rows,
                batch_size=batch_size,
                response_count_field=count_field,
                extra_payload={"project_id": self._project_id},
            )
            out[key] = updated
        return out

    # -----------------------
    # Backfills
    # -----------------------
    def backfill_entity_relationship_ids(self, callbacks: IngestionWorkflowCallbacks) -> None:
        step = "backfill_entity_relationship_ids"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        try:
            if not self._project_id:
                ok = False
                err = "No project_id set"
                return
            post_json(
                self._client,
                "/v1/requirements-graph/backfill/entity-relationship-ids",
                {"project_id": self._project_id},
            )
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to backfill entity.relationship_ids", error=err, project_id=self._project_id)
        finally:
            callbacks.backfill_end(step, ok, err)

    def backfill_community_membership(self, callbacks: IngestionWorkflowCallbacks) -> None:
        step = "backfill_community_membership"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        try:
            if not self._project_id:
                ok = False
                err = "No project_id set"
                return
            post_json(
                self._client,
                "/v1/requirements-graph/backfill/community-membership",
                {"project_id": self._project_id},
            )
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to backfill community membership", error=err, project_id=self._project_id)
        finally:
            callbacks.backfill_end(step, ok, err)

    def backfill_community_ids(self, callbacks: IngestionWorkflowCallbacks) -> int:
        step = "backfill_community_ids"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        count = 0
        try:
            data = post_json(self._client, "/v1/requirements-graph/backfill/community-ids", {})
            count = int(data.get("communities_updated", 0))
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to backfill community IDs", error=err)
        finally:
            callbacks.backfill_end(step, ok, err)
        return count

    def backfill_community_hierarchy(self, callbacks: IngestionWorkflowCallbacks) -> None:
        step = "backfill_community_hierarchy"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        try:
            if not self._project_id:
                ok = False
                err = "No project_id set"
                return
            post_json(
                self._client,
                "/v1/requirements-graph/backfill/community-hierarchy",
                {"project_id": self._project_id},
            )
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to backfill community hierarchy", error=err, project_id=self._project_id)
        finally:
            callbacks.backfill_end(step, ok, err)

    # -----------------------
    # Validation and cleanup
    # -----------------------
    def validate_all_embeddings(self, callbacks: IngestionWorkflowCallbacks) -> Dict[str, Any]:
        step = "validate_all_embeddings"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None

        result_dict: Dict[str, Any] = {
            "has_critical_issues": False,
            "has_warnings": False,
            "issues": [],
            "suggestions": [],
        }

        try:
            if not self._project_id:
                ok = False
                err = "No project_id set"
                return result_dict

            record = post_json(self._client, "/v1/requirements-graph/validate/embeddings", {"project_id": self._project_id})
            result_dict.update(record)

            communities_missing = int(record.get("communities_missing_embedding", 0))
            chunks_missing = int(record.get("chunks_missing_embedding", 0))
            total_chunks = int(record.get("total_chunks", 0))

            if communities_missing > 0:
                result_dict["has_critical_issues"] = True
                result_dict["issues"].append(
                    f"CRITICAL: {communities_missing} communities missing embeddings - DRIFT search will fail"
                )
                result_dict["suggestions"].append("Re-run LanceDB ingestion or inspect embeddings tables")

            if total_chunks > 0 and chunks_missing > total_chunks * 0.1:
                result_dict["has_warnings"] = True
                pct = (chunks_missing / total_chunks * 100.0)
                result_dict["issues"].append(
                    f"WARNING: {chunks_missing} ({pct:.1f}%) chunks missing embeddings - DRIFT quality degraded"
                )
                result_dict["suggestions"].append("Check LanceDB table 'default-text_unit-text' completeness")

        except Exception as exc:
            ok = False
            err = str(exc)
            result_dict["has_critical_issues"] = True
            result_dict["issues"].append(f"Validation failed: {err}")
            logger.warning("Failed to validate embeddings", error=err, project_id=self._project_id)
        finally:
            callbacks.backfill_end(step, ok, err)

        return result_dict

    def validate_relationships(self) -> Dict[str, Any]:
        if not self._project_id:
            raise ValueError("project_id is required for relationship validation")
        return post_json(self._client, "/v1/requirements-graph/validate/relationships", {"project_id": self._project_id})

    def cleanup_duplicate_relationships(self) -> int:
        if not self._project_id:
            raise ValueError("project_id is required for relationship cleanup")
        data = post_json(
            self._client,
            "/v1/requirements-graph/cleanup/duplicate-relationships",
            {"project_id": self._project_id},
        )
        return int(data.get("total_duplicates_removed", 0))

    def detect_orphaned_nodes(self) -> Dict[str, Any]:
        if not self._project_id:
            raise ValueError("project_id is required for orphan detection")
        return post_json(self._client, "/v1/requirements-graph/detect/orphaned-nodes", {"project_id": self._project_id})

    def cleanup_orphaned_nodes(self, callbacks: IngestionWorkflowCallbacks) -> Dict[str, Any]:
        step = "cleanup_orphaned_nodes"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        stats: Dict[str, Any] = {}
        try:
            if not self._project_id:
                ok = False
                err = "No project_id set"
                return stats
            stats = post_json(
                self._client,
                "/v1/requirements-graph/cleanup/orphaned-nodes",
                {"project_id": self._project_id},
            )
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to cleanup orphaned nodes", error=err, project_id=self._project_id)
        finally:
            callbacks.backfill_end(step, ok, err)
        return stats

    def sync_entity_relationship_ids(self, callbacks: IngestionWorkflowCallbacks) -> int:
        step = "sync_entity_relationship_ids"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        count = 0
        try:
            if not self._project_id:
                ok = False
                err = "No project_id set"
                return 0
            data = post_json(
                self._client,
                "/v1/requirements-graph/sync/entity-relationship-ids",
                {"project_id": self._project_id},
            )
            count = int(data.get("entities_updated", 0))
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to sync entity relationship IDs", error=err, project_id=self._project_id)
        finally:
            callbacks.backfill_end(step, ok, err)
        return count


