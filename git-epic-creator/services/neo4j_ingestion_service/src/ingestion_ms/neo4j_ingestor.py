from __future__ import annotations

import time
from typing import Any, Dict, List

import httpx
import structlog

from .callbacks import IngestionWorkflowCallbacks
from .dtos import (
    ChunkEmbeddingRow,
    CommunityEmbeddingRow,
    EntityEmbeddingRow,
    RequirementsGraphBundleRequest,
)
from .neo4j_repository_service_client import post_json

logger = structlog.get_logger(__name__)


class Neo4jIngestor:
    """
    Ingest GraphRAG outputs into Neo4j via neo4j-repository-service (HTTP).

    This replaces direct Neo4j driver usage end-to-end for this service.
    """

    def __init__(
        self,
        client: httpx.Client,
        project_id: str | None = None,
        *,
        repo_auth_header: str | None = None,
    ) -> None:
        self._client = client
        self._project_id = project_id
        self._repo_auth_header = repo_auth_header

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
            data = post_json(self._client, path, payload, auth_header=self._repo_auth_header)
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

    def ingest_all_parquet(self, records: Dict[str, List[dict]], batch_size: int = 1000) -> Dict[str, Any]:
        """
        Collect all parquet-derived rows first and send them in one transactional request.

        Note: this is "option A" bundling of STRUCTURE ingestion only (documents/chunks/entities/relationships/communities).
        Vectors/backfills/cleanup remain separate endpoints.
        """
        if not self._project_id:
            raise ValueError("project_id is required for parquet bundle ingestion")

        documents = records.get("documents", [])
        chunks = records.get("chunks", [])
        entities = records.get("entities", [])
        entity_relationships = records.get("entity_relationships", [])
        community_reports = records.get("community_reports", [])
        communities = records.get("communities", [])

        # Normalize/validate all outgoing rows to a stable contract.
        # Unknown parquet columns are intentionally dropped.
        req = RequirementsGraphBundleRequest.model_validate(
            {
                "project_id": self._project_id,
                "documents": documents,
                "chunks": chunks,
                "entities": entities,
                "relationships": entity_relationships,
                "community_reports": community_reports,
                "communities": communities,
            }
        )
        data = post_json(
            self._client,
            "/v1/requirements-graph/merge/bundle",
            req.model_dump(mode="python"),
            auth_header=self._repo_auth_header,
        )

        # Maintain a compatible "counts by dataset" result shape for downstream logging.
        result: Dict[str, Any] = {
            "documents": {"created": int(data.get("documents_created", 0)), "input": len(documents)},
            "chunks": {"created": int(data.get("chunks_created", 0)), "input": len(chunks)},
            "entities": {"created": int(data.get("entities_created", 0)), "input": len(entities)},
            "entity_relationships": {"created": int(data.get("relationships_processed", 0)), "input": len(entity_relationships)},
            "community_reports": {"created": int(data.get("community_reports_created", 0)), "input": len(community_reports)},
            "communities": {"created": int(data.get("communities_created", 0)), "input": len(communities)},
            "bundle": {
                "documents_created": int(data.get("documents_created", 0)),
                "chunks_created": int(data.get("chunks_created", 0)),
                "entities_created": int(data.get("entities_created", 0)),
                "relationships_processed": int(data.get("relationships_processed", 0)),
                "community_reports_created": int(data.get("community_reports_created", 0)),
                "communities_created": int(data.get("communities_created", 0)),
            },
        }
        total_input = sum([len(documents), len(chunks), len(entities), len(entity_relationships), len(community_reports), len(communities)])
        total_created = sum([
            result["documents"]["created"],
            result["chunks"]["created"],
            result["entities"]["created"],
            result["entity_relationships"]["created"],
            result["community_reports"]["created"],
            result["communities"]["created"],
        ])
        success_rate = (total_created / total_input * 100.0) if total_input > 0 else 0.0
        logger.info(
            "parquet_bundle_ingestion_completed",
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

        def _normalize_embedding_rows(key: str, rows: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
            # Build a predictable payload shape per endpoint.
            # If a row cannot be validated, drop it (and rely on downstream validation).
            out: List[Dict[str, Any]] = []
            for r in rows:
                try:
                    if key == "chunk_text":
                        m = ChunkEmbeddingRow.model_validate(r)
                    elif key == "entity_description":
                        m = EntityEmbeddingRow.model_validate(r)
                    elif key == "community_summary":
                        m = CommunityEmbeddingRow.model_validate(r)
                    else:
                        continue
                    out.append(m.model_dump(mode="python"))
                except Exception:
                    continue
            return out

        out: Dict[str, int] = {}
        for key, (path, count_field) in mapping.items():
            rows = _normalize_embedding_rows(key, vectors.get(key, []))
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
                auth_header=self._repo_auth_header,
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
                auth_header=self._repo_auth_header,
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
            data = post_json(
                self._client,
                "/v1/requirements-graph/backfill/community-ids",
                {},
                auth_header=self._repo_auth_header,
            )
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
                auth_header=self._repo_auth_header,
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

            record = post_json(
                self._client,
                "/v1/requirements-graph/validate/embeddings",
                {"project_id": self._project_id},
                auth_header=self._repo_auth_header,
            )
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
        return post_json(
            self._client,
            "/v1/requirements-graph/validate/relationships",
            {"project_id": self._project_id},
            auth_header=self._repo_auth_header,
        )

    def cleanup_duplicate_relationships(self) -> int:
        if not self._project_id:
            raise ValueError("project_id is required for relationship cleanup")
        data = post_json(
            self._client,
            "/v1/requirements-graph/cleanup/duplicate-relationships",
            {"project_id": self._project_id},
            auth_header=self._repo_auth_header,
        )
        return int(data.get("total_duplicates_removed", 0))

    def detect_orphaned_nodes(self) -> Dict[str, Any]:
        if not self._project_id:
            raise ValueError("project_id is required for orphan detection")
        return post_json(
            self._client,
            "/v1/requirements-graph/detect/orphaned-nodes",
            {"project_id": self._project_id},
            auth_header=self._repo_auth_header,
        )

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
                auth_header=self._repo_auth_header,
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
                auth_header=self._repo_auth_header,
            )
            count = int(data.get("entities_updated", 0))
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to sync entity relationship IDs", error=err, project_id=self._project_id)
        finally:
            callbacks.backfill_end(step, ok, err)
        return count




