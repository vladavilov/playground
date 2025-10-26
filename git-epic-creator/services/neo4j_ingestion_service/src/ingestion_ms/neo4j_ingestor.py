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
    get_backfill_community_hierarchy,
    get_backfill_community_ids,
    get_update_community_embedding,
    get_sync_entity_relationship_ids,
    get_validate_embeddings_query,
    get_validate_relationships_query,
    get_cleanup_duplicate_relationships_query,
    get_cleanup_orphaned_nodes_query,
    get_detect_orphaned_nodes_query,
)


from configuration.vector_index_config import get_vector_index_env
from .callbacks import IngestionWorkflowCallbacks


logger = structlog.get_logger(__name__)


class Neo4jIngestor:
    """Handle all Neo4j ingestions and backfills.

    This class ingests pre-shaped rows from ParquetReader and LanceDBReader.
    """

    def __init__(self, driver: Any, project_id: str | None = None) -> None:
        self._driver = driver
        self._project_id = project_id
        env = get_vector_index_env()
        self._embedding_property = env.VECTOR_INDEX_PROPERTY

    # -----------------------
    # Generic batch runner
    # -----------------------
    def _batched_run(self, statement: str, rows: List[dict], batch_size: int = 1000) -> tuple[int, int]:
        """
        Execute Cypher statement in batches with error handling and accurate node counting.
        
        Args:
            statement: Cypher query that must return count as first column
            rows: Input rows to process
            batch_size: Number of rows per batch
            
        Returns:
            Tuple of (nodes_created, total_input_rows)
            
        Raises:
            Exception: Re-raises database errors after logging
        """
        total_input = len(rows)
        if total_input == 0:
            return (0, 0)
        
        nodes_created = 0
        batches_processed = 0
        
        try:
            with self._driver.session() as session:
                for start in range(0, total_input, batch_size):
                    batch = rows[start : min(start + batch_size, total_input)]
                    batch_num = (start // batch_size) + 1
                    total_batches = (total_input + batch_size - 1) // batch_size
                    
                    try:
                        import time
                        batch_start = time.time()
                        
                        result = session.run(statement, rows=batch)
                        record = result.single()
                        
                        # Extract count from first column of result (handles different return names)
                        batch_count = 0
                        if record:
                            # Get first value regardless of column name
                            batch_count = record[0] if record else 0
                        
                        nodes_created += batch_count
                        batches_processed += 1
                        
                        batch_duration = time.time() - batch_start
                        
                        logger.info(
                            "batch_completed",
                            batch=f"{batch_num}/{total_batches}",
                            input_rows=len(batch),
                            nodes_created=batch_count,
                            duration_ms=round(batch_duration * 1000, 2),
                        )
                        
                        # Warn if batch created fewer nodes than input rows (may indicate skips or failures)
                        if batch_count < len(batch):
                            logger.warning(
                                "batch_partial_success",
                                batch=f"{batch_num}/{total_batches}",
                                input_rows=len(batch),
                                nodes_created=batch_count,
                                missing=len(batch) - batch_count,
                                message="Batch created fewer nodes than input rows - some may have been skipped or merged"
                            )
                    
                    except Exception as batch_exc:
                        logger.error(
                            "batch_failed",
                            batch=f"{batch_num}/{total_batches}",
                            input_rows=len(batch),
                            error=str(batch_exc),
                            error_type=type(batch_exc).__name__,
                        )
                        raise
                        
        except Exception as exc:
            logger.error(
                "batched_run_failed",
                total_input=total_input,
                batches_processed=batches_processed,
                nodes_created=nodes_created,
                error=str(exc),
                error_type=type(exc).__name__,
            )
            raise
        
        return (nodes_created, total_input)

    # -----------------------
    # Parquet entity ingestions
    # -----------------------
    def _attach_project_id(self, rows: List[dict]) -> List[dict]:
        if not self._project_id:
            return rows
        pid = self._project_id
        return [{**r, "project_id": pid} for r in rows]

    def ingest_documents(self, rows: List[dict], batch_size: int = 1000) -> tuple[int, int]:
        """Ingest documents. Returns (nodes_created, total_input)."""
        nodes_created, total_input = self._batched_run(
            get_merge_document_query(), 
            self._attach_project_id(rows), 
            batch_size
        )
        logger.info("documents_ingestion_completed", nodes_created=nodes_created, total_input=total_input)
        if nodes_created != total_input:
            logger.warning(
                "documents_ingestion_mismatch",
                nodes_created=nodes_created,
                total_input=total_input,
                missing=total_input - nodes_created,
            )
        return (nodes_created, total_input)

    def ingest_chunks(self, rows: List[dict], batch_size: int = 1000) -> tuple[int, int]:
        """Ingest chunks. Returns (nodes_created, total_input)."""
        nodes_created, total_input = self._batched_run(
            get_merge_chunk_query(), 
            self._attach_project_id(rows), 
            batch_size
        )
        logger.info("chunks_ingestion_completed", nodes_created=nodes_created, total_input=total_input)
        if nodes_created != total_input:
            logger.warning(
                "chunks_ingestion_mismatch",
                nodes_created=nodes_created,
                total_input=total_input,
                missing=total_input - nodes_created,
            )
        return (nodes_created, total_input)

    def ingest_entities(self, rows: List[dict], batch_size: int = 1000) -> tuple[int, int]:
        """Ingest entities. Returns (nodes_created, total_input)."""
        nodes_created, total_input = self._batched_run(
            get_merge_entity_query(), 
            self._attach_project_id(rows), 
            batch_size
        )
        logger.info("entities_ingestion_completed", nodes_created=nodes_created, total_input=total_input)
        if nodes_created != total_input:
            logger.warning(
                "entities_ingestion_mismatch",
                nodes_created=nodes_created,
                total_input=total_input,
                missing=total_input - nodes_created,
            )
        return (nodes_created, total_input)

    def ingest_entity_relationships(self, rows: List[dict], batch_size: int = 1000) -> tuple[int, int]:
        """Ingest entity relationships. Returns (relationships_created, total_input)."""
        rels_created, total_input = self._batched_run(
            get_merge_relationship_query(), 
            self._attach_project_id(rows), 
            batch_size
        )
        logger.info("relationships_ingestion_completed", rels_created=rels_created, total_input=total_input)
        if rels_created != total_input:
            logger.warning(
                "relationships_ingestion_mismatch",
                rels_created=rels_created,
                total_input=total_input,
                missing=total_input - rels_created,
            )
        return (rels_created, total_input)

    def ingest_community_reports(self, rows: List[dict], batch_size: int = 1000) -> tuple[int, int]:
        """Ingest community reports. Returns (nodes_created, total_input)."""
        nodes_created, total_input = self._batched_run(
            get_merge_community_report_query(), 
            self._attach_project_id(rows), 
            batch_size
        )
        logger.info("community_reports_ingestion_completed", nodes_created=nodes_created, total_input=total_input)
        if nodes_created != total_input:
            logger.warning(
                "community_reports_ingestion_mismatch",
                nodes_created=nodes_created,
                total_input=total_input,
                missing=total_input - nodes_created,
            )
        return (nodes_created, total_input)

    def ingest_communities(self, rows: List[dict], batch_size: int = 1000) -> tuple[int, int]:
        """Ingest communities. Returns (nodes_created, total_input)."""
        nodes_created, total_input = self._batched_run(
            get_merge_community_query(), 
            self._attach_project_id(rows), 
            batch_size
        )
        logger.info("communities_ingestion_completed", nodes_created=nodes_created, total_input=total_input)
        if nodes_created != total_input:
            logger.warning(
                "communities_ingestion_mismatch",
                nodes_created=nodes_created,
                total_input=total_input,
                missing=total_input - nodes_created,
            )
        return (nodes_created, total_input)

    def ingest_all_parquet(self, records: Dict[str, List[dict]], batch_size: int = 1000) -> Dict[str, Any]:
        """
        Ingest all parquet records with detailed metrics.
        
        Returns:
            Dict with format:
            {
                "documents": {"created": int, "input": int},
                "chunks": {"created": int, "input": int},
                ...
                "summary": {
                    "total_created": int,
                    "total_input": int,
                    "success_rate": float
                }
            }
        """
        doc_created, doc_input = self.ingest_documents(records.get("documents", []), batch_size)
        chunk_created, chunk_input = self.ingest_chunks(records.get("chunks", []), batch_size)
        entity_created, entity_input = self.ingest_entities(records.get("entities", []), batch_size)
        rel_created, rel_input = self.ingest_entity_relationships(records.get("entity_relationships", []), batch_size)
        report_created, report_input = self.ingest_community_reports(records.get("community_reports", []), batch_size)
        comm_created, comm_input = self.ingest_communities(records.get("communities", []), batch_size)
        
        total_created = doc_created + chunk_created + entity_created + rel_created + report_created + comm_created
        total_input = doc_input + chunk_input + entity_input + rel_input + report_input + comm_input
        success_rate = (total_created / total_input * 100.0) if total_input > 0 else 0.0
        
        result = {
            "documents": {"created": doc_created, "input": doc_input},
            "chunks": {"created": chunk_created, "input": chunk_input},
            "entities": {"created": entity_created, "input": entity_input},
            "entity_relationships": {"created": rel_created, "input": rel_input},
            "community_reports": {"created": report_created, "input": report_input},
            "communities": {"created": comm_created, "input": comm_input},
            "summary": {
                "total_created": total_created,
                "total_input": total_input,
                "success_rate": round(success_rate, 2),
            }
        }
        
        logger.info(
            "parquet_ingestion_completed",
            total_created=total_created,
            total_input=total_input,
            success_rate=round(success_rate, 2),
            documents=result["documents"],
            chunks=result["chunks"],
            entities=result["entities"],
        )
        
        return result

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
        """Backfill entity.relationship_ids for current project."""
        step = "backfill_entity_relationship_ids"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        try:
            if not self._project_id:
                logger.warning("No project_id set, skipping entity relationship IDs backfill")
                ok = False
                err = "No project_id set"
                return
                
            with self._driver.session() as session:
                session.run(get_backfill_entity_rel_ids(), project_id=self._project_id)
                logger.info("Backfilled entity relationship IDs", project_id=self._project_id)
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to backfill entity.relationship_ids", error=err, project_id=self._project_id)
        finally:
            callbacks.backfill_end(step, ok, err)

    def backfill_community_membership(self, callbacks: IngestionWorkflowCallbacks) -> None:
        """Backfill community.entity_ids and IN_COMMUNITY relationships for current project."""
        step = "backfill_community_membership"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        try:
            if not self._project_id:
                logger.warning("No project_id set, skipping community membership backfill")
                ok = False
                err = "No project_id set"
                return
                
            with self._driver.session() as session:
                result = session.run(get_backfill_community_membership(), project_id=self._project_id)
                record = result.single()
                count = record.get("communities_processed", 0) if record else 0
                logger.info("Backfilled community membership", project_id=self._project_id, communities_processed=count)
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to backfill community.entity_ids and IN_COMMUNITY edges", error=err, project_id=self._project_id)
        finally:
            callbacks.backfill_end(step, ok, err)

    def backfill_community_hierarchy(self, callbacks: IngestionWorkflowCallbacks) -> None:
        """Backfill hierarchical IN_COMMUNITY relationships between communities."""
        step = "backfill_community_hierarchy"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        count = 0
        try:
            if not self._project_id:
                logger.warning("No project_id set, skipping community hierarchy backfill")
                ok = False
                err = "No project_id set"
                return
            with self._driver.session() as session:
                result = session.run(get_backfill_community_hierarchy(), project_id=self._project_id)
                record = result.single()
                if record:
                    count = record.get("hierarchy_links_created", 0)
                    logger.info("Created community hierarchy links", project_id=self._project_id, count=count)
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to backfill community hierarchy", error=err, project_id=self._project_id)
        finally:
            callbacks.backfill_end(step, ok, err)

    def update_community_embedding_by_id(
        self, 
        community_id: int, 
        project_id: str, 
        embedding: List[float]
    ) -> bool:
        """
        Update embedding for a specific community by ID.
        
        Use cases:
        - Fix missing embeddings after validation
        - Re-generate embeddings for specific communities
        - Manual embedding updates during debugging
        
        Args:
            community_id: Community number (not composite ID)
            project_id: Project ID
            embedding: Vector embedding (should match VECTOR_INDEX_DIMENSIONS)
            
        Returns:
            True if update succeeded, False otherwise
        """
        try:
            with self._driver.session() as session:
                result = session.run(
                    get_update_community_embedding(),
                    community_id=community_id,
                    project_id=project_id,
                    embedding=embedding
                )
                record = result.single()
                if record and record.get("updated_id"):
                    logger.info(
                        "Updated community embedding",
                        community_id=community_id,
                        project_id=project_id,
                        embedding_dim=len(embedding)
                    )
                    return True
                else:
                    logger.warning(
                        "Community not found for embedding update",
                        community_id=community_id,
                        project_id=project_id
                    )
                    return False
        except Exception as exc:
            logger.error(
                "Failed to update community embedding",
                community_id=community_id,
                project_id=project_id,
                error=str(exc)
            )
            return False

    def backfill_community_ids(self, callbacks: IngestionWorkflowCallbacks) -> int:
        """Backfill community.id property for existing communities."""
        step = "backfill_community_ids"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        count = 0
        
        try:
            with self._driver.session() as session:
                result = session.run(get_backfill_community_ids())
                record = result.single()
                if record:
                    count = record.get("communities_updated", 0)
                    logger.info("Backfilled community IDs", count=count)
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to backfill community IDs", error=err)
        finally:
            callbacks.backfill_end(step, ok, err)
            
        return count

    def validate_all_embeddings(self, callbacks: IngestionWorkflowCallbacks) -> Dict[str, Any]:
        """
        Comprehensive embedding validation across all node types.
        
        Validates:
        - Chunks: Should have embeddings for DRIFT follow-up phase
        - Entities: Optional but improves search quality
        - Communities: CRITICAL for DRIFT primer phase
        
        Returns:
            Dict with counts, coverage metrics, and actionable flags:
            - has_critical_issues: True if communities missing embeddings (blocks DRIFT)
            - has_warnings: True if >10% chunks missing embeddings
            - issues: List of human-readable issue descriptions
            - suggestions: List of remediation steps
        """
        step = "validate_all_embeddings"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        result_dict: Dict[str, Any] = {
            "has_critical_issues": False,
            "has_warnings": False,
            "issues": [],
            "suggestions": []
        }
        
        try:
            if not self._project_id:
                logger.warning("No project_id set, skipping comprehensive embedding validation")
                ok = False
                err = "No project_id set"
                return result_dict
                
            with self._driver.session() as session:
                result = session.run(get_validate_embeddings_query(), project_id=self._project_id)
                record = result.single()
                
                if record:
                    # Store raw counts
                    result_dict.update({
                        "total_chunks": record.get("total_chunks", 0),
                        "chunks_with_embedding": record.get("chunks_with_embedding", 0),
                        "chunks_missing_embedding": record.get("chunks_missing_embedding", 0),
                        "total_entities": record.get("total_entities", 0),
                        "entities_with_embedding": record.get("entities_with_embedding", 0),
                        "entities_missing_embedding": record.get("entities_missing_embedding", 0),
                        "total_communities": record.get("total_communities", 0),
                        "communities_with_embedding": record.get("communities_with_embedding", 0),
                        "communities_missing_embedding": record.get("communities_missing_embedding", 0),
                        "community_embedding_coverage": record.get("community_embedding_coverage", 0.0),
                    })
                    
                    # Analyze and flag issues
                    communities_missing = result_dict["communities_missing_embedding"]
                    chunks_missing = result_dict["chunks_missing_embedding"]
                    total_chunks = result_dict["total_chunks"]
                    
                    # CRITICAL: Missing community embeddings blocks DRIFT
                    if communities_missing > 0:
                        result_dict["has_critical_issues"] = True
                        result_dict["issues"].append(
                            f"CRITICAL: {communities_missing} communities missing embeddings - DRIFT search will fail"
                        )
                        result_dict["suggestions"].append(
                            "Run LanceDB ingestion again or use update_community_embedding_by_id() to fix specific communities"
                        )
                        logger.error(
                            "CRITICAL: Communities missing embeddings - DRIFT search will fail",
                            project_id=self._project_id,
                            missing_count=communities_missing,
                            total_count=result_dict["total_communities"]
                        )
                    
                    # WARNING: Many chunks missing embeddings degrades quality
                    if total_chunks > 0 and chunks_missing > total_chunks * 0.1:
                        result_dict["has_warnings"] = True
                        pct = (chunks_missing / total_chunks * 100)
                        result_dict["issues"].append(
                            f"WARNING: {chunks_missing} ({pct:.1f}%) chunks missing embeddings - DRIFT quality degraded"
                        )
                        result_dict["suggestions"].append(
                            "Check LanceDB table 'default-text_unit-text' for completeness"
                        )
                        logger.warning(
                            "HIGH: Many chunks missing embeddings - DRIFT search quality degraded",
                            project_id=self._project_id,
                            missing_count=chunks_missing,
                            total_count=total_chunks,
                            percentage=f"{pct:.1f}%"
                        )
                    
                    # Log success or summary
                    if not result_dict["has_critical_issues"] and not result_dict["has_warnings"]:
                        logger.info(
                            "Embedding validation PASSED - all critical nodes have embeddings",
                            project_id=self._project_id,
                            community_coverage=f"{result_dict['community_embedding_coverage'] * 100:.1f}%",
                            chunks_ok=result_dict["chunks_with_embedding"],
                            entities_ok=result_dict["entities_with_embedding"],
                            communities_ok=result_dict["communities_with_embedding"]
                        )
                    else:
                        logger.warning(
                            "Embedding validation completed with issues",
                            project_id=self._project_id,
                            critical_issues=result_dict["has_critical_issues"],
                            warnings=result_dict["has_warnings"],
                            issue_count=len(result_dict["issues"])
                        )
                    
        except Exception as exc:
            ok = False
            err = str(exc)
            result_dict["has_critical_issues"] = True
            result_dict["issues"].append(f"Validation failed: {str(exc)}")
            logger.warning("Failed to validate embeddings", error=err, project_id=self._project_id)
        finally:
            callbacks.backfill_end(step, ok, err)
            
        return result_dict

    # -----------------------
    # Validation and cleanup
    # -----------------------
    def validate_relationships(self) -> Dict[str, Any]:
        """Validate relationship health for the current project.
        
        Returns statistics about entities and relationships including duplicate detection.
        """
        if not self._project_id:
            raise ValueError("project_id is required for relationship validation")
        
        with self._driver.session() as session:
            result = session.run(get_validate_relationships_query(), project_id=self._project_id)
            record = result.single()
            if record:
                return dict(record)
            return {}

    def cleanup_duplicate_relationships(self) -> int:
        """Remove duplicate RELATED relationships for the current project.
        
        Returns the count of duplicates removed.
        """
        if not self._project_id:
            raise ValueError("project_id is required for relationship cleanup")
        
        with self._driver.session() as session:
            result = session.run(get_cleanup_duplicate_relationships_query(), project_id=self._project_id)
            record = result.single()
            if record:
                return record.get("total_duplicates_removed", 0)
            return 0

    def sync_entity_relationship_ids(self, callbacks: IngestionWorkflowCallbacks) -> int:
        """Synchronize entity.relationship_ids with actual RELATED relationships.
        
        Should be run after:
        - Relationship ingestion
        - Relationship cleanup/deletion
        - Data quality maintenance
        
        Returns the count of entities updated.
        """
        step = "sync_entity_relationship_ids"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        count = 0
        
        try:
            if not self._project_id:
                logger.warning("No project_id set, skipping entity relationship ID sync")
                ok = False
                err = "No project_id set"
                return count
                
            with self._driver.session() as session:
                result = session.run(get_sync_entity_relationship_ids(), project_id=self._project_id)
                record = result.single()
                if record:
                    count = record.get("entities_updated", 0)
                    logger.info("Synchronized entity relationship IDs", project_id=self._project_id, count=count)
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to sync entity relationship IDs", error=err, project_id=self._project_id)
        finally:
            callbacks.backfill_end(step, ok, err)
            
        return count

    def detect_orphaned_nodes(self) -> Dict[str, Any]:
        """Detect orphaned nodes for the current project.
        
        Returns statistics about orphaned nodes by type.
        """
        if not self._project_id:
            raise ValueError("project_id is required for orphaned node detection")
        
        with self._driver.session() as session:
            result = session.run(get_detect_orphaned_nodes_query(), project_id=self._project_id)
            record = result.single()
            if record:
                return dict(record)
            return {}

    def cleanup_orphaned_nodes(self, callbacks: IngestionWorkflowCallbacks) -> Dict[str, int]:
        """Remove orphaned nodes after ingestion and deduplication.
        
        Returns counts of removed nodes by type.
        """
        if not self._project_id:
            raise ValueError("project_id is required for orphaned node cleanup")
        
        step = "cleanup_orphaned_nodes"
        callbacks.backfill_start(step)
        ok = True
        err: str | None = None
        result_counts = {}
        
        try:
            with self._driver.session() as session:
                result = session.run(get_cleanup_orphaned_nodes_query(), project_id=self._project_id)
                record = result.single()
                if record:
                    result_counts = {
                        "orphaned_chunks_removed": record.get("orphaned_chunks_removed", 0),
                        "orphaned_entities_removed": record.get("orphaned_entities_removed", 0),
                        "orphaned_communities_removed": record.get("orphaned_communities_removed", 0),
                        "unlinked_nodes_removed": record.get("unlinked_nodes_removed", 0),
                        "total_removed": record.get("total_removed", 0),
                    }
                    logger.info(
                        "orphaned_nodes_cleanup_completed",
                        project_id=self._project_id,
                        **result_counts
                    )
        except Exception as exc:
            ok = False
            err = str(exc)
            logger.warning("Failed to cleanup orphaned nodes", error=err, project_id=self._project_id)
        finally:
            callbacks.backfill_end(step, ok, err)
        
        return result_counts
