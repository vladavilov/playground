import asyncio
import json
from uuid import UUID
import structlog

from graphrag.callbacks.workflow_callbacks import WorkflowCallbacks
from graphrag.logger.progress import Progress
from utils.redis_client import get_redis_client
from utils.unified_redis_messages import ProjectProgressMessage
from constants import UI_CHANNEL_PREFIX, UI_PROJECT_PROGRESS_NAME

logger = structlog.get_logger(__name__)

class IngestionWorkflowCallbacks(WorkflowCallbacks):
    """Lightweight callbacks to log progress and publish status updates."""

    def __init__(self, project_id: str) -> None:
        self.project_id = project_id
        self._workflow_index = 0
        self._total_workflows = 0
        self._pending_tasks: list[asyncio.Task] = []
        # Progress normalization settings: document processing contributes 0-20%,
        # ingestion pipeline + vectors + backfill contribute the remaining 80%.
        self._offset_pct: float = 20.0
        self._band_ingestion: float = 65.0   # GraphRAG pipeline workflows (from 20% to 85%)
        self._band_vectors_read: float = 5.0 # LanceDB reads (to 90%)
        self._band_vectors_ingest: float = 10.0 # Vector writes (to 100% - backfill)
        self._band_backfill: float = 5.0     # Backfill to 100%
        self._ordered_workflows: list[str] = []
        self._weights: list[float] = []  # per-step weights within the 80% band (sum to 80)
        # Default per-workflow absolute weights within ingestion band (sum to _band_ingestion)
        # Modify these values as needed to tune overall progress behavior.
        self._weights_by_name: dict[str, float] = {
            "load_input_documents": 2.0,
            "create_base_text_units": 6.0,
            "create_final_documents": 4.0,
            "extract_graph": 20.0,
            "finalize_graph": 3.0,
            "extract_covariates": 3.0,
            "create_communities": 8.0,
            "create_final_text_units": 5.0,
            "create_community_reports": 6.0,
            "generate_text_embeddings": 8.0,
        }
        # Vector/backfill step tracking
        self._vectors_read_done: int = 0
        self._vectors_ingest_done: int = 0
        self._backfill_done: int = 0
        self._vectors_read_total: int = 3
        self._vectors_ingest_total: int = 3
        self._backfill_total: int = 2
        try:
            self._loop = asyncio.get_running_loop()
        except RuntimeError:
            self._loop = None

    async def _publish(
        self,
        process_step: str,
        processed_pct: float | None = None,
    ) -> None:
        """Publish a UI progress message.

        Only send processed_pct (already normalized). Do not send counts.
        """
        try:
            project_uuid = UUID(self.project_id)
            message = ProjectProgressMessage(
                project_id=project_uuid,
                status="rag_processing",
                process_step=process_step,
                processed_pct=(
                    float(processed_pct)
                    if processed_pct is not None
                    else None
                ),
            )

            channel = f"{UI_CHANNEL_PREFIX}:{UI_PROJECT_PROGRESS_NAME}"
            redis = get_redis_client()
            await redis.publish(channel, json.dumps(message.to_dict()))
        except Exception:
            # Intentionally ignore publishing errors to avoid impacting the pipeline
            pass

    def _schedule(
        self,
        process_step: str,
        processed_pct_override: float | None = None,
    ) -> None:
        def _create() -> None:
            try:
                task = asyncio.create_task(self._publish(process_step, processed_pct_override))
                self._pending_tasks.append(task)
            except Exception:
                pass

        try:
            if self._loop and self._loop.is_running():
                self._loop.call_soon_threadsafe(_create)
            else:
                _create()
        except Exception:
            pass

    async def drain(self) -> None:
        pending = [t for t in self._pending_tasks if not t.done()]
        self._pending_tasks.clear()
        if not pending:
            return
        try:
            await asyncio.gather(*pending, return_exceptions=True)
        except Exception:
            pass

    def _friendly_name(self, name: str) -> str:
        base = (name or "").replace("_", " ").strip()
        if not base:
            return "Unnamed step"
        return base[:1].upper() + base[1:]

    def _compute_weights(self, names: list[str]) -> None:
        """Assign default absolute weights per known step; fallback to equal share for unknown steps."""
        self._ordered_workflows = names or []
        n = len(self._ordered_workflows)
        if n <= 0:
            self._weights = []
            return
        # First try to map each step to a configured weight
        mapped: list[float] = []
        unknown_indices: list[int] = []
        total_mapped = 0.0
        for idx, name in enumerate(self._ordered_workflows):
            w = self._weights_by_name.get(name, None)
            if w is None:
                unknown_indices.append(idx)
                mapped.append(0.0)
            else:
                mapped.append(float(w))
                total_mapped += float(w)
        # Distribute any remaining band among unknown steps equally; if none unknown but sum differs, scale proportionally.
        remaining = max(0.0, self._band_ingestion - total_mapped)
        if unknown_indices:
            per_unknown = remaining / float(len(unknown_indices))
            for i in unknown_indices:
                mapped[i] = per_unknown
        else:
            # If everything is known but sums off (manual edits), scale to fit the band
            if total_mapped > 0:
                scale = self._band_ingestion / total_mapped
                mapped = [x * scale for x in mapped]
        self._weights = mapped

    def _cumulative_before(self, index_1_based: int) -> float:
        """Return cumulative weight of completed steps before the current 1-based index."""
        idx0 = max(0, min(index_1_based - 1, len(self._weights)))
        return sum(self._weights[:idx0])

    # WorkflowCallbacks overrides
    def pipeline_start(self, names: list[str]) -> None:  # type: ignore[override]
        self._workflow_index = 0
        self._total_workflows = len(names or [])
        self._compute_weights(names or [])
        logger.info("GraphRAG pipeline_start", total_workflows=self._total_workflows)
        # Align ingestion start to the offset boundary
        self._schedule("Pipeline started", processed_pct_override=self._offset_pct)

    def pipeline_end(self, results) -> None:  # type: ignore[override]
        logger.info("GraphRAG pipeline_end")
        # Do not mark 100% yet; keep room for vectors and backfill
        final_ingestion_pct = self._offset_pct + sum(self._weights)
        self._schedule("Pipeline finished", processed_pct_override=final_ingestion_pct)

    def workflow_start(self, name: str, instance: object) -> None:  # type: ignore[override]
        self._workflow_index += 1
        logger.info(
            "GraphRAG workflow_start",
            workflow=name,
            index=self._workflow_index,
            total=self._total_workflows,
        )
        friendly = self._friendly_name(name)
        cum = self._offset_pct + self._cumulative_before(self._workflow_index)
        self._schedule(
            f"Start: {friendly} ({self._workflow_index}/{self._total_workflows})",
            processed_pct_override=cum,
        )

    def workflow_end(self, name: str, instance: object) -> None:  # type: ignore[override]
        logger.info("GraphRAG workflow_end", workflow=name)
        friendly = self._friendly_name(name)
        cum = self._offset_pct + self._cumulative_before(self._workflow_index) + (
            self._weights[self._workflow_index - 1] if 0 < self._workflow_index <= len(self._weights) else 0.0
        )
        self._schedule(f"Finished: {friendly}", processed_pct_override=cum)

    def progress(self, progress: Progress) -> None:
        logger.info("GraphRAG progress", description=(progress.description or ""))
        # Map step-local progress to overall weighted processed_pct
        completed = getattr(progress, "completed_items", None)
        total = getattr(progress, "total_items", None)
        pct_override = None
        if isinstance(completed, (int, float)) and isinstance(total, (int, float)) and total:
            step_frac = max(0.0, min(1.0, float(completed) / float(total)))
            base = self._offset_pct + self._cumulative_before(self._workflow_index)
            step_weight = (
                self._weights[self._workflow_index - 1]
                if 0 < self._workflow_index <= len(self._weights)
                else 0.0
            )
            pct_override = base + step_weight * step_frac
        # Build a friendly per-step message with local percentage
        current_step = ""
        if 0 < self._workflow_index <= len(self._ordered_workflows):
            current_step = self._friendly_name(self._ordered_workflows[self._workflow_index - 1])
        desc = (progress.description or "").strip()
        if not current_step:
            current_step = self._friendly_name(desc) if desc else "Working"
        local_pct = int(round(step_frac * 100)) if 'step_frac' in locals() else None
        if local_pct is not None:
            label = f"{current_step} — {local_pct}%"
        else:
            label = f"{current_step}"
        self._schedule(label, processed_pct_override=pct_override)

    # -------------
    # Vector helpers
    # -------------
    def vectors_read_start(self, table_name: str) -> None:
        logger.info("Vectors read start", table=table_name)
        self._schedule(f"Vectors read start: {table_name}")

    def vectors_read_end(self, table_name: str, row_count: int) -> None:
        logger.info("Vectors read end", table=table_name, rows=row_count)
        # Increment vectors-read progress within its band
        self._vectors_read_done = min(self._vectors_read_total, self._vectors_read_done + 1)
        frac = self._vectors_read_done / float(self._vectors_read_total)
        base = self._offset_pct + sum(self._weights)
        pct = base + self._band_vectors_read * frac
        self._schedule(f"Vectors read finished: {table_name} ({self._vectors_read_done}/{self._vectors_read_total})", processed_pct_override=pct)

    def vectors_ingest_start(self, label: str, text_property: str, row_count: int) -> None:
        logger.info("Vectors ingest start", label=label, property=text_property, rows=row_count)
        self._schedule(f"Vectors ingest start: {label}.{text_property}")

    def vectors_ingest_end(
        self,
        label: str,
        text_property: str,
        ingested_count: int,
        total_count: int | None = None,
    ) -> None:
        logger.info("Vectors ingest end", label=label, property=text_property, ingested=ingested_count, total=total_count)
        self._vectors_ingest_done = min(self._vectors_ingest_total, self._vectors_ingest_done + 1)
        # Progress includes vectors_read band fully plus a portion of vectors_ingest band
        base = self._offset_pct + sum(self._weights) + self._band_vectors_read
        frac = self._vectors_ingest_done / float(self._vectors_ingest_total)
        pct = base + self._band_vectors_ingest * frac
        self._schedule(
            f"Vectors ingest finished: {label}.{text_property} ({self._vectors_ingest_done}/{self._vectors_ingest_total})",
            processed_pct_override=pct,
        )

    def vectors_ingest_error(self, label: str, text_property: str, error: str) -> None:
        logger.warning("Vectors ingest error", label=label, property=text_property, error=error)
        self._schedule(f"Vectors ingest error: {label}.{text_property}")

    # -------------
    # Backfill helpers
    # -------------
    def backfill_start(self, step_name: str) -> None:
        logger.info("Backfill start", step=step_name)
        self._schedule(f"Backfill start: {step_name}")

    def backfill_end(self, step_name: str, success: bool, error: str | None = None) -> None:
        if success:
            logger.info("Backfill end", step=step_name, success=True)
            self._backfill_done = min(self._backfill_total, self._backfill_done + 1)
            base = self._offset_pct + sum(self._weights) + self._band_vectors_read + self._band_vectors_ingest
            frac = self._backfill_done / float(self._backfill_total)
            pct = base + self._band_backfill * frac
            self._schedule(f"Backfill finished: {step_name} ({self._backfill_done}/{self._backfill_total})", processed_pct_override=pct)
        else:
            logger.warning("Backfill end", step=step_name, success=False, error=error)
            self._schedule(f"Backfill failed: {step_name}")


