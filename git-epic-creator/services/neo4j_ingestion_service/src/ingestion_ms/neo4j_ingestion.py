import logging
import os
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple

import pandas as pd
from .lancedb_utils import read_table_df, build_text_embedding_rows
from configuration.vector_index_config import get_vector_index_env

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


logger = logging.getLogger(__name__)


# -----------------------------
# Module-level constants (DRY)
DOCUMENTS_FILE = "documents.parquet"
TEXT_UNITS_FILE = "text_units.parquet"
ENTITIES_FILE = "entities.parquet"
RELATIONSHIPS_FILE = "relationships.parquet"
COMMUNITY_REPORTS_FILE = "community_reports.parquet"
COMMUNITIES_FILE = "communities.parquet"

TABLE_COMMUNITY_SUMMARY = "default-community-full_content"
TABLE_ENTITY_DESCRIPTION = "default-entity-description"
TABLE_TEXT_UNIT = "default-text_unit-text"
LANCEDB_SUBDIR = "output/lancedb"


def _batched_import(driver: Any, statement: str, rows: List[dict], batch_size: int = 1000) -> int:
    total = len(rows)
    if total == 0:
        return 0
    with driver.session() as session:
        for start in range(0, total, batch_size):
            batch = rows[start : min(start + batch_size, total)]
            session.run(statement, rows=batch)
    return total


def _read_parquet(path: Path, columns: Optional[List[str]] = None) -> Optional[pd.DataFrame]:
    """Read parquet safely; return None if missing or empty."""
    try:
        if not path.exists():
            return None
        df = pd.read_parquet(str(path), columns=columns) if columns else pd.read_parquet(str(path))
        if df is None or df.empty:
            return None
        return df
    except Exception:
        return None


# -----------------------------
# DataFrame transform helpers (high cohesion, reusable)

def _validate_required_columns(df: pd.DataFrame, required: Sequence[str]) -> bool:
    if not required:
        return True
    present = set(df.columns)
    missing = [c for c in required if c not in present]
    if missing:
        logger.warning("Missing required columns: %s", missing)
        return False
    return True


def _fillna_columns(columns: Sequence[str], fill_value: Any) -> Callable[[pd.DataFrame], pd.DataFrame]:
    def _apply(df: pd.DataFrame) -> pd.DataFrame:
        for col in columns:
            if col in df.columns:
                df[col] = df[col].fillna(fill_value)
        return df
    return _apply


def _normalize_list_column(column: str) -> Callable[[pd.DataFrame], pd.DataFrame]:
    def _apply(df: pd.DataFrame) -> pd.DataFrame:
        if column in df.columns:
            try:
                df[column] = df[column].apply(lambda v: v if isinstance(v, list) else ([] if v is None else ([])))
            except Exception:
                df[column] = [[] for _ in range(len(df))]
        return df
    return _apply


def _synthesize_relationship_id() -> Callable[[pd.DataFrame], pd.DataFrame]:
    def _apply(df: pd.DataFrame) -> pd.DataFrame:
        if "id" not in df.columns:
            try:
                df["id"] = df["source"].astype(str) + "|" + df["target"].astype(str)
            except Exception:
                df["id"] = df.index.astype(str)
        return df
    return _apply


# -----------------------------
# Unified parquet import pipeline (DRY)

def _import_parquet_rows(
    driver: Any,
    path: Path,
    required_cols: Optional[Sequence[str]],
    transforms: Optional[Sequence[Callable[[pd.DataFrame], pd.DataFrame]]],
    statement_builder: Callable[[], str],
    entity_name: str,
    batch_size: int = 1000,
) -> int:
    df = _read_parquet(path)
    if df is None:
        logger.info("%s not found or empty at %s", entity_name, path)
        return 0
    if required_cols and not _validate_required_columns(df, required_cols):
        logger.warning("%s missing required columns at %s", entity_name, path)
        return 0
    if transforms:
        for transform in transforms:
            try:
                df = transform(df)
            except Exception as exc:
                logger.exception("%s transform failed for %s: %s", entity_name, path, exc)
                # Continue after transform failure to avoid blocking entire import
    records = df.to_dict("records")
    count = _batched_import(driver, statement_builder(), records, batch_size=batch_size)
    logger.info("Imported %d %s from %s", count, entity_name, path)
    return count


def _import_community_reports(driver: Any, output_dir: Path) -> int:
    path = output_dir / COMMUNITY_REPORTS_FILE
    return _import_parquet_rows(
        driver=driver,
        path=path,
        required_cols=None,
        transforms=[_fillna_columns(["summary", "full_content", "rating_explanation"], "")],
        statement_builder=get_merge_community_report_query,
        entity_name="community reports",
    )


def _import_communities(driver: Any, output_dir: Path) -> int:
    path = output_dir / COMMUNITIES_FILE
    return _import_parquet_rows(
        driver=driver,
        path=path,
        required_cols=["community", "entity_ids"],
        transforms=[_normalize_list_column("entity_ids")],
        statement_builder=get_merge_community_query,
        entity_name="communities",
    )


def _import_documents(driver: Any, output_dir: Path) -> int:
    path = output_dir / DOCUMENTS_FILE
    return _import_parquet_rows(
        driver=driver,
        path=path,
        required_cols=["id"],
        transforms=None,
        statement_builder=get_merge_document_query,
        entity_name="documents",
    )


def _import_chunks(driver: Any, output_dir: Path) -> int:
    path = output_dir / TEXT_UNITS_FILE
    return _import_parquet_rows(
        driver=driver,
        path=path,
        required_cols=["id", "document_ids"],
        transforms=None,
        statement_builder=get_merge_chunk_query,
        entity_name="chunks",
    )


def _import_entities(driver: Any, output_dir: Path) -> int:
    path = output_dir / ENTITIES_FILE
    return _import_parquet_rows(
        driver=driver,
        path=path,
        required_cols=["id", "text_unit_ids"],
        transforms=None,
        statement_builder=get_merge_entity_query,
        entity_name="entities",
    )


def _import_entity_relationships(driver: Any, output_dir: Path) -> int:
    path = output_dir / RELATIONSHIPS_FILE
    return _import_parquet_rows(
        driver=driver,
        path=path,
        required_cols=["source", "target"],
        transforms=[_synthesize_relationship_id()],
        statement_builder=get_merge_relationship_query,
        entity_name="relationships",
    )


def ingest_workspace_output(driver: Any, output_dir: Path) -> Dict[str, int]:
    """Ingest GraphRAG workspace parquet outputs into Neo4j.

    Returns a dict of counts for each ingested category.
    """
    expected = [
        DOCUMENTS_FILE,
        TEXT_UNITS_FILE,
        ENTITIES_FILE,
        RELATIONSHIPS_FILE,
        COMMUNITY_REPORTS_FILE,
        COMMUNITIES_FILE,
    ]
    existing = {name: (output_dir / name).exists() for name in expected}
    logger.info("GraphRAG output directory: %s", output_dir)
    logger.info("Parquet file existence: %s", existing)

    counts = {
        "documents": _import_documents(driver, output_dir),
        "chunks": _import_chunks(driver, output_dir),
        "entities": _import_entities(driver, output_dir),
        "entity_relationships": _import_entity_relationships(driver, output_dir),
        "community_reports": _import_community_reports(driver, output_dir),
        "communities": _import_communities(driver, output_dir),
    }
    return counts


def backfill_entity_relationship_ids(driver: Any) -> None:
    try:
        with driver.session() as session:
            session.run(get_backfill_entity_rel_ids())
    except Exception as exc:
        logger.warning("Failed to backfill entity.relationship_ids: %s", exc)


def backfill_community_membership(driver: Any) -> None:
    try:
        with driver.session() as session:
            session.run(get_backfill_community_membership())
    except Exception as exc:
        logger.warning("Failed to backfill community.entity_ids and IN_COMMUNITY edges: %s", exc)

def backfill_vector_indexes(driver: Any, workspace: Path, batch_size: int = 1000) -> Dict[str, int]:
    """Backfill vector indexes from LanceDB tables under workspace/output/lancedb.

    Categories:
    - Community summary embeddings → `summary_embedding` of `__Community__`
    - Entity description embeddings → `description_embedding` of `__Entity__`
    - Chunk embeddings → `embedding` of `__Chunk__`
    """
    lancedb_dir = workspace / LANCEDB_SUBDIR
    results: Dict[str, int] = {
        "processed": 0,
        "updated": 0,
    }

    if not lancedb_dir.exists():
        logger.info("No lancedb directory found at %s", lancedb_dir)
        return results

    env = get_vector_index_env()
    vector_dims = int(env.VECTOR_INDEX_DIMENSIONS)
    # Single embedding property for all vectors (shared with maintenance service)
    embedding_prop = env.VECTOR_INDEX_PROPERTY

    def _build_update_embeddings_statement(node_label: str, text_property: str, embedding_property: str) -> str:
        return (
            "UNWIND $rows AS r\n"
            f"MATCH (n:`{node_label}` {{{text_property}: r.text}})\n"
            f"SET n.{embedding_property} = r.embedding\n"
            "RETURN 0\n"
        )

    def _backfill_from_lancedb_table(
        table_name: str,
        statement: str,
        label: str,
    ) -> Tuple[int, int]:
        try:
            df = read_table_df(lancedb_dir, table_name)
            if df is None:
                return 0, 0
            processed = int(len(df))
            rows: List[Dict[str, Any]] = build_text_embedding_rows(
                df, dims=vector_dims, text_col="text", vector_col="vector"
            )
            updated = 0
            if rows:
                updated = _batched_import(driver, statement, rows, batch_size=batch_size)
            logger.info("Backfilled %s embeddings: updated=%d processed=%d", label, updated, processed)
            logger.debug("%s embeddings rows prepared: %d", label, len(rows))
            return processed, updated
        except Exception as exc:
            logger.exception("Failed to backfill %s embeddings from LanceDB", label)
            return 0, 0

    # Execute backfills
    comm_stmt = _build_update_embeddings_statement("__Community__", "summary", embedding_prop)
    ent_stmt = _build_update_embeddings_statement("__Entity__", "description", embedding_prop)
    chunk_stmt = _build_update_embeddings_statement("__Chunk__", "text", embedding_prop)

    comm_processed, comm_updated = _backfill_from_lancedb_table(
        TABLE_COMMUNITY_SUMMARY, comm_stmt, "community summary"
    )
    ent_processed, ent_updated = _backfill_from_lancedb_table(
        TABLE_ENTITY_DESCRIPTION, ent_stmt, "entity description"
    )
    chunk_processed, chunk_updated = _backfill_from_lancedb_table(
        TABLE_TEXT_UNIT, chunk_stmt, "chunk"
    )

    results["processed"] = comm_processed + ent_processed + chunk_processed
    results["updated"] = comm_updated + ent_updated + chunk_updated

    return results


__all__ = [
    "ingest_workspace_output",
    "backfill_entity_relationship_ids",
    "backfill_community_membership",
    "backfill_vector_indexes",
]


