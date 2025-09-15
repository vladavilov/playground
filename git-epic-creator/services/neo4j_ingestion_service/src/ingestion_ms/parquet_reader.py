import structlog
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Sequence

import pandas as pd


logger = structlog.get_logger(__name__)


DOCUMENTS_FILE = "documents.parquet"
TEXT_UNITS_FILE = "text_units.parquet"
ENTITIES_FILE = "entities.parquet"
RELATIONSHIPS_FILE = "relationships.parquet"
COMMUNITY_REPORTS_FILE = "community_reports.parquet"
COMMUNITIES_FILE = "communities.parquet"


class ParquetReader:
    """Read GraphRAG parquet outputs and prepare ingestible rows."""

    def read_documents(self, output_dir: Path) -> List[Dict[str, Any]]:
        return self._prepare_rows(
            path=Path(output_dir) / DOCUMENTS_FILE,
            required_cols=["id"],
            transforms=None,
        )

    def read_chunks(self, output_dir: Path) -> List[Dict[str, Any]]:
        return self._prepare_rows(
            path=Path(output_dir) / TEXT_UNITS_FILE,
            required_cols=["id", "document_ids"],
            transforms=None,
        )

    def read_entities(self, output_dir: Path) -> List[Dict[str, Any]]:
        return self._prepare_rows(
            path=Path(output_dir) / ENTITIES_FILE,
            required_cols=["id", "text_unit_ids"],
            transforms=None,
        )

    def read_entity_relationships(self, output_dir: Path) -> List[Dict[str, Any]]:
        return self._prepare_rows(
            path=Path(output_dir) / RELATIONSHIPS_FILE,
            required_cols=["source", "target"],
            transforms=[self._synthesize_relationship_id()],
        )

    def read_community_reports(self, output_dir: Path) -> List[Dict[str, Any]]:
        return self._prepare_rows(
            path=Path(output_dir) / COMMUNITY_REPORTS_FILE,
            required_cols=None,
            transforms=[self._fillna_columns(["summary", "full_content", "rating_explanation"], "")],
        )

    def read_communities(self, output_dir: Path) -> List[Dict[str, Any]]:
        return self._prepare_rows(
            path=Path(output_dir) / COMMUNITIES_FILE,
            required_cols=["community", "entity_ids"],
            transforms=[self._normalize_list_column("entity_ids")],
        )

    def _read_parquet(self, path: Path, columns: Optional[List[str]] = None) -> Optional[pd.DataFrame]:
        try:
            if not path.exists():
                return None
            df = pd.read_parquet(str(path), columns=columns) if columns else pd.read_parquet(str(path))
            if df is None or df.empty:
                return None
            return df
        except Exception:
            return None

    def _validate_required_columns(self, df: pd.DataFrame, required: Sequence[str]) -> bool:
        if not required:
            return True
        present = set(df.columns)
        missing = [c for c in required if c not in present]
        if missing:
            logger.warning("Missing required columns", missing=missing)
            return False
        return True

    def _fillna_columns(self, columns: Sequence[str], fill_value: Any) -> Callable[[pd.DataFrame], pd.DataFrame]:
        def _apply(df: pd.DataFrame) -> pd.DataFrame:
            for col in columns:
                if col in df.columns:
                    df[col] = df[col].fillna(fill_value)
            return df
        return _apply

    def _normalize_list_column(self, column: str) -> Callable[[pd.DataFrame], pd.DataFrame]:
        def _apply(df: pd.DataFrame) -> pd.DataFrame:
            if column in df.columns:
                try:
                    df[column] = df[column].apply(lambda v: v if isinstance(v, list) else ([] if v is None else ([])))
                except Exception:
                    df[column] = [[] for _ in range(len(df))]
            return df
        return _apply

    def _synthesize_relationship_id(self) -> Callable[[pd.DataFrame], pd.DataFrame]:
        def _apply(df: pd.DataFrame) -> pd.DataFrame:
            if "id" not in df.columns:
                try:
                    df["id"] = df["source"].astype(str) + "|" + df["target"].astype(str)
                except Exception:
                    df["id"] = df.index.astype(str)
            return df
        return _apply

    def _prepare_rows(
        self,
        path: Path,
        required_cols: Optional[Sequence[str]],
        transforms: Optional[Sequence[Callable[[pd.DataFrame], pd.DataFrame]]],
    ) -> List[Dict[str, Any]]:
        df = self._read_parquet(path)
        if df is None:
            logger.warning("Parquet not found or empty", path=str(path))
            return []
        if required_cols and not self._validate_required_columns(df, required_cols):
            logger.warning("Parquet missing required columns", path=str(path))
            return []
        if transforms:
            for transform in transforms:
                df = transform(df)
        return df.to_dict("records")


