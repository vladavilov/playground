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
            transforms=[self._normalize_list_column("document_ids")],
        )

    def read_entities(self, output_dir: Path) -> List[Dict[str, Any]]:
        return self._prepare_rows(
            path=Path(output_dir) / ENTITIES_FILE,
            required_cols=["id", "text_unit_ids"],
            transforms=[self._normalize_list_column("text_unit_ids")],
        )

    def read_entity_relationships(self, output_dir: Path) -> List[Dict[str, Any]]:
        """
        Read entity relationships from parquet file.
        
        Expected columns from GraphRAG:
        - source (required): Source entity name
        - target (required): Target entity name
        - description (optional but expected): LLM-generated relationship description
        - weight (optional but expected): Relationship strength/weight
        - source_id: Synthesized if not present (fallback to source|target)
        
        If description/weight are missing, it indicates LLM schema drift (non-deterministic output).
        """
        path = Path(output_dir) / RELATIONSHIPS_FILE
        
        # Read raw parquet to check schema before validation
        df = self._read_parquet(path)
        if df is not None:
            present_cols = set(df.columns)
            expected_cols = {"source", "target", "description", "weight"}
            missing_cols = expected_cols - present_cols
            
            if missing_cols:
                logger.warning(
                    "SCHEMA_DRIFT_DETECTED: Relationship parquet missing expected columns - "
                    "This indicates non-deterministic LLM output. "
                    "Verify GRAPHRAG_LLM_TEMPERATURE=0 in configuration.",
                    path=str(path),
                    present_columns=sorted(present_cols),
                    missing_columns=sorted(missing_cols),
                    total_rows=len(df),
                )
        
        return self._prepare_rows(
            path=path,
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
        """Normalize a column to list type, handling various input formats.
        
        Handles:
        - Python lists (pass through)
        - None/NaN (convert to [])
        - JSON strings (parse)
        - Comma-separated strings (split)
        - Numpy arrays (convert)
        - Pandas Series/arrays (convert)
        - Numeric values (wrap in list)
        """
        def _parse_value(v: Any) -> list:
            import json
            import numpy as np
            
            # Already a list
            if isinstance(v, list):
                return v
            
            # None or NaN
            if v is None or (isinstance(v, float) and pd.isna(v)):
                return []
            
            # Numpy array
            if isinstance(v, np.ndarray):
                return v.tolist()
            
            # Pandas array-like
            if hasattr(v, 'tolist'):
                return v.tolist()
            
            # String types - try JSON first, then comma-separated
            if isinstance(v, str):
                v = v.strip()
                if not v:
                    return []
                
                # Try parsing as JSON array
                if v.startswith('[') and v.endswith(']'):
                    try:
                        parsed = json.loads(v)
                        if isinstance(parsed, list):
                            return parsed
                    except (json.JSONDecodeError, ValueError):
                        pass
                
                # Try comma-separated values
                if ',' in v:
                    return [item.strip() for item in v.split(',') if item.strip()]
                
                # Single string value -> wrap in list
                return [v]
            
            # Numeric or other scalar -> wrap in list
            return [v]
        
        def _apply(df: pd.DataFrame) -> pd.DataFrame:
            if column in df.columns:
                try:
                    df[column] = df[column].apply(_parse_value)
                except Exception as exc:
                    logger.warning(
                        f"Failed to normalize list column {column}, defaulting to empty lists",
                        error=str(exc)
                    )
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


