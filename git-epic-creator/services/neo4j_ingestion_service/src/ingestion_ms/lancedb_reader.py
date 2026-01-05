import structlog
from pathlib import Path
from typing import Any, Dict, List, Optional

import pandas as pd
import lancedb

from configuration.vector_index_config import get_vector_index_env
from .callbacks import IngestionWorkflowCallbacks

logger = structlog.get_logger(__name__)


class LanceDBReader:
    """Read LanceDB tables under workspace/output/lancedb and return ingestible rows.

    Output schema per table: {"text": str, "embedding": List[float]}
    """

    DEFAULT_TABLES: Dict[str, str] = {
        "community_summary": "default-community-full_content",
        "entity_description": "default-entity-description",
        "chunk_text": "default-text_unit-text",
    }

    def __init__(self) -> None:
        pass

    def read_all_embeddings(self, workspace: Path, callbacks: IngestionWorkflowCallbacks) -> Dict[str, List[Dict[str, Any]]]:
        base = (Path(workspace) / "output" / "lancedb").resolve()
        env = get_vector_index_env()
        dims = int(env.VECTOR_INDEX_DIMENSIONS)
        results: Dict[str, List[Dict[str, Any]]] = {k: [] for k in self.DEFAULT_TABLES.keys()}
        if not base.exists():
            logger.error("No lancedb directory found", path=str(base))
            return results

        for key, table_name in self.DEFAULT_TABLES.items():
            callbacks.vectors_read_start(table_name)
            df = self._read_table_df(base, table_name)
            if df is None:
                logger.warning("LanceDB table empty or not found", table=table_name)
                results[key] = []
                callbacks.vectors_read_end(table_name, 0)
                continue
            # Prefer stable IDs if present so downstream embedding upserts are deterministic.
            id_col = "id" if "id" in df.columns else None
            rows = self._build_rows(df, dims=dims, text_col="text", vector_col="vector", id_col=id_col)
            results[key] = rows
            callbacks.vectors_read_end(table_name, len(rows))
        return results

    def _read_table_df(self, base_dir: Path, table_name: str) -> Optional[pd.DataFrame]:
        # Use absolute db_uri to avoid relative-path quirks
        db_uri = str(Path(base_dir).resolve())
        conn = lancedb.connect(db_uri)
        table = conn.open_table(table_name)
        return table.to_pandas()

    def _to_list_1d(self, value: Any) -> List[float]:
        if hasattr(value, "tolist"):
            value = value.tolist()
        return [float(x) for x in value]

    def _build_rows(
        self,
        df: pd.DataFrame,
        dims: int,
        text_col: str = "text",
        vector_col: str = "vector",
        id_col: str | None = None,
    ) -> List[Dict[str, Any]]:
        rows: List[Dict[str, Any]] = []
        cols: list[str] = [text_col, vector_col]
        if id_col and id_col in df.columns:
            cols = [id_col] + cols
        for tup in df[cols].itertuples(index=False, name=None):
            if id_col and id_col in df.columns:
                row_id, text, vector = tup
            else:
                row_id = None
                text, vector = tup
            vec = self._to_list_1d(vector)
            if len(vec) < dims:
                continue
            if len(vec) > dims:
                vec = vec[:dims]
            row: Dict[str, Any] = {"text": str(text), "embedding": vec}
            if row_id is not None:
                row["id"] = str(row_id)
            rows.append(row)
        return rows


