import structlog
from pathlib import Path
from typing import Any, Dict, List, Optional

import pandas as pd
import lancedb

from configuration.vector_index_config import get_vector_index_env

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

    def read_all_embeddings(self, workspace: Path) -> Dict[str, List[Dict[str, Any]]]:
        base = Path(workspace) / "output" / "lancedb"
        env = get_vector_index_env()
        dims = int(env.VECTOR_INDEX_DIMENSIONS)
        results: Dict[str, List[Dict[str, Any]]] = {k: [] for k in self.DEFAULT_TABLES.keys()}
        if not base.exists():
            logger.error("No lancedb directory found", path=str(base))
            return results

        for key, table_name in self.DEFAULT_TABLES.items():
            df = self._read_table_df(base, table_name)
            if df is None:
                results[key] = []
                continue
            rows = self._build_rows(df, dims=dims, text_col="text", vector_col="vector")
            results[key] = rows
        return results

    def _read_table_df(self, base_dir: Path, table_name: str) -> Optional[pd.DataFrame]:
        conn = lancedb.connect(str(base_dir))
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
    ) -> List[Dict[str, Any]]:
        rows: List[Dict[str, Any]] = []
        for text, vector in df[[text_col, vector_col]].itertuples(index=False, name=None):
            vec = self._to_list_1d(vector)
            if len(vec) < dims:
                continue
            if len(vec) > dims:
                vec = vec[:dims]
            rows.append({"text": str(text), "embedding": vec})
        return rows


