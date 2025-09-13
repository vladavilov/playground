import logging
from pathlib import Path
from typing import Any, Dict, List, Optional

import pandas as pd


logger = logging.getLogger(__name__)


def read_table_df(base_dir: Path, table_name: str) -> Optional[pd.DataFrame]:
    """Open a LanceDB table and return a pandas DataFrame, or None on failure.

    - Connects to the LanceDB database at base_dir
    - Opens table by name
    - Tries to get pandas directly; falls back to Arrow->pandas
    """
    try:
        import lancedb  # type: ignore
    except Exception:
        logger.warning("lancedb is not installed; cannot read LanceDB tables")
        return None

    try:
        conn = lancedb.connect(str(base_dir))
        table = conn.open_table(table_name)
        try:
            return table.to_pandas()  # type: ignore[attr-defined]
        except Exception:
            try:
                return table.to_arrow().to_pandas()  # type: ignore[attr-defined]
            except Exception:
                logger.warning("Failed to read LanceDB table %s as pandas", table_name)
                return None
    except Exception:
        logger.info("LanceDB table %s not found under %s", table_name, base_dir)
        return None


def _to_list_1d(value: Any) -> Optional[List[float]]:
    if value is None:
        return None
    try:
        if hasattr(value, "tolist"):
            value = value.tolist()
        if isinstance(value, (list, tuple)):
            out: List[float] = []
            for x in value:
                if x is None:
                    return None
                out.append(float(x))
            return out
    except Exception:
        return None
    return None


def build_text_embedding_rows(
    df: pd.DataFrame, dims: int = 1536, text_col: str = "text", vector_col: str = "vector"
) -> List[Dict[str, Any]]:
    """Convert a LanceDB pandas DataFrame into rows of {text, embedding}.

    - Skips rows without text or vector
    - Skips vectors shorter than dims; slices vectors longer than dims
    """
    if df is None or df.empty:
        return []
    if text_col not in df.columns:
        logger.warning("Missing '%s' column in LanceDB DataFrame", text_col)
        return []
    if vector_col not in df.columns:
        logger.warning("Missing '%s' column in LanceDB DataFrame", vector_col)
        return []

    rows: List[Dict[str, Any]] = []
    for _, rec in df[[text_col, vector_col]].iterrows():
        text = rec.get(text_col)
        vec = _to_list_1d(rec.get(vector_col))
        if not text or vec is None:
            continue
        if len(vec) < dims:
            continue
        if len(vec) > dims:
            vec = vec[:dims]
        rows.append({"text": str(text), "embedding": vec})
    return rows


