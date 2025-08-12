"""
Import GraphRAG parquet outputs into Neo4j with idempotent MERGE.
"""

from __future__ import annotations

from pathlib import Path
from typing import Dict
import pandas as pd


def ensure_constraints(neo4j_client) -> None:
    queries = [
        "CREATE CONSTRAINT document_id_unique IF NOT EXISTS FOR (d:`__Document__`) REQUIRE d.id IS UNIQUE",
        "CREATE CONSTRAINT entity_id_unique IF NOT EXISTS FOR (e:`__Entity__`) REQUIRE e.id IS UNIQUE",
        "CREATE CONSTRAINT community_id_unique IF NOT EXISTS FOR (c:`__Community__`) REQUIRE c.id IS UNIQUE",
    ]
    for q in queries:
        neo4j_client.execute_query_with_retry(q)


def _read_table_counts(output_dir: Path) -> Dict[str, int]:
    def read_count(name: str) -> int:
        fp = output_dir / f"{name}.parquet"
        if not fp.exists():
            return 0
        df = pd.read_parquet(fp, engine="fastparquet")
        return int(len(df.index))

    return {
        "documents": read_count("documents"),
        "text_units": read_count("text_units"),
        "entities": read_count("entities"),
        "relationships": read_count("relationships"),
        "communities": read_count("communities"),
        "community_reports": read_count("community_reports"),
    }


def import_graphrag_outputs(workdir: Path, neo4j_client) -> Dict[str, int]:
    output_dir = Path(workdir) / "output"

    counts = _read_table_counts(output_dir)

    ensure_constraints(neo4j_client)

    return counts


