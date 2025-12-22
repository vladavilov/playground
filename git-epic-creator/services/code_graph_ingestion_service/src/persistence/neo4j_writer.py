"""Row builders for code-graph persistence payloads.

This service no longer ships or executes Cypher. All Neo4j interactions are routed
through `neo4j-repository-service` (Rust). The helpers here only shape request rows.
"""

from __future__ import annotations

from typing import Iterable

from core.records import CodeNodeRecord, EdgeRecord

def code_node_rows(nodes: Iterable[CodeNodeRecord]) -> list[dict]:
    return [
        {
            "project_id": n.project_id,
            "repo_fingerprint": n.repo_fingerprint,
            "node_id": n.node_id,
            "props": {
                "language": n.language,
                "kind": n.kind,
                "symbol": n.symbol,
                "file_path": n.file_path,
                "start_line": n.start_line,
                "end_line": n.end_line,
                "snippet_hash": n.snippet_hash,
                "text": n.text,
            },
            "extra_labels": list(n.extra_labels),
        }
        for n in nodes
    ]


def edge_rows_by_type(edges: Iterable[EdgeRecord]) -> dict[str, list[dict]]:
    out: dict[str, list[dict]] = {}
    for e in edges:
        out.setdefault(e.rel_type, []).append(
            {
                "project_id": e.project_id,
                "repo_fingerprint": e.repo_fingerprint,
                "src_node_id": e.src_node_id,
                "dst_node_id": e.dst_node_id,
                "confidence": float(e.confidence),
                "metadata": e.metadata or {},
            }
        )
    return out


