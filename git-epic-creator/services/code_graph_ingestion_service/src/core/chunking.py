"""Oversized node chunking (Task 17, CGI-FR-018).

Deterministic chunking for nodes spanning >1000 physical lines.
"""

from __future__ import annotations

from dataclasses import dataclass

from core.records import CodeNodeRecord, EdgeRecord
from core.stable_ids import snippet_hash, stable_node_id


@dataclass(frozen=True)
class ChunkingResult:
    nodes: list[CodeNodeRecord]
    edges: list[EdgeRecord]


def chunk_oversized_nodes(
    nodes: list[CodeNodeRecord],
    *,
    max_lines: int = 1000,
    chunk_size: int = 250,
    overlap: int = 25,
) -> ChunkingResult:
    out_nodes: list[CodeNodeRecord] = []
    out_edges: list[EdgeRecord] = []

    for n in nodes:
        span_lines = max(0, n.end_line - n.start_line + 1)
        if span_lines <= max_lines or not n.text:
            out_nodes.append(n)
            continue

        # Chunk deterministically by physical line windows.
        physical_lines = n.text.replace("\r\n", "\n").replace("\r", "\n").split("\n")
        if physical_lines and physical_lines[-1] == "":
            physical_lines = physical_lines[:-1]

        start = n.start_line
        idx = 0
        chunk_ids: list[str] = []

        while start <= n.end_line:
            idx += 1
            end = min(n.end_line, start + chunk_size - 1)
            chunk_text = "\n".join(physical_lines[start - n.start_line : end - n.start_line + 1]) + "\n"
            cid = stable_node_id(n.project_id, n.repo_fingerprint, n.node_id, "chunk", str(idx))
            chunk_ids.append(cid)
            ch = CodeNodeRecord(
                project_id=n.project_id,
                repo_fingerprint=n.repo_fingerprint,
                node_id=cid,
                language=n.language,
                kind=n.kind,
                symbol=n.symbol,
                file_path=n.file_path,
                start_line=start,
                end_line=end,
                snippet_hash=snippet_hash(file_path=n.file_path, start_line=start, end_line=end, text=chunk_text),
                text=chunk_text,
                extra_labels=n.extra_labels,
            )
            out_nodes.append(ch)

            if end == n.end_line:
                break
            start = end - overlap + 1

        # Pipe edges between chunks (synthetic).
        for i in range(len(chunk_ids) - 1):
            out_edges.append(
                EdgeRecord(
                    project_id=n.project_id,
                    repo_fingerprint=n.repo_fingerprint,
                    rel_type="CALLS",
                    src_node_id=chunk_ids[i],
                    dst_node_id=chunk_ids[i + 1],
                    confidence=1.0,
                    metadata={"synthetic": True, "pipe": True, "order": i + 1},
                )
            )

    return ChunkingResult(nodes=out_nodes, edges=out_edges)



