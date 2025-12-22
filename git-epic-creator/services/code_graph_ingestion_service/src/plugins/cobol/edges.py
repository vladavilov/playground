"""COBOL edge extraction.

Conservative implementation:
- Emits CALLS / PERFORMS edges with confidence-coded targets.
- When resolution is uncertain, creates explicit `unresolved` target nodes.
"""

from __future__ import annotations

import re
from dataclasses import dataclass

from core.records import CodeNodeRecord, EdgeRecord
from core.stable_ids import snippet_hash, stable_node_id


_RE_CALL_LITERAL = re.compile(r'\bCALL\s+"([^"]+)"', re.IGNORECASE)
_RE_CALL_DYNAMIC = re.compile(r"\bCALL\s+([A-Za-z0-9_-]+)\b", re.IGNORECASE)
_RE_PERFORM = re.compile(r"\bPERFORM\s+([A-Za-z0-9_-]+)\b", re.IGNORECASE)


@dataclass(frozen=True)
class CobolEdgesResult:
    nodes: list[CodeNodeRecord]
    edges: list[EdgeRecord]


def extract_cobol_edges(
    *,
    project_id: str,
    repo_fingerprint: str,
    file_path: str,
    program_node_id: str,
    physical_lines: list[str],
) -> CobolEdgesResult:
    nodes: list[CodeNodeRecord] = []
    edges: list[EdgeRecord] = []

    text = "\n".join(physical_lines) + "\n"

    # CALL "LITERAL"
    for m in _RE_CALL_LITERAL.finditer(text):
        callee = m.group(1).strip()
        target_id = _unresolved_node_id(project_id, repo_fingerprint, file_path, "call", callee)
        nodes.append(
            _unresolved_node(
                project_id=project_id,
                repo_fingerprint=repo_fingerprint,
                file_path=file_path,
                start_line=1,
                end_line=len(physical_lines),
                symbol=callee,
                node_id=target_id,
                extra_labels=("__UnresolvedCall__",),
            )
        )
        edges.append(
            EdgeRecord(
                project_id=project_id,
                repo_fingerprint=repo_fingerprint,
                rel_type="CALLS",
                src_node_id=program_node_id,
                dst_node_id=target_id,
                confidence=0.8,
                metadata={"callee": callee, "call_type": "literal"},
            )
        )

    # Dynamic CALL target
    for m in _RE_CALL_DYNAMIC.finditer(text):
        token = m.group(1).strip()
        # Skip ones already matched as literals via the other regex.
        if token.startswith('"') and token.endswith('"'):
            continue
        if token.upper() == "CALL":
            continue
        target_id = _unresolved_node_id(project_id, repo_fingerprint, file_path, "call", token)
        nodes.append(
            _unresolved_node(
                project_id=project_id,
                repo_fingerprint=repo_fingerprint,
                file_path=file_path,
                start_line=1,
                end_line=len(physical_lines),
                symbol=token,
                node_id=target_id,
                extra_labels=("__UnresolvedCall__",),
            )
        )
        edges.append(
            EdgeRecord(
                project_id=project_id,
                repo_fingerprint=repo_fingerprint,
                rel_type="CALLS",
                src_node_id=program_node_id,
                dst_node_id=target_id,
                confidence=0.2,
                metadata={"callee": token, "call_type": "dynamic"},
            )
        )

    # PERFORM targets (paragraph/section)
    for m in _RE_PERFORM.finditer(text):
        target = m.group(1).strip()
        target_id = _unresolved_node_id(project_id, repo_fingerprint, file_path, "perform", target)
        nodes.append(
            _unresolved_node(
                project_id=project_id,
                repo_fingerprint=repo_fingerprint,
                file_path=file_path,
                start_line=1,
                end_line=len(physical_lines),
                symbol=target,
                node_id=target_id,
                extra_labels=(),
            )
        )
        edges.append(
            EdgeRecord(
                project_id=project_id,
                repo_fingerprint=repo_fingerprint,
                rel_type="PERFORMS",
                src_node_id=program_node_id,
                dst_node_id=target_id,
                confidence=0.6,
                metadata={"target": target},
            )
        )

    # Deterministic de-dup by dst_node_id + rel_type + metadata key.
    uniq_nodes = {n.node_id: n for n in nodes}
    uniq_edges = {}
    for e in edges:
        key = (e.rel_type, e.src_node_id, e.dst_node_id, tuple(sorted((e.metadata or {}).items())))
        uniq_edges[key] = e

    return CobolEdgesResult(nodes=list(uniq_nodes.values()), edges=list(uniq_edges.values()))


def _unresolved_node_id(project_id: str, repo_fingerprint: str, file_path: str, kind: str, symbol: str) -> str:
    return stable_node_id(project_id, repo_fingerprint, file_path, "unresolved", kind, symbol)


def _unresolved_node(
    *,
    project_id: str,
    repo_fingerprint: str,
    file_path: str,
    start_line: int,
    end_line: int,
    symbol: str,
    node_id: str,
    extra_labels: tuple[str, ...],
) -> CodeNodeRecord:
    # Unresolved nodes use the file as evidence container.
    text = ""  # keep empty; evidence is in the source program node
    sh = snippet_hash(file_path=file_path, start_line=start_line, end_line=end_line, text=text)
    return CodeNodeRecord(
        project_id=project_id,
        repo_fingerprint=repo_fingerprint,
        node_id=node_id,
        language="cobol",
        kind="unresolved",
        symbol=symbol,
        file_path=file_path,
        start_line=start_line,
        end_line=end_line,
        snippet_hash=sh,
        text=text,
        extra_labels=extra_labels,
    )


