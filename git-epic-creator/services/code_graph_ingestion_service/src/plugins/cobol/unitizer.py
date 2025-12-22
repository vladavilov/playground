"""COBOL unitization.

Minimal implementation:
- AST gate: try Tree-sitter parse; if usable, map root span back to evidence lines
- Fallback: deterministic scanner to produce a single program node

This will be expanded in later tasks to emit divisions/sections/paragraphs/sentences/statements.
"""

from __future__ import annotations

import re
from dataclasses import dataclass

from core.records import CodeNodeRecord, EdgeRecord
from core.stable_ids import snippet_hash, stable_node_id
from plugins.cobol.normalizer import PreprocessResult
from plugins.cobol.ts_gate import ParseGateMetrics, gate_treesitter


_RE_PROGRAM_ID = re.compile(r"\bPROGRAM-ID\.\s*([A-Za-z0-9_-]+)\b", re.IGNORECASE)


@dataclass(frozen=True)
class UnitizeResult:
    nodes: list[CodeNodeRecord]
    metrics: ParseGateMetrics | None
    used_treesitter: bool


def unitize_cobol_file(
    *,
    project_id: str,
    repo_fingerprint: str,
    file_path: str,
    preprocess: PreprocessResult,
    max_error_ratio: float,
) -> UnitizeResult:
    # Attempt Tree-sitter gate first.
    metrics: ParseGateMetrics | None = None
    used_ts = False

    start_line = 1
    end_line = len(preprocess.physical_lines)
    symbol = _extract_program_id(preprocess.physical_lines) or None

    try:
        ok, metrics = gate_treesitter(preprocess.parse_bytes, max_error_ratio=max_error_ratio)
        if ok:
            used_ts = True
            # Map root node (best-effort): use full file span based on mapping.
            # We keep it minimal for now: one program node spanning all evidence lines.
    except Exception:
        metrics = None
        used_ts = False

    text = _slice_text(preprocess.physical_lines, start_line, end_line)
    node_id = stable_node_id(project_id, repo_fingerprint, file_path, "program", symbol or "", str(start_line), str(end_line))
    sh = snippet_hash(file_path=file_path, start_line=start_line, end_line=end_line, text=text)
    nodes = [
        CodeNodeRecord(
            project_id=project_id,
            repo_fingerprint=repo_fingerprint,
            node_id=node_id,
            language="cobol",
            kind="program",
            symbol=symbol,
            file_path=file_path,
            start_line=start_line,
            end_line=end_line,
            snippet_hash=sh,
            text=text,
        )
    ]

    # Emit exec_block nodes from preprocessing markers.
    for idx, ln in enumerate(preprocess.logical_lines):
        if ln.strip().upper() != "EXEC_BLOCK.":
            continue
        span = preprocess.logical_spans[idx]
        s, e = span
        exec_text = _slice_text(preprocess.physical_lines, s, e)
        exec_id = stable_node_id(project_id, repo_fingerprint, file_path, "exec_block", "", str(s), str(e))
        exec_sh = snippet_hash(file_path=file_path, start_line=s, end_line=e, text=exec_text)
        nodes.append(
            CodeNodeRecord(
                project_id=project_id,
                repo_fingerprint=repo_fingerprint,
                node_id=exec_id,
                language="cobol",
                kind="exec_block",
                symbol=None,
                file_path=file_path,
                start_line=s,
                end_line=e,
                snippet_hash=exec_sh,
                text=exec_text,
            )
        )

    return UnitizeResult(nodes=nodes, metrics=metrics, used_treesitter=used_ts)


def _extract_program_id(physical_lines: list[str]) -> str | None:
    for line in physical_lines:
        m = _RE_PROGRAM_ID.search(line)
        if m:
            return m.group(1)
    return None


def _slice_text(lines: list[str], start_line: int, end_line: int) -> str:
    if start_line < 1:
        start_line = 1
    if end_line > len(lines):
        end_line = len(lines)
    if end_line < start_line:
        return ""
    return "\n".join(lines[start_line - 1 : end_line]) + "\n"



