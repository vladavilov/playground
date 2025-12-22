"""Internal record contracts for graph persistence."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional


Language = str  # "cobol" | "java" | "javascript" | "xml" | "other"


@dataclass(frozen=True)
class CodeNodeRecord:
    project_id: str
    repo_fingerprint: str
    node_id: str
    language: Language
    kind: str
    symbol: Optional[str]
    file_path: str
    start_line: int
    end_line: int
    snippet_hash: str
    text: str
    extra_labels: tuple[str, ...] = ()


@dataclass(frozen=True)
class EdgeRecord:
    project_id: str
    repo_fingerprint: str
    rel_type: str
    src_node_id: str
    dst_node_id: str
    confidence: float
    metadata: dict

