"""Tree-sitter per-file quality gate for COBOL."""

from __future__ import annotations

import os
from dataclasses import dataclass
import tree_sitter_cobol
from tree_sitter import Language, Parser

@dataclass(frozen=True)
class ParseGateMetrics:
    has_error: bool
    error_ratio: float
    total_nodes: int
    error_nodes: int
    missing_nodes: int


def _walk_counts(node) -> tuple[int, int, int]:
    total = 1
    error_nodes = 1 if node.type == "ERROR" else 0
    missing_nodes = 1 if getattr(node, "is_missing", False) else 0
    for child in node.children:
        t, e, m = _walk_counts(child)
        total += t
        error_nodes += e
        missing_nodes += m
    return total, error_nodes, missing_nodes


def max_error_ratio_default() -> float:
    return float(os.getenv("COBOL_TS_MAX_ERROR_RATIO", "0.02"))


def parse_with_treesitter(parse_bytes: bytes):
    lang = Language(tree_sitter_cobol.language())
    parser = Parser()
    parser.language = lang
    return parser.parse(parse_bytes)


def gate_treesitter(parse_bytes: bytes, *, max_error_ratio: float) -> tuple[bool, ParseGateMetrics]:
    """Return (ok, metrics) following the binding's test math."""

    tree = parse_with_treesitter(parse_bytes)
    root = tree.root_node
    total, error_nodes, missing_nodes = _walk_counts(root)
    error_ratio = (error_nodes + missing_nodes) / max(total, 1)
    metrics = ParseGateMetrics(
        has_error=bool(root.has_error),
        error_ratio=float(error_ratio),
        total_nodes=int(total),
        error_nodes=int(error_nodes),
        missing_nodes=int(missing_nodes),
    )
    ok = (not metrics.has_error) or (metrics.error_ratio <= max_error_ratio)
    return ok, metrics


