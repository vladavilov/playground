from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path

import pytest
from tree_sitter import Language, Parser

import tree_sitter_cobol

FIXTURES_DIR = Path(__file__).resolve().parent / "fixtures"


@dataclass(frozen=True)
class ParseMetrics:
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


def _parse_bytes(source: bytes):
    lang = Language(tree_sitter_cobol.language())
    parser = Parser()
    parser.language = lang
    return parser.parse(source)


def _metrics_for_file(path: Path) -> ParseMetrics:
    tree = _parse_bytes(path.read_bytes())
    root = tree.root_node

    total, error_nodes, missing_nodes = _walk_counts(root)
    error_ratio = (error_nodes + missing_nodes) / max(total, 1)

    return ParseMetrics(
        has_error=root.has_error,
        error_ratio=error_ratio,
        total_nodes=total,
        error_nodes=error_nodes,
        missing_nodes=missing_nodes,
    )


def _max_error_ratio() -> float:
    return float(os.getenv("COBOL_TS_MAX_ERROR_RATIO", "0.02"))


@pytest.mark.parametrize(
    "dialect_dir, strict",
    [
        ("cobol_core", True),
        ("ibm_enterprise", False),
        ("micro_focus", False),
        ("gnucobol", False),
    ],
)
def test_fixture_quality_gates(dialect_dir: str, strict: bool) -> None:
    folder = FIXTURES_DIR / dialect_dir
    files = sorted([p for p in folder.rglob("*") if p.is_file()])
    assert files, f"No fixtures found in {folder}"

    max_ratio = _max_error_ratio()

    for path in files:
        m = _metrics_for_file(path)

        # Deterministic import+parse capability check.
        assert m.total_nodes > 0

        if strict:
            assert (
                m.has_error is False
            ), f"{path} has_error=True (error_nodes={m.error_nodes}, missing={m.missing_nodes})"
        else:
            assert (
                m.error_ratio <= max_ratio
            ), f"{path} error_ratio={m.error_ratio:.4f} > {max_ratio} (error_nodes={m.error_nodes}, missing={m.missing_nodes}, total={m.total_nodes})"


def test_languages_api_returns_loadable_capsules() -> None:
    """All advertised dialect keys should produce loadable Language capsules."""

    caps = tree_sitter_cobol.languages()
    assert isinstance(caps, dict)
    assert {"cobol", "ibm", "micro_focus", "gnucobol"} <= set(caps.keys())

    for key, cap in caps.items():
        lang = Language(cap)
        parser = Parser()
        parser.language = lang
        tree = parser.parse(b"       IDENTIFICATION DIVISION.\n       PROGRAM-ID. HELLO.\n       PROCEDURE DIVISION.\n           STOP RUN.\n")
        assert tree.root_node is not None, key
