"""Deterministic stable IDs and snippet hashing."""

from __future__ import annotations

import hashlib


def stable_node_id(*parts: str, size: int = 24) -> str:
    """Return a stable node_id derived from semantic identity + span."""

    raw = "|".join(parts).encode("utf-8", errors="replace")
    return hashlib.sha256(raw).hexdigest()[:size]


def _normalize_text_for_snippet_hash(text: str) -> str:
    """Normalize text for snippet hashing.

    Allowed normalizations (IMPLEMENTATION.md §4.2):
    - newline normalization (\r\n/\r -> \n)
    - trimming trailing whitespace
    """

    t = text.replace("\r\n", "\n").replace("\r", "\n")
    lines = t.split("\n")
    lines = [ln.rstrip(" \t") for ln in lines]
    return "\n".join(lines)


def snippet_hash(*, file_path: str, start_line: int, end_line: int, text: str) -> str:
    """Compute deterministic snippet hash for evidence fidelity."""

    norm = _normalize_text_for_snippet_hash(text)
    raw = f"{file_path}|{start_line}|{end_line}|\n{norm}".encode("utf-8", errors="replace")
    return hashlib.sha256(raw).hexdigest()

