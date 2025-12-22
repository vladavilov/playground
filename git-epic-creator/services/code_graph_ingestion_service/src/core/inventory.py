"""Deterministic file inventory builder (Task 05).

Produces stable file entries:
- path: repo-relative POSIX path
- sha256: sha256 over file bytes
- language: inferred from extension (conservative)
- line_count: physical line count based on bytes
"""

from __future__ import annotations

import hashlib
from dataclasses import dataclass
from pathlib import Path

from core.ignore_rules import IgnoreRules, build_ignore_rules


Language = str  # "cobol" | "java" | "javascript" | "xml" | "other"


@dataclass(frozen=True)
class InventoryEntry:
    path: str
    sha256: str
    language: Language
    line_count: int

def _count_physical_lines(data: bytes) -> int:
    if not data:
        return 0
    n = data.count(b"\n")
    if data.endswith(b"\n"):
        return n
    return n + 1


def _infer_language(rel_posix_path: str) -> Language:
    p = rel_posix_path.lower()
    if p.endswith((".cbl", ".cob", ".cpy")):
        return "cobol"
    if p.endswith(".java"):
        return "java"
    if p.endswith((".js", ".jsx", ".mjs", ".cjs", ".ts", ".tsx")):
        return "javascript"
    if p.endswith((".xml", ".xsd", ".wsdl")):
        return "xml"
    return "other"


def iter_repo_files(repo_root: Path, ignore: IgnoreRules) -> list[Path]:
    """Return a deterministic list of in-scope files under `repo_root`."""

    files: list[Path] = []
    for p in repo_root.rglob("*"):
        if not p.is_file():
            continue
        if p.is_symlink():
            continue
        rel = p.relative_to(repo_root).as_posix()
        if ignore.is_ignored(rel):
            continue
        files.append(p)
    return sorted(files, key=lambda x: x.relative_to(repo_root).as_posix())


def build_inventory(repo_root: Path, ignore: IgnoreRules | None = None) -> list[InventoryEntry]:
    """Build deterministic `files[]` inventory for `repo_index.json`."""

    ignore = ignore or build_ignore_rules(repo_root)
    out: list[InventoryEntry] = []

    for p in iter_repo_files(repo_root, ignore):
        rel = p.relative_to(repo_root).as_posix()
        data = p.read_bytes()
        out.append(
            InventoryEntry(
                path=rel,
                sha256=hashlib.sha256(data).hexdigest(),
                language=_infer_language(rel),
                line_count=_count_physical_lines(data),
            )
        )
    return out


