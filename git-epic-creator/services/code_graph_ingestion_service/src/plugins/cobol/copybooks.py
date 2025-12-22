"""COBOL copybook expansion.

Deterministic, conservative implementation:
- Recognizes `COPY <name> ... .` in the parse stream
- Resolves copybooks via deterministic search paths
- Applies basic `REPLACING ==from== BY ==to==` substitutions
- Returns an expansion map for provenance
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path


_RE_COPY = re.compile(r"^\s*COPY\s+([A-Za-z0-9_.-]+)", re.IGNORECASE)
_RE_REPLACING = re.compile(r"\bREPLACING\b", re.IGNORECASE)
_RE_BY = re.compile(r"\bBY\b", re.IGNORECASE)
_RE_PSEUDO_TEXT = re.compile(r"==(.+?)==")


@dataclass(frozen=True)
class CopybookInsertProvenance:
    copybook_path: str  # repo-relative POSIX path
    copybook_start_line: int
    copybook_end_line: int
    inserted_at_logical_line: int
    inserted_line_count: int


@dataclass(frozen=True)
class CopybookExpansionResult:
    logical_lines: list[str]
    logical_spans: list[tuple[int, int]]
    includes: list[str]
    expansion_map: list[CopybookInsertProvenance]


def expand_copybooks(
    *,
    repo_root: Path,
    logical_lines: list[str],
    logical_spans: list[tuple[int, int]],
    search_paths: list[Path] | None = None,
) -> CopybookExpansionResult:
    """Expand COPY statements in a logical stream."""

    if len(logical_lines) != len(logical_spans):
        raise ValueError("logical_lines and logical_spans must be aligned")

    search_paths = search_paths or [repo_root]
    out_lines: list[str] = []
    out_spans: list[tuple[int, int]] = []
    includes: list[str] = []
    expansion_map: list[CopybookInsertProvenance] = []

    for idx, (line, span) in enumerate(zip(logical_lines, logical_spans), start=1):
        m = _RE_COPY.match(line)
        if not m:
            out_lines.append(line)
            out_spans.append(span)
            continue

        name = m.group(1).strip().rstrip(".")
        replacing_pairs = _parse_replacing_pairs(line)
        copy_path = _resolve_copybook(repo_root, name, search_paths)

        if copy_path is None:
            # Deterministic fallback: keep the COPY statement as-is.
            out_lines.append(line)
            out_spans.append(span)
            continue

        rel = copy_path.relative_to(repo_root).as_posix()
        includes.append(rel)

        raw = copy_path.read_text(encoding="utf-8", errors="replace").replace("\r\n", "\n").replace("\r", "\n")
        raw_lines = raw.split("\n")
        if raw_lines and raw_lines[-1] == "":
            raw_lines = raw_lines[:-1]

        # Apply REPLACING deterministically.
        inserted_lines = [_apply_replacing(ln, replacing_pairs) for ln in raw_lines]

        inserted_at = len(out_lines) + 1
        out_lines.extend(inserted_lines)
        # Inserted logical spans point back to the COPY statement span; provenance provides real origin.
        out_spans.extend([span] * len(inserted_lines))

        if inserted_lines:
            expansion_map.append(
                CopybookInsertProvenance(
                    copybook_path=rel,
                    copybook_start_line=1,
                    copybook_end_line=len(raw_lines),
                    inserted_at_logical_line=inserted_at,
                    inserted_line_count=len(inserted_lines),
                )
            )

    # Deterministic: de-dup includes while preserving order.
    seen = set()
    includes_dedup: list[str] = []
    for p in includes:
        if p in seen:
            continue
        seen.add(p)
        includes_dedup.append(p)

    return CopybookExpansionResult(
        logical_lines=out_lines,
        logical_spans=out_spans,
        includes=includes_dedup,
        expansion_map=expansion_map,
    )


def _resolve_copybook(repo_root: Path, name: str, search_paths: list[Path]) -> Path | None:
    # Deterministic candidate order.
    candidates = [
        name,
        f"{name}.cpy",
        f"{name}.cbl",
        f"{name}.cob",
        name.lower(),
        f"{name.lower()}.cpy",
        name.upper(),
        f"{name.upper()}.cpy",
    ]

    for base in search_paths:
        for cand in candidates:
            p = (base / cand).resolve()
            try:
                # Ensure resolved path stays within repo_root for safety/determinism.
                p.relative_to(repo_root.resolve())
            except Exception:
                continue
            if p.is_file():
                return p
    return None


def _parse_replacing_pairs(line: str) -> list[tuple[str, str]]:
    """Parse `REPLACING ==from== BY ==to== ...` pairs from a COPY line."""

    if not _RE_REPLACING.search(line):
        return []

    # Extract pseudo-text chunks and pair them around BY tokens.
    chunks = _RE_PSEUDO_TEXT.findall(line)
    if not chunks:
        return []

    # Heuristic: treat as alternating from/to pairs in order of appearance.
    pairs: list[tuple[str, str]] = []
    it = iter(chunks)
    for a in it:
        try:
            b = next(it)
        except StopIteration:
            break
        pairs.append((a, b))
    return pairs


def _apply_replacing(s: str, pairs: list[tuple[str, str]]) -> str:
    out = s
    for a, b in pairs:
        out = out.replace(a, b)
    return out


