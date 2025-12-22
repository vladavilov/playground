"""Ignore rules for deterministic inventory (Task 05).

Uses gitignore-compatible matching via `pathspec`.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import pathspec


# Deterministic defaults. Keep stable; expanding is ok if done deliberately.
DEFAULT_IGNORE_PATTERNS: tuple[str, ...] = (
    # VCS
    ".git/",
    ".hg/",
    ".svn/",
    # Python
    "__pycache__/",
    "*.pyc",
    "*.pyo",
    ".pytest_cache/",
    ".mypy_cache/",
    ".ruff_cache/",
    ".tox/",
    ".nox/",
    ".venv/",
    "venv/",
    # Node
    "node_modules/",
    # Java / build
    "target/",
    "build/",
    "dist/",
    "out/",
    # IDE / OS junk
    ".idea/",
    ".vscode/",
    "*.swp",
    "Thumbs.db",
    "Desktop.ini",
)


@dataclass(frozen=True)
class IgnoreRules:
    spec: pathspec.PathSpec

    def is_ignored(self, repo_rel_posix_path: str) -> bool:
        # PathSpec expects POSIX-style paths.
        return self.spec.match_file(repo_rel_posix_path)


def build_ignore_rules(repo_root: Path, extra_patterns: list[str] | None = None) -> IgnoreRules:
    """Build ignore rules from defaults + optional `.gitignore` + extra patterns."""

    patterns: list[str] = list(DEFAULT_IGNORE_PATTERNS)
    if extra_patterns:
        patterns.extend(extra_patterns)

    gitignore = repo_root / ".gitignore"
    if gitignore.is_file():
        # gitignore is line-based and already in gitwildmatch format.
        patterns.extend(gitignore.read_text(encoding="utf-8", errors="replace").splitlines())

    # Deterministic: PathSpec treats order as given; keep stable ordering.
    spec = pathspec.PathSpec.from_lines("gitwildmatch", patterns)
    return IgnoreRules(spec=spec)


