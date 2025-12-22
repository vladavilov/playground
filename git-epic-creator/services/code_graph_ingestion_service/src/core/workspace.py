"""Deterministic workspace layout helpers.

The ingestion pipeline materializes repos into a project-scoped workspace on disk.
Paths must be deterministic for identical inputs (CGI-FR-005).
"""

from __future__ import annotations

import hashlib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path


_SAFE_SEGMENT_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.-]{0,127}$")


def _stable_key(value: str) -> str:
    """Return a stable directory-safe key.

    Prefer a readable safe segment; otherwise fall back to a short sha256 prefix.
    """

    if _SAFE_SEGMENT_RE.match(value):
        return value
    h = hashlib.sha256(value.encode("utf-8", errors="replace")).hexdigest()
    return f"sha256-{h[:16]}"


@dataclass(frozen=True)
class Workspace:
    """Workspace layout rooted at a deterministic base directory."""

    root: Path

    def project_dir(self, project_id: str) -> Path:
        return self.root / _stable_key(project_id)

    def zip_repo_dir(self, project_id: str) -> Path:
        return self.project_dir(project_id) / "zip" / "repo"

    def git_repo_dir(self, project_id: str, git_url: str, ref: str | None) -> Path:
        key_src = f"{git_url}\n{ref or ''}"
        key = _stable_key(key_src)
        return self.project_dir(project_id) / "git" / key / "repo"

    def ensure_empty_dir(self, path: Path) -> None:
        """Ensure `path` exists and is empty (deterministic materialization)."""

        if path.exists():
            shutil.rmtree(path)
        path.mkdir(parents=True, exist_ok=True)


