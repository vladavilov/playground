"""Plugin contract types."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Protocol

from core.records import CodeNodeRecord, EdgeRecord


@dataclass(frozen=True)
class IngestionContext:
    project_id: str
    repo_fingerprint: str
    repo_root: Path
    source_language: str  # "cobol" | "java" | "javascript"


class LanguagePlugin(Protocol):
    name: str
    file_globs: tuple[str, ...]

    def iter_files(self, ctx: IngestionContext) -> Iterable[Path]:
        """Return repo-relative source files this plugin owns (must be deterministic)."""

    def ingest(
        self, ctx: IngestionContext, files: list[Path]
    ) -> tuple[list[CodeNodeRecord], list[EdgeRecord], dict]:
        """Produce nodes, edges, and plugin facts (deterministic)."""


