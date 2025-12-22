"""Postgres repo_index.json store (Task 09).

Stores a deterministic project manifest keyed by (project_id, repo_fingerprint).
"""

from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Any
from uuid import UUID

from models.project_db import ProjectRepoIndex
from utils.postgres_client import PostgresClient


def canonical_json_bytes(obj: Any) -> bytes:
    """Canonical JSON bytes (sorted keys, no whitespace)."""

    return json.dumps(obj, sort_keys=True, separators=(",", ":"), ensure_ascii=False).encode("utf-8")


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


@dataclass(frozen=True)
class RepoIndexUpsertResult:
    content_sha256: str


def upsert_repo_index(
    *,
    postgres_client: PostgresClient,
    project_id: UUID,
    repo_fingerprint: str,
    repo_index_json: dict,
    created_by: str = "code-graph-ingestion-service",
) -> RepoIndexUpsertResult:
    """Upsert deterministic `repo_index_json` into Postgres."""

    canonical = canonical_json_bytes(repo_index_json)
    content_sha = sha256_hex(canonical)

    with postgres_client.get_sync_session() as session:
        existing = (
            session.query(ProjectRepoIndex)
            .filter(ProjectRepoIndex.project_id == project_id)
            .filter(ProjectRepoIndex.repo_fingerprint == repo_fingerprint)
            .one_or_none()
        )

        if existing is None:
            row = ProjectRepoIndex(
                project_id=project_id,
                repo_fingerprint=repo_fingerprint,
                repo_index_json=repo_index_json,
                content_sha256=content_sha,
                created_by=created_by,
            )
            session.add(row)
        else:
            existing.repo_index_json = repo_index_json
            existing.content_sha256 = content_sha
            existing.updated_at = datetime.now(timezone.utc)

    return RepoIndexUpsertResult(content_sha256=content_sha)


