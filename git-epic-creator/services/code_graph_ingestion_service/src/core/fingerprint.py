"""Repo fingerprinting.

Simplified version:
- ZIP ingestion: fingerprint is sha256(zip_bytes)
- Git ingestion: fingerprint is the checked-out HEAD commit SHA

We intentionally do *not* attempt to make different ZIPs of the same extracted
content share the same fingerprint, and we do *not* attempt to make ZIP and Git
fingerprints comparable. This keeps the contract simple and predictable.
"""

from __future__ import annotations

import hashlib
from dataclasses import dataclass
from pathlib import Path

def _sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


@dataclass(frozen=True)
class RepoFingerprint:
    value: str
    source_type: str  # "zip" | "git"
    anchor: str  # raw sha (zip) or commit sha (git)


def fingerprint_for_zip_bytes(zip_bytes: bytes) -> RepoFingerprint:
    """Fingerprint a ZIP payload by hashing its raw bytes."""

    sha = _sha256_hex(zip_bytes)
    return RepoFingerprint(value=f"zip:{sha}", source_type="zip", anchor=sha)


def fingerprint_for_git_repo(*, repo_root: Path, head_commit: str) -> RepoFingerprint:
    """Fingerprint a Git checkout by its checked-out HEAD commit SHA."""

    _ = repo_root  # reserved for future "dirty working tree" detection
    return RepoFingerprint(value=f"git:{head_commit}", source_type="git", anchor=head_commit)


