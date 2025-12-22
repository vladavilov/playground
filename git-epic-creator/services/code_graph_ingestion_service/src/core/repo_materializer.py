"""Deterministic repo materialization (Task 03).

- ZIP: safe, deterministic extraction (no zip-slip)
- Git: clone + optional ref checkout (dulwich; deterministic workspace path)
"""

from __future__ import annotations

import io
import os
import posixpath
import shutil
import stat
import time
import zipfile
from dataclasses import dataclass
from pathlib import Path

import safezipfile
from dulwich import porcelain
from dulwich.repo import Repo


class ZipSlipError(ValueError):
    """Raised when a ZIP entry would escape the destination directory."""


class UnsafeZipEntryError(ValueError):
    """Raised when a ZIP entry is unsafe (e.g., symlink)."""


def _is_symlink(info: zipfile.ZipInfo) -> bool:
    # External attributes are platform-specific; this is best-effort and safe.
    # When created on Unix, the high 16 bits encode st_mode.
    mode = (info.external_attr >> 16) & 0xFFFF
    return stat.S_IFMT(mode) == stat.S_IFLNK


def _normalized_zip_relpath(name: str) -> str:
    """Normalize a ZIP entry name into a safe POSIX relative path."""

    # ZIP spec uses forward slashes, but Windows tools may emit backslashes.
    p = name.replace("\\", "/")
    p = posixpath.normpath(p)

    # Drop leading "./"
    if p.startswith("./"):
        p = p[2:]

    # Reject absolute paths and drive-letter-ish paths.
    if p.startswith("/") or p.startswith("\\") or ":" in p.split("/", 1)[0]:
        raise ZipSlipError(f"Unsafe absolute path in zip entry: {name!r}")

    parts = [part for part in p.split("/") if part not in ("", ".")]
    if any(part == ".." for part in parts):
        raise ZipSlipError(f"Unsafe parent traversal in zip entry: {name!r}")

    rel = "/".join(parts)
    if rel in ("", "."):
        return ""
    return rel


def materialize_zip_bytes(*, zip_bytes: bytes, dest_dir: Path) -> Path:
    """Extract a ZIP payload deterministically into `dest_dir`."""

    # Deterministic: ensure the output directory is empty.
    if dest_dir.exists():
        _rmtree_retry(dest_dir)
    dest_dir.mkdir(parents=True, exist_ok=True)

    # Use a hardened ZipFile implementation to mitigate zip-slip/zip-bomb risks.
    # We still enforce deterministic ordering + explicit symlink rejection.
    with safezipfile.ZipFile(io.BytesIO(zip_bytes)) as zf:
        infos = list(zf.infolist())

        # Deterministic extraction order: sort by normalized repo-relative path.
        def sort_key(info: zipfile.ZipInfo) -> str:
            try:
                return _normalized_zip_relpath(info.filename)
            except Exception:
                # Still stable: fall back to raw filename so failures are deterministic.
                return info.filename.replace("\\", "/")

        ordered: list[zipfile.ZipInfo] = []
        for info in sorted(infos, key=sort_key):
            rel = _normalized_zip_relpath(info.filename)
            if rel == "":
                continue

            if _is_symlink(info):
                raise UnsafeZipEntryError(f"Symlinks are not allowed in zip: {info.filename!r}")
            ordered.append(info)

        # Note: safezipfile adds safety limits (max_files/max_total_size/etc) in extractall().
        # Passing `members=` preserves our deterministic extraction order.
        zf.extractall(path=dest_dir, members=ordered)

    return dest_dir


@dataclass(frozen=True)
class GitMaterializationResult:
    repo_root: Path
    head_commit: str | None


def materialize_git(
    *,
    git_url: str,
    ref: str | None,
    dest_dir: Path,
) -> GitMaterializationResult:
    """Clone a git repo into `dest_dir` and optionally checkout `ref`."""

    # Deterministic: ensure the output directory is empty.
    if dest_dir.exists():
        _rmtree_retry(dest_dir)
    dest_dir.parent.mkdir(parents=True, exist_ok=True)

    # Keep clone deterministic and quiet.
    err = io.BytesIO()

    porcelain.clone(git_url, target=str(dest_dir), checkout=True, errstream=err)

    if ref:
        # Accept branch name, tag, or commit-ish. `force=True` keeps re-runs stable.
        porcelain.checkout(str(dest_dir), ref, force=True)

    head_commit: str | None = None
    try:
        repo = Repo(str(dest_dir))
        head = repo.head()  # bytes (hex sha) when present
        if isinstance(head, (bytes, bytearray)):
            head_commit = head.decode("ascii", errors="replace")
        else:
            head_commit = str(head)
    except Exception:
        # Materialization succeeded; HEAD might be absent for unusual repos.
        head_commit = None

    return GitMaterializationResult(repo_root=dest_dir, head_commit=head_commit)


def _rmtree_retry(path: Path, *, attempts: int = 30, delay_sec: float = 0.1) -> None:
    """Robust rmtree for Windows file-lock flakiness."""

    last_err: Exception | None = None

    def _onerror(func, p, exc_info):
        # Try to make the path writable and retry once.
        os.chmod(p, stat.S_IWRITE)
        func(p)

    for _ in range(attempts):
        try:
            shutil.rmtree(path, onerror=_onerror)
            return
        except Exception as e:
            last_err = e
            time.sleep(delay_sec)
    if last_err:
        raise last_err


