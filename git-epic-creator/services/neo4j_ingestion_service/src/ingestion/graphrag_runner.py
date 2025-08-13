"""
GraphRAG CLI runner utilities.

Provides per-project workspace isolation and safe CLI invocation.
"""

from __future__ import annotations

import os
import subprocess
from pathlib import Path

from configuration.common_config import get_app_settings

class GraphRAGIndexError(RuntimeError):
    """Raised when GraphRAG indexing fails with a non-zero exit code."""

def _get_workspace_root() -> Path:
    # Allow test-time env overrides to take precedence without requiring
    # global settings cache invalidation between tests.
    env_root = os.getenv("RAG_WORKSPACE_ROOT")
    if env_root:
        return Path(env_root)
    settings = get_app_settings()
    return Path(settings.graphrag.RAG_WORKSPACE_ROOT)


def _ensure_workspace_initialized(workdir: Path) -> None:
    workdir.mkdir(parents=True, exist_ok=True)
    # Idempotent: running init repeatedly should be safe according to CLI
    env_vars = {
        **os.environ,
        # Pass through concurrency hints if configured
        "GRAPHRAG_LLM_THREAD_COUNT": str(get_app_settings().graphrag.GRAPHRAG_LLM_THREAD_COUNT),
        "GRAPHRAG_EMBEDDING_THREAD_COUNT": str(get_app_settings().graphrag.GRAPHRAG_EMBEDDING_THREAD_COUNT),
    }
    completed = subprocess.run(
        ["graphrag", "init", "--root", str(workdir)],
        capture_output=True,
        text=True,
        check=False,
        env=env_vars,
    )
    # Some versions of the CLI return success even if already initialized.
    # If it fails, we surface the error early for visibility.
    if completed.returncode != 0:
        raise GraphRAGIndexError(
            f"GraphRAG init failed (code={completed.returncode}): {completed.stderr or completed.stdout}"
        )


def run_index(project_id: str) -> Path:
    """
    Initialize the project workspace if needed and invoke GraphRAG index.

    Returns the project workdir path.
    """
    if not isinstance(project_id, str) or not project_id.strip():
        raise ValueError("project_id must be a non-empty string")

    root = _get_workspace_root()
    workdir = root / project_id

    _ensure_workspace_initialized(workdir)

    # Ensure settings.yaml points to relative input/output within WORKDIR
    settings_path = workdir / "settings.yaml"
    project_input = f"input/{project_id}"
    project_output = f"output/{project_id}"
    settings_contents = (
        "input:\n"
        f"  file_pattern: {project_input}\n"
        "output:\n"
        f"  path: {project_output}\n"
        "storage:\n"
        "  base_dir: output\n"
        "reporting:\n"
        "  base_dir: output\n"
    )
    try:
        settings_path.write_text(settings_contents, encoding="utf-8")
    except Exception:
        # Non-fatal; CLI defaults may already be relative
        pass

    env_vars = {
        **os.environ,
        "GRAPHRAG_LLM_THREAD_COUNT": str(get_app_settings().graphrag.GRAPHRAG_LLM_THREAD_COUNT),
        "GRAPHRAG_EMBEDDING_THREAD_COUNT": str(get_app_settings().graphrag.GRAPHRAG_EMBEDDING_THREAD_COUNT),
    }

    completed = subprocess.run(
        ["graphrag", "index", "--root", str(workdir)],
        capture_output=True,
        text=True,
        check=False,
        env=env_vars,
    )
    if completed.returncode != 0:
        raise GraphRAGIndexError(
            f"GraphRAG index failed (code={completed.returncode}): {completed.stderr or completed.stdout}"
        )

    return workdir
