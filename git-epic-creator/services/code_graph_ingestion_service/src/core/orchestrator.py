"""Ingestion orchestrator.

Pipeline:
materialize -> fingerprint -> inventory -> run selected plugin -> persist -> upsert repo_index
"""

from __future__ import annotations

from datetime import datetime, timezone
from pathlib import Path
from uuid import UUID

import structlog

from api.schemas import SourceLanguage
from config import get_code_graph_ingestion_settings
from core.chunking import chunk_oversized_nodes
from core.graph_contract import validate_graph_contract
from core.fingerprint import fingerprint_for_git_repo, fingerprint_for_zip_bytes
from core.ignore_rules import build_ignore_rules
from core.inventory import InventoryEntry, build_inventory
from core.repo_materializer import materialize_git, materialize_zip_bytes
from core.workspace import Workspace
from persistence.neo4j_writer import (
    code_node_rows,
    edge_rows_by_type,
)
from persistence.repo_index_store import upsert_repo_index
from plugins.base import IngestionContext
from plugins.registry import run_selected_plugin
from utils.postgres_client import PostgresClient
from observability.tracing import get_tracer
from neo4j_repository_service_client import post_json
import httpx


logger = structlog.get_logger(__name__)
tracer = get_tracer(__name__)


def _workspace() -> Workspace:
    settings = get_code_graph_ingestion_settings()
    root = Path(settings.WORKSPACE_ROOT)
    return Workspace(root=root)


def _file_rows(project_id: str, repo_fingerprint: str, files: list[InventoryEntry]) -> list[dict]:
    return [
        {
            "project_id": project_id,
            "repo_fingerprint": repo_fingerprint,
            "file_path": f.path,
            "sha256": f.sha256,
            "language": f.language,
            "line_count": f.line_count,
        }
        for f in files
    ]


def _persist_to_neo4j(
    *,
    neo4j_client: httpx.Client | None,
    project_id: str,
    repo_fingerprint: str,
    files: list[InventoryEntry],
    nodes: list,
    edges: list,
) -> None:
    if neo4j_client is None or not isinstance(neo4j_client, httpx.Client):
        return

    post_json(neo4j_client, "/v1/code-graph/merge-project", {"project_id": project_id})
    post_json(
        neo4j_client,
        "/v1/code-graph/merge-repo",
        {"project_id": project_id, "repo_fingerprint": repo_fingerprint},
    )
    post_json(
        neo4j_client,
        "/v1/code-graph/merge-files",
        {"rows": _file_rows(project_id, repo_fingerprint, files)},
    )

    if nodes:
        post_json(
            neo4j_client,
            "/v1/code-graph/merge-code-nodes",
            {"rows": code_node_rows(nodes)},
        )
    if edges:
        for rel_type, rows in edge_rows_by_type(edges).items():
            post_json(
                neo4j_client,
                f"/v1/code-graph/merge-edges/{rel_type}",
                {"rows": rows},
            )


def _persist_to_postgres(
    *,
    postgres_client: PostgresClient | None,
    project_id: UUID,
    repo_fingerprint: str,
    repo_index_json: dict,
) -> None:
    if postgres_client is None or not isinstance(postgres_client, PostgresClient):
        return
    upsert_repo_index(
        postgres_client=postgres_client,
        project_id=project_id,
        repo_fingerprint=repo_fingerprint,
        repo_index_json=repo_index_json,
    )


def ingest_zip(
    *,
    project_id: UUID,
    source_language: SourceLanguage,
    zip_bytes: bytes,
    neo4j_client: httpx.Client | None = None,
    postgres_client: PostgresClient | None = None,
) -> str:
    ws = _workspace()
    repo_root = ws.zip_repo_dir(str(project_id))
    with tracer.start_as_current_span("ingest_zip") as span:
        span.set_attribute("project_id", str(project_id))
        span.set_attribute("source_language", source_language)
        logger.info("ingest_zip.start", project_id=str(project_id), source_language=source_language)

        with tracer.start_as_current_span("materialize.zip"):
            materialize_zip_bytes(zip_bytes=zip_bytes, dest_dir=repo_root)

        with tracer.start_as_current_span("fingerprint.zip"):
            fp = fingerprint_for_zip_bytes(zip_bytes)
            repo_fingerprint = fp.value
            span.set_attribute("repo_fingerprint", repo_fingerprint)
            logger.info("ingest_zip.fingerprint", project_id=str(project_id), repo_fingerprint=repo_fingerprint)

        with tracer.start_as_current_span("inventory"):
            ignore = build_ignore_rules(repo_root)
            files = build_inventory(repo_root, ignore=ignore)

        with tracer.start_as_current_span("plugin"):
            ctx = IngestionContext(
                project_id=str(project_id),
                repo_fingerprint=repo_fingerprint,
                repo_root=repo_root,
                source_language=source_language,
            )
            plugins = _default_plugins()
            nodes, edges, plugin_facts = run_selected_plugin(ctx, plugins)
    chunked = chunk_oversized_nodes(nodes)
    nodes = chunked.nodes
    edges = edges + chunked.edges
    validate_graph_contract(
        project_id=str(project_id),
        repo_fingerprint=repo_fingerprint,
        files=files,
        nodes=nodes,
        edges=edges,
    )

    _persist_to_neo4j(
        neo4j_client=neo4j_client,
        project_id=str(project_id),
        repo_fingerprint=repo_fingerprint,
        files=files,
        nodes=nodes,
        edges=edges,
    )

    repo_index_json = {
        "project_id": str(project_id),
        "repo_fingerprint": repo_fingerprint,
        "created_at": datetime.now(timezone.utc).isoformat(),
        "files": [f.__dict__ for f in files],
        "facts": plugin_facts,
    }
    _persist_to_postgres(
        postgres_client=postgres_client,
        project_id=project_id,
        repo_fingerprint=repo_fingerprint,
        repo_index_json=repo_index_json,
    )
    logger.info("ingest_zip.done", project_id=str(project_id), repo_fingerprint=repo_fingerprint)
    return repo_fingerprint


def ingest_git(
    *,
    project_id: UUID,
    source_language: SourceLanguage,
    git_url: str,
    ref: str | None,
    neo4j_client: httpx.Client | None = None,
    postgres_client: PostgresClient | None = None,
) -> str:
    ws = _workspace()
    repo_root = ws.git_repo_dir(str(project_id), git_url, ref)
    with tracer.start_as_current_span("ingest_git") as span:
        span.set_attribute("project_id", str(project_id))
        span.set_attribute("source_language", source_language)
        span.set_attribute("git_url", git_url)
        if ref:
            span.set_attribute("ref", ref)
        logger.info("ingest_git.start", project_id=str(project_id), source_language=source_language, git_url=git_url, ref=ref)

        with tracer.start_as_current_span("materialize.git"):
            res = materialize_git(git_url=git_url, ref=ref, dest_dir=repo_root)
            if not res.head_commit:
                raise ValueError("Could not determine HEAD commit for git materialization")

        with tracer.start_as_current_span("fingerprint.git"):
            fp = fingerprint_for_git_repo(repo_root=repo_root, head_commit=res.head_commit)
            repo_fingerprint = fp.value
            span.set_attribute("repo_fingerprint", repo_fingerprint)
            logger.info("ingest_git.fingerprint", project_id=str(project_id), repo_fingerprint=repo_fingerprint, head_commit=res.head_commit)

        with tracer.start_as_current_span("inventory"):
            ignore = build_ignore_rules(repo_root)
            files = build_inventory(repo_root, ignore=ignore)

        with tracer.start_as_current_span("plugin"):
            ctx = IngestionContext(
                project_id=str(project_id),
                repo_fingerprint=repo_fingerprint,
                repo_root=repo_root,
                source_language=source_language,
            )
            plugins = _default_plugins()
            nodes, edges, plugin_facts = run_selected_plugin(ctx, plugins)
    chunked = chunk_oversized_nodes(nodes)
    nodes = chunked.nodes
    edges = edges + chunked.edges
    validate_graph_contract(
        project_id=str(project_id),
        repo_fingerprint=repo_fingerprint,
        files=files,
        nodes=nodes,
        edges=edges,
    )

    _persist_to_neo4j(
        neo4j_client=neo4j_client,
        project_id=str(project_id),
        repo_fingerprint=repo_fingerprint,
        files=files,
        nodes=nodes,
        edges=edges,
    )
    repo_index_json = {
        "project_id": str(project_id),
        "repo_fingerprint": repo_fingerprint,
        "created_at": datetime.now(timezone.utc).isoformat(),
        "files": [f.__dict__ for f in files],
        "facts": plugin_facts,
    }
    _persist_to_postgres(
        postgres_client=postgres_client,
        project_id=project_id,
        repo_fingerprint=repo_fingerprint,
        repo_index_json=repo_index_json,
    )
    logger.info("ingest_git.done", project_id=str(project_id), repo_fingerprint=repo_fingerprint)
    return repo_fingerprint


def _default_plugins():
    # Local import to avoid import-time optional dependencies for future plugins.
    from plugins.cobol.plugin import CobolPlugin
    from plugins.java.plugin import JavaPlugin
    from plugins.javascript.plugin import JavaScriptPlugin

    return [CobolPlugin(), JavaPlugin(), JavaScriptPlugin()]


