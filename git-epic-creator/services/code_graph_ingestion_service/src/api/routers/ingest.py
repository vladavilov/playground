"""Ingestion API endpoints (Task 02 contract only)."""

from __future__ import annotations

from uuid import UUID

from fastapi import APIRouter, File, Form, Request, UploadFile

from api.schemas import IngestGitRequest, IngestResponse, SourceLanguage
from core.orchestrator import ingest_git, ingest_zip
from neo4j_repository_service_client import get_client


router = APIRouter(prefix="/ingest", tags=["Ingestion"])


@router.post("/git", response_model=IngestResponse)
async def ingest_git_endpoint(payload: IngestGitRequest, request: Request) -> IngestResponse:
    repo_fingerprint = ingest_git(
        project_id=payload.project_id,
        source_language=payload.source_language,
        git_url=payload.git_url,
        ref=payload.ref,
        neo4j_client=get_client(),
        postgres_client=getattr(request.app.state, "postgres_client", None),
    )
    return IngestResponse(project_id=payload.project_id, repo_fingerprint=repo_fingerprint)


@router.post("/zip", response_model=IngestResponse)
async def ingest_zip_endpoint(
    request: Request,
    project_id: UUID = Form(...),
    source_language: SourceLanguage = Form(...),
    file: UploadFile = File(...),
) -> IngestResponse:
    zip_bytes = await file.read()
    repo_fingerprint = ingest_zip(
        project_id=project_id,
        source_language=source_language,
        zip_bytes=zip_bytes,
        neo4j_client=get_client(),
        postgres_client=getattr(request.app.state, "postgres_client", None),
    )
    return IngestResponse(project_id=project_id, repo_fingerprint=repo_fingerprint)

