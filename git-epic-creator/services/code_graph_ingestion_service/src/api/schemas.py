"""Pydantic request/response models for ingestion API."""

from __future__ import annotations

import hashlib
from typing import Literal
from uuid import UUID

from pydantic import BaseModel, Field


SourceLanguage = Literal["cobol", "java", "javascript"]


class IngestResponse(BaseModel):
    project_id: UUID
    repo_fingerprint: str = Field(min_length=1)


class IngestGitRequest(BaseModel):
    project_id: UUID
    source_language: SourceLanguage
    git_url: str = Field(min_length=1)
    ref: str | None = None
