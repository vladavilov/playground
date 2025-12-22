"""Service-specific configuration for code graph ingestion service."""

from __future__ import annotations

from functools import lru_cache

from pydantic import Field

from configuration.base_config import BaseConfig


class CodeGraphIngestionSettings(BaseConfig):
    """Settings specific to code graph ingestion service."""

    WORKSPACE_ROOT: str = Field(
        default="workspace",
        description="Deterministic base directory for materialized repos (relative to service root in local runs; /app in container).",
    )

    COBOL_TS_MAX_ERROR_RATIO: float = Field(
        default=0.02,
        description="Max Tree-sitter COBOL parse error ratio allowed before falling back to non-AST unitization.",
        ge=0.0,
        le=1.0,
    )


@lru_cache()
def get_code_graph_ingestion_settings() -> CodeGraphIngestionSettings:
    """Return cached service settings instance."""

    return CodeGraphIngestionSettings()


