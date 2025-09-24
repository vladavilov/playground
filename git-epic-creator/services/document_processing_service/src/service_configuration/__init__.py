"""Service-specific configuration module for document processing service."""

from .tika_config import TikaSettings

try:
    from .docling_config import DoclingSettings  # type: ignore
except Exception:  # pragma: no cover - optional during migration
    DoclingSettings = None  # type: ignore

__all__ = ['TikaSettings', 'DoclingSettings']