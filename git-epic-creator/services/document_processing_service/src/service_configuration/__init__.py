"""Service-specific configuration module for document processing service."""

from .tika_config import TikaSettings
from .docling_config import DoclingSettings

__all__ = ['TikaSettings', 'DoclingSettings']