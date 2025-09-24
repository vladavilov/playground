"""Docling-backed processor that handles PDFs and images only.

Selection of which processor to use per file type is done by the calling layer.
"""
from __future__ import annotations

from typing import Dict, Any, Optional, Set
from datetime import datetime
import os
import structlog

from service_configuration.docling_config import DoclingSettings
from dataclasses import dataclass
from docling.document_converter import DocumentConverter

logger = structlog.get_logger(__name__)


class DoclingProcessor:
    """Processor that routes PDFs/images to Docling, others to Tika."""

    def __init__(self, settings: Optional[DoclingSettings] = None) -> None:
        self.settings = settings or DoclingSettings()
        self._image_exts: Set[str] = {
            ext.strip().lower() for ext in self.settings.DOCLING_IMAGE_EXTENSIONS.split(",") if ext.strip()
        }

    def _is_image(self, file_path: str) -> bool:
        return os.path.splitext(file_path)[1].lower() in self._image_exts

    def _is_pdf(self, file_path: str) -> bool:
        return os.path.splitext(file_path)[1].lower() == ".pdf"

    def is_supported_format(self, file_path: str) -> bool:
        # Docling supports PDFs and images only
        return self._is_pdf(file_path) or self._is_image(file_path)

    def extract_text(self, file_path: str) -> str:
        # Delegate non-PDF/non-image to Tika directly
        if not self.is_supported_format(file_path):
            raise DocumentProcessingError("Unsopproted format for Docling")
        result = self.extract_text_with_result(file_path)
        if not result.success:
            raise DocumentProcessingError(result.error_message or "Unknown error")
        return result.extracted_text or ""

    def extract_metadata(self, file_path: str) -> Dict[str, Any]:
        if not os.path.exists(file_path):
            raise DocumentProcessingError(f"File not found: {file_path}")
        if not self.is_supported_format(file_path):
            raise DocumentProcessingError("Unsupported file format for Docling")
        docling_result = self.extract_text_with_result(file_path)
        if docling_result.success:
            return docling_result.metadata or {}
        raise DocumentProcessingError(docling_result.error_message or "Docling failed to extract metadata")

    def process_document(self, file_path: str) -> Dict[str, Any]:
        if not os.path.exists(file_path):
            raise DocumentProcessingError(f"File not found: {file_path}")
        if not self.is_supported_format(file_path):
            raise DocumentProcessingError("Unsupported file format for Docling")
        result = self.extract_text_with_result(file_path)
        if not result.success:
            raise DocumentProcessingError(result.error_message or "Unknown error")
        return {
            'file_path': file_path,
            'file_type': result.file_type or 'unknown',
            'extracted_text': result.extracted_text or '',
            'text_length': len(result.extracted_text or ''),
            'metadata': result.metadata or {},
            'processing_timestamp': datetime.utcnow().isoformat(),
            'processor_version': 'docling-1.0'
        }

    def extract_text_with_result(self, file_path: str) -> DocumentProcessingResult:
        if not os.path.exists(file_path):
            return DocumentProcessingResult(success=False, error_message=f"File not found: {file_path}")

        if not self.settings.DOCLING_ENABLED:
            return DocumentProcessingResult(success=False, error_message="Docling disabled")

        if not self.is_supported_format(file_path):
            raise DocumentProcessingError("Unsupported format for Docling")

        try:
            start_time = datetime.utcnow()


            converter = DocumentConverter()
            result = converter.convert(file_path)

            # Prefer Markdown export as unified plain text representation
            extracted_text = (result.document.export_to_markdown() or "").strip()
            metadata: Dict[str, Any] = {}
            try:
                meta_obj = getattr(result.document, 'metadata', None)
                if isinstance(meta_obj, dict):
                    metadata.update(meta_obj)
            except Exception:
                pass

            processing_time = (datetime.utcnow() - start_time).total_seconds()
            file_type = 'application/pdf' if self._is_pdf(file_path) else 'image/*'

            logger.info("DOCLING PROCESSING COMPLETED",
                        file_path=file_path,
                        text_length=len(extracted_text),
                        processing_time=processing_time)

            return DocumentProcessingResult(
                extracted_text=extracted_text,
                file_type=file_type,
                page_count=None,
                metadata=metadata,
                success=True,
                processing_time=processing_time,
            )
        except Exception as exc:
            logger.warning("Docling processing failed", file_path=file_path, error=str(exc))
            return DocumentProcessingResult(success=False, error_message=f"Docling failed: {exc}")


class DocumentProcessingError(Exception):
    """Custom exception for document processing errors (Docling)."""


@dataclass
class DocumentProcessingResult:
    extracted_text: Optional[str] = None
    file_type: Optional[str] = None
    page_count: Optional[int] = None
    metadata: Optional[Dict[str, Any]] = None
    success: bool = False
    processing_time: Optional[float] = None
    error_message: Optional[str] = None
