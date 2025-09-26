"""Docling-backed processor that handles PDFs and images only.

Selection of which processor to use per file type is done by the calling layer.
"""
from __future__ import annotations

from typing import Dict, Any, Optional, Set, List
from datetime import datetime, timezone
import os
import structlog

from service_configuration.docling_config import DoclingSettings
from dataclasses import dataclass
from docling.document_converter import (
    DocumentConverter,
    PdfFormatOption,
    ImageFormatOption,
)
from docling.datamodel.base_models import InputFormat
from docling.datamodel.pipeline_options import (
    PdfPipelineOptions,
    smolvlm_picture_description,
)

# Markdown serialization with picture descriptions
from docling_core.transforms.serializer.base import (
    BaseDocSerializer,
    SerializationResult,
)
from docling_core.transforms.serializer.common import create_ser_result
from docling_core.transforms.serializer.markdown import (
    MarkdownDocSerializer,
    MarkdownPictureSerializer,
)
from docling_core.types.doc.document import (
    DoclingDocument,
    PictureItem,
    PictureDescriptionData,
)

logger = structlog.get_logger(__name__)


# Constants
UNSUPPORTED_ERR = "Unsupported format for Docling"
PROCESSOR_VERSION = "docling-1.0"


# Reusable Markdown picture serializer that appends picture descriptions
class _AnnotationPictureSerializer(MarkdownPictureSerializer):
    def serialize(
        self,
        *,
        item: PictureItem,
        doc_serializer: BaseDocSerializer,
        doc: DoclingDocument,
        **kwargs: Any,
    ) -> SerializationResult:
        base_res = super().serialize(item=item, doc_serializer=doc_serializer, doc=doc, **kwargs)
        parts: List[str] = [base_res.text or ""]
        for annotation in getattr(item, "annotations", []) or []:
            if isinstance(annotation, PictureDescriptionData):
                parts.append(f"\n\n{annotation.text}\n")
        text = doc_serializer.post_process(text="".join(parts))
        return create_ser_result(text=text, span_source=item)


_ANNOTATION_PICTURE_SERIALIZER = _AnnotationPictureSerializer()


class DoclingProcessor:
    """Processor that routes PDFs/images to Docling, others to Tika."""

    def __init__(self, settings: Optional[DoclingSettings] = None) -> None:
        self.settings = settings or DoclingSettings()
        self._image_exts: Set[str] = {
            ext.strip().lower() for ext in self.settings.DOCLING_IMAGE_EXTENSIONS.split(",") if ext.strip()
        }

        # Build a single DocumentConverter for PDFs and Images with SmolVLM-based picture description
        self._converter = self._build_converter()

    def _is_image(self, file_path: str) -> bool:
        return os.path.splitext(file_path)[1].lower() in self._image_exts

    def _is_pdf(self, file_path: str) -> bool:
        return os.path.splitext(file_path)[1].lower() == ".pdf"

    def is_supported_format(self, file_path: str) -> bool:
        # Docling supports PDFs and images only
        return self._is_pdf(file_path) or self._is_image(file_path)

    def extract_text(self, file_path: str) -> str:
        result = self.extract_text_with_result(file_path)
        if not result.success:
            raise RuntimeError(result.error_message or "Unknown error")
        return result.extracted_text or ""

    def extract_metadata(self, file_path: str) -> Dict[str, Any]:
        result = self.extract_text_with_result(file_path)
        if result.success:
            return result.metadata or {}
        raise RuntimeError(result.error_message or "Docling failed to extract metadata")

    def process_document(self, file_path: str) -> Dict[str, Any]:
        result = self.extract_text_with_result(file_path)
        if not result.success:
            raise RuntimeError(result.error_message or "Unknown error")
        return {
            'file_path': file_path,
            'file_type': result.file_type or 'unknown',
            'extracted_text': result.extracted_text or '',
            'text_length': len(result.extracted_text or ''),
            'metadata': result.metadata or {},
            'processing_timestamp': datetime.now(timezone.utc).isoformat(),
            'processor_version': PROCESSOR_VERSION
        }

    def extract_text_with_result(self, file_path: str) -> DocumentProcessingResult:
        if not os.path.exists(file_path):
            return DocumentProcessingResult(success=False, error_message=f"File not found: {file_path}")

        if not self.is_supported_format(file_path):
            return DocumentProcessingResult(success=False, error_message=UNSUPPORTED_ERR)

        try:
            start_time = datetime.now(timezone.utc)

            result = self._converter.convert(file_path)

            # Prefer Markdown export as unified plain text representation
            extracted_text = self._export_markdown_with_descriptions(result.document)
            meta_obj = getattr(result.document, 'metadata', None)
            metadata: Dict[str, Any] = meta_obj if isinstance(meta_obj, dict) else {}

            processing_time = (datetime.now(timezone.utc) - start_time).total_seconds()
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

    def _build_converter(self) -> DocumentConverter:
        """Create a converter configured for PDFs and images with OCR and SmolVLM picture descriptions."""
        pdf_opts = PdfPipelineOptions()
        # Always enable OCR and picture descriptions for PDFs
        pdf_opts.do_ocr = bool(self.settings.DOCLING_USE_OCR)
        pdf_opts.do_picture_description = True
        pdf_opts.picture_description_options = smolvlm_picture_description
        # Scale images for better VLM performance and export pictures
        images_scale = float(self.settings.DOCLING_IMAGES_SCALE)
        pdf_opts.images_scale = images_scale
        pdf_opts.generate_picture_images = True
        ocr_langs = (self.settings.DOCLING_OCR_LANGS or "").strip()
        pdf_opts.ocr_options.lang = [lang.strip() for lang in ocr_langs.split(",") if lang.strip()]

        converter = DocumentConverter(
            allowed_formats=[InputFormat.PDF, InputFormat.IMAGE],
            format_options={
                InputFormat.PDF: PdfFormatOption(pipeline_options=pdf_opts),
                InputFormat.IMAGE: ImageFormatOption(),
            },
        )
        return converter

    def _export_markdown_with_descriptions(self, doc: DoclingDocument) -> str:
        """Serialize to Markdown ensuring picture descriptions are included."""
        serializer = MarkdownDocSerializer(doc=doc, picture_serializer=_ANNOTATION_PICTURE_SERIALIZER)
        try:
            ser_res = serializer.serialize()
            return (ser_res.text or "").strip()
        except Exception:
            # Fallback to built-in markdown export if custom serialization fails
            return (getattr(doc, "export_to_markdown", lambda: "")() or "").strip()


@dataclass
class DocumentProcessingResult:
    extracted_text: Optional[str] = None
    file_type: Optional[str] = None
    page_count: Optional[int] = None
    metadata: Optional[Dict[str, Any]] = None
    success: bool = False
    processing_time: Optional[float] = None
    error_message: Optional[str] = None
