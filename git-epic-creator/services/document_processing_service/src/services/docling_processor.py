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
    VlmPipelineOptions,
    smolvlm_picture_description,
)
from docling.datamodel.pipeline_options_vlm_model import ApiVlmOptions, ResponseFormat
from docling.pipeline.vlm_pipeline import VlmPipeline
import requests

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

    def _create_azure_openai_vlm_options(self) -> ApiVlmOptions:
        """Create Azure OpenAI VLM options for Llama 3.2 Vision or GPT-4o (PRIMARY remote provider)."""
        
        if not self.settings.DOCLING_AZURE_OPENAI_ENDPOINT:
            raise ValueError("DOCLING_AZURE_OPENAI_ENDPOINT is required for azure_openai provider")
        if not self.settings.DOCLING_AZURE_OPENAI_DEPLOYMENT_NAME:
            raise ValueError("DOCLING_AZURE_OPENAI_DEPLOYMENT_NAME is required for azure_openai provider")
        if not self.settings.DOCLING_AZURE_OPENAI_API_KEY:
            raise ValueError("DOCLING_AZURE_OPENAI_API_KEY is required for azure_openai provider")
        
        # Azure OpenAI endpoint format
        base_url = self.settings.DOCLING_AZURE_OPENAI_ENDPOINT.rstrip('/')
        deployment = self.settings.DOCLING_AZURE_OPENAI_DEPLOYMENT_NAME
        api_version = self.settings.DOCLING_AZURE_OPENAI_API_VERSION
        
        url = f"{base_url}/openai/deployments/{deployment}/chat/completions?api-version={api_version}"
        
        # Azure uses api-key header, not Authorization Bearer
        headers = {
            "api-key": self.settings.DOCLING_AZURE_OPENAI_API_KEY
        }
        
        # Map response format string to enum
        response_format = (
            ResponseFormat.DOCTAGS 
            if self.settings.DOCLING_VLM_RESPONSE_FORMAT.upper() == "DOCTAGS"
            else ResponseFormat.MARKDOWN
        )
        
        logger.info("AZURE_OPENAI_VLM_CONFIGURED",
                   endpoint=base_url,
                   deployment=deployment,
                   api_version=api_version,
                   response_format=response_format)
        
        options = ApiVlmOptions(
            url=url,
            params=dict(
                max_tokens=self.settings.DOCLING_VLM_MAX_TOKENS,
                temperature=self.settings.DOCLING_VLM_TEMPERATURE,
            ),
            headers=headers,
            prompt=self.settings.DOCLING_VLM_PROMPT,
            timeout=self.settings.DOCLING_VLM_TIMEOUT,
            scale=float(self.settings.DOCLING_IMAGES_SCALE),
            response_format=response_format,
        )
        
        return options

    def _create_lm_studio_vlm_options(self) -> ApiVlmOptions:
        """Create LM Studio VLM options (OpenAI-compatible local API)."""
        
        headers = {}
        if self.settings.DOCLING_VLM_API_KEY:
            headers["Authorization"] = f"Bearer {self.settings.DOCLING_VLM_API_KEY}"
        
        response_format = (
            ResponseFormat.DOCTAGS 
            if self.settings.DOCLING_VLM_RESPONSE_FORMAT.upper() == "DOCTAGS"
            else ResponseFormat.MARKDOWN
        )
        
        logger.info("LM_STUDIO_VLM_CONFIGURED",
                   endpoint=self.settings.DOCLING_VLM_ENDPOINT,
                   model=self.settings.DOCLING_VLM_MODEL)
        
        options = ApiVlmOptions(
            url=f"{self.settings.DOCLING_VLM_ENDPOINT.rstrip('/')}/v1/chat/completions",
            params=dict(
                model=self.settings.DOCLING_VLM_MODEL,
                max_tokens=self.settings.DOCLING_VLM_MAX_TOKENS,
            ),
            headers=headers,
            prompt=self.settings.DOCLING_VLM_PROMPT,
            timeout=self.settings.DOCLING_VLM_TIMEOUT,
            scale=float(self.settings.DOCLING_IMAGES_SCALE),
            temperature=self.settings.DOCLING_VLM_TEMPERATURE,
            response_format=response_format,
        )
        
        return options

    def _create_ollama_vlm_options(self) -> ApiVlmOptions:
        """Create Ollama VLM options."""
        
        response_format = (
            ResponseFormat.DOCTAGS 
            if self.settings.DOCLING_VLM_RESPONSE_FORMAT.upper() == "DOCTAGS"
            else ResponseFormat.MARKDOWN
        )
        
        logger.info("OLLAMA_VLM_CONFIGURED",
                   endpoint=self.settings.DOCLING_VLM_ENDPOINT,
                   model=self.settings.DOCLING_VLM_MODEL)
        
        options = ApiVlmOptions(
            url=f"{self.settings.DOCLING_VLM_ENDPOINT.rstrip('/')}/v1/chat/completions",
            params=dict(
                model=self.settings.DOCLING_VLM_MODEL,
            ),
            prompt=self.settings.DOCLING_VLM_PROMPT,
            timeout=self.settings.DOCLING_VLM_TIMEOUT,
            scale=1.0,
            response_format=response_format,
        )
        
        return options

    def _create_watsonx_vlm_options(self) -> ApiVlmOptions:
        """Create watsonx.ai VLM options."""
        
        if not self.settings.WX_API_KEY:
            raise ValueError("WX_API_KEY is required for watsonx provider")
        if not self.settings.WX_PROJECT_ID:
            raise ValueError("WX_PROJECT_ID is required for watsonx provider")
        
        def _get_iam_access_token(api_key: str) -> str:
            """Get IBM Cloud IAM access token."""
            res = requests.post(
                url="https://iam.cloud.ibm.com/identity/token",
                headers={
                    "Content-Type": "application/x-www-form-urlencoded",
                },
                data=f"grant_type=urn:ibm:params:oauth:grant-type:apikey&apikey={api_key}",
                timeout=30,
            )
            res.raise_for_status()
            api_out = res.json()
            return api_out["access_token"]
        
        response_format = (
            ResponseFormat.DOCTAGS 
            if self.settings.DOCLING_VLM_RESPONSE_FORMAT.upper() == "DOCTAGS"
            else ResponseFormat.MARKDOWN
        )
        
        logger.info("WATSONX_VLM_CONFIGURED",
                   model=self.settings.DOCLING_VLM_MODEL,
                   project_id=self.settings.WX_PROJECT_ID)
        
        options = ApiVlmOptions(
            url="https://us-south.ml.cloud.ibm.com/ml/v1/text/chat?version=2023-05-29",
            params=dict(
                model_id=self.settings.DOCLING_VLM_MODEL,
                project_id=self.settings.WX_PROJECT_ID,
                parameters=dict(
                    max_new_tokens=self.settings.DOCLING_VLM_MAX_TOKENS,
                ),
            ),
            headers={
                "Authorization": "Bearer " + _get_iam_access_token(api_key=self.settings.WX_API_KEY),
            },
            prompt=self.settings.DOCLING_VLM_PROMPT,
            timeout=self.settings.DOCLING_VLM_TIMEOUT,
            response_format=response_format,
        )
        
        return options

    def _create_openai_compatible_vlm_options(self) -> ApiVlmOptions:
        """Create generic OpenAI-compatible VLM options."""
        
        headers = {}
        if self.settings.DOCLING_VLM_API_KEY:
            headers["Authorization"] = f"Bearer {self.settings.DOCLING_VLM_API_KEY}"
        
        response_format = (
            ResponseFormat.DOCTAGS 
            if self.settings.DOCLING_VLM_RESPONSE_FORMAT.upper() == "DOCTAGS"
            else ResponseFormat.MARKDOWN
        )
        
        logger.info("OPENAI_COMPATIBLE_VLM_CONFIGURED",
                   endpoint=self.settings.DOCLING_VLM_ENDPOINT,
                   model=self.settings.DOCLING_VLM_MODEL)
        
        options = ApiVlmOptions(
            url=f"{self.settings.DOCLING_VLM_ENDPOINT.rstrip('/')}/v1/chat/completions",
            params=dict(
                model=self.settings.DOCLING_VLM_MODEL,
                max_tokens=self.settings.DOCLING_VLM_MAX_TOKENS,
            ),
            headers=headers,
            prompt=self.settings.DOCLING_VLM_PROMPT,
            timeout=self.settings.DOCLING_VLM_TIMEOUT,
            scale=float(self.settings.DOCLING_IMAGES_SCALE),
            temperature=self.settings.DOCLING_VLM_TEMPERATURE,
            response_format=response_format,
        )
        
        return options

    def _get_remote_vlm_options(self) -> ApiVlmOptions:
        """Factory method to get VLM options based on configured provider."""
        
        provider = self.settings.DOCLING_VLM_PROVIDER.lower()
        
        if provider == "azure_openai":
            return self._create_azure_openai_vlm_options()
        elif provider == "lm_studio":
            return self._create_lm_studio_vlm_options()
        elif provider == "ollama":
            return self._create_ollama_vlm_options()
        elif provider == "watsonx":
            return self._create_watsonx_vlm_options()
        elif provider == "openai_compatible":
            return self._create_openai_compatible_vlm_options()
        else:
            raise ValueError(
                f"Unsupported VLM provider: {provider}. "
                f"Supported providers: azure_openai, lm_studio, ollama, watsonx, openai_compatible"
            )

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
        except requests.exceptions.Timeout as timeout_exc:
            # Remote VLM API timeout
            error_msg = f"VLM API timeout after {self.settings.DOCLING_VLM_TIMEOUT}s: {timeout_exc}"
            logger.error("DOCLING_VLM_API_TIMEOUT",
                        file_path=file_path,
                        timeout=self.settings.DOCLING_VLM_TIMEOUT,
                        vlm_mode=self.settings.DOCLING_VLM_MODE,
                        vlm_provider=self.settings.DOCLING_VLM_PROVIDER,
                        endpoint=self.settings.DOCLING_AZURE_OPENAI_ENDPOINT if self.settings.DOCLING_VLM_PROVIDER == "azure_openai" else self.settings.DOCLING_VLM_ENDPOINT,
                        deployment=self.settings.DOCLING_AZURE_OPENAI_DEPLOYMENT_NAME if self.settings.DOCLING_VLM_PROVIDER == "azure_openai" else None,
                        error=str(timeout_exc),
                        exc_info=True)
            return DocumentProcessingResult(success=False, error_message=error_msg)
        except requests.exceptions.HTTPError as http_exc:
            # Remote VLM API HTTP error (401, 403, 429, 500, etc.)
            status_code = getattr(http_exc.response, 'status_code', 'unknown') if hasattr(http_exc, 'response') else 'unknown'
            error_msg = f"VLM API HTTP error (status {status_code}): {http_exc}"
            logger.error("DOCLING_VLM_API_HTTP_ERROR",
                        file_path=file_path,
                        status_code=status_code,
                        vlm_mode=self.settings.DOCLING_VLM_MODE,
                        vlm_provider=self.settings.DOCLING_VLM_PROVIDER,
                        error=str(http_exc),
                        exc_info=True)
            return DocumentProcessingResult(success=False, error_message=error_msg)
        except requests.exceptions.ConnectionError as conn_exc:
            # Remote VLM API connection error (network issues, DNS resolution, etc.)
            error_msg = f"VLM API connection error: {conn_exc}"
            logger.error("DOCLING_VLM_API_CONNECTION_ERROR",
                        file_path=file_path,
                        vlm_mode=self.settings.DOCLING_VLM_MODE,
                        vlm_provider=self.settings.DOCLING_VLM_PROVIDER,
                        endpoint=self.settings.DOCLING_AZURE_OPENAI_ENDPOINT if self.settings.DOCLING_VLM_PROVIDER == "azure_openai" else self.settings.DOCLING_VLM_ENDPOINT,
                        deployment=self.settings.DOCLING_AZURE_OPENAI_DEPLOYMENT_NAME if self.settings.DOCLING_VLM_PROVIDER == "azure_openai" else None,
                        error=str(conn_exc),
                        exc_info=True)
            return DocumentProcessingResult(success=False, error_message=error_msg)
        except requests.exceptions.RequestException as req_exc:
            # Any other requests-related error
            error_msg = f"VLM API request error: {req_exc}"
            logger.error("DOCLING_VLM_API_REQUEST_ERROR",
                        file_path=file_path,
                        vlm_mode=self.settings.DOCLING_VLM_MODE,
                        vlm_provider=self.settings.DOCLING_VLM_PROVIDER,
                        error=str(req_exc),
                        exc_info=True)
            return DocumentProcessingResult(success=False, error_message=error_msg)
        except Exception as exc:
            # Catch-all for unexpected errors (threading issues, C library crashes, memory errors, etc.)
            error_msg = f"Docling processing failed: {type(exc).__name__}: {exc}"
            logger.error("DOCLING_PROCESSING_FAILED",
                        file_path=file_path,
                        file_size_bytes=os.path.getsize(file_path) if os.path.exists(file_path) else 0,
                        vlm_mode=self.settings.DOCLING_VLM_MODE,
                        vlm_provider=self.settings.DOCLING_VLM_PROVIDER,
                        endpoint=self.settings.DOCLING_AZURE_OPENAI_ENDPOINT if self.settings.DOCLING_VLM_PROVIDER == "azure_openai" else self.settings.DOCLING_VLM_ENDPOINT,
                        deployment=self.settings.DOCLING_AZURE_OPENAI_DEPLOYMENT_NAME if self.settings.DOCLING_VLM_PROVIDER == "azure_openai" else None,
                        timeout_config=self.settings.DOCLING_VLM_TIMEOUT,
                        error=str(exc),
                        error_type=type(exc).__name__,
                        error_module=type(exc).__module__,
                        exc_info=True)
            return DocumentProcessingResult(success=False, error_message=error_msg)

    def _build_converter(self) -> DocumentConverter:
        """Create a converter configured for PDFs and images with local or remote VLM support."""
        
        vlm_mode = self.settings.DOCLING_VLM_MODE.lower()
        
        if vlm_mode == "local":
            # LOCAL MODE: Use SmolVLM (current implementation)
            logger.info("DOCLING_VLM_MODE_LOCAL", provider="SmolVLM")
            
            pdf_opts = PdfPipelineOptions()
            pdf_opts.do_ocr = bool(self.settings.DOCLING_USE_OCR)
            pdf_opts.do_picture_description = True
            pdf_opts.picture_description_options = smolvlm_picture_description
            
            # Scale images for better VLM performance
            images_scale = float(self.settings.DOCLING_IMAGES_SCALE)
            pdf_opts.images_scale = images_scale
            pdf_opts.generate_picture_images = True
            
            # Configure OCR languages
            ocr_langs = (self.settings.DOCLING_OCR_LANGS or "").strip()
            pdf_opts.ocr_options.lang = [lang.strip() for lang in ocr_langs.split(",") if lang.strip()]

            converter = DocumentConverter(
                allowed_formats=[InputFormat.PDF, InputFormat.IMAGE],
                format_options={
                    InputFormat.PDF: PdfFormatOption(pipeline_options=pdf_opts),
                    InputFormat.IMAGE: ImageFormatOption(),
                },
            )
            
        elif vlm_mode == "remote":
            # REMOTE MODE: Use API-based VLM via provider factory
            logger.info("DOCLING_VLM_MODE_REMOTE", provider=self.settings.DOCLING_VLM_PROVIDER)
            
            # Get provider-specific VLM options
            vlm_options = self._get_remote_vlm_options()
            
            # Configure VLM pipeline options
            vlm_pipeline_opts = VlmPipelineOptions(
                enable_remote_services=True,
                vlm_options=vlm_options
            )
            
            # Create converter with VLM pipeline
            converter = DocumentConverter(
                allowed_formats=[InputFormat.PDF, InputFormat.IMAGE],
                format_options={
                    InputFormat.PDF: PdfFormatOption(
                        pipeline_cls=VlmPipeline,
                        pipeline_options=vlm_pipeline_opts
                    ),
                    InputFormat.IMAGE: ImageFormatOption(),
                },
            )
            
        else:
            raise ValueError(
                f"Unsupported VLM mode: {vlm_mode}. "
                f"Supported modes: 'local' (SmolVLM), 'remote' (API-based)"
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
