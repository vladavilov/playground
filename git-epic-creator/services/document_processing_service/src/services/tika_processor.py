"""
Apache Tika document processor for multi-format text extraction.
Supports PDF, DOCX, XLSX, TXT formats with structured JSON output.
"""

from typing import Dict, Any, Optional
from datetime import datetime, timezone
import os
import structlog
from dataclasses import dataclass

from tika import parser


logger = structlog.get_logger(__name__)


class DocumentProcessingError(Exception):
    """Custom exception for document processing errors."""


@dataclass
class DocumentProcessingResult:
    """Result of document processing operation."""
    extracted_text: Optional[str] = None
    file_type: Optional[str] = None
    page_count: Optional[int] = None
    metadata: Optional[Dict[str, Any]] = None
    success: bool = False
    processing_time: Optional[float] = None
    error_message: Optional[str] = None

    def to_structured_json(self) -> Dict[str, Any]:
        """Convert result to structured JSON format."""
        return {
            'extracted_text': self.extracted_text,
            'metadata': self.metadata or {},
            'file_info': {
                'file_type': self.file_type,
                'page_count': self.page_count,
                'processing_time': self.processing_time
            },
            'success': self.success,
            'error_message': self.error_message
        }


class TikaProcessor:
    """
    Apache Tika document processor for extracting text and metadata from various document formats.
    Supports PDF, DOCX, XLSX, TXT formats as specified in Requirements 6.3 and 10.2.
    """

    def __init__(self):
        """Initialize the Tika processor."""
        self.supported_formats = {'.pdf', '.docx', '.doc', '.xlsx', '.xls', '.txt'}
        logger.info("TikaProcessor initialized", supported_formats=list(self.supported_formats))

    def process_document(self, file_path: str) -> Dict[str, Any]:
        """
        Process a document and return structured JSON output.
        Implements Requirements 6.3 and 10.2 for structured document processing.

        Args:
            file_path: Path to the document file

        Returns:
            Dict[str, Any]: Structured document processing result

        Raises:
            DocumentProcessingError: If document processing fails
        """
        if not os.path.exists(file_path):
            raise DocumentProcessingError(f"File not found: {file_path}")
        if not self.is_supported_format(file_path):
            raise DocumentProcessingError(f"Unsupported file format: {file_path}")

        try:
            logger.info("Processing document", file_path=file_path)

            # Extract both text and metadata
            with open(file_path, 'rb') as file:
                parsed = parser.from_buffer(file.read())

            extracted_text = (parsed.get('content', '') or '').strip()
            metadata = parsed.get('metadata', {}) or {}

            # Create structured output
            result = {
                'file_path': file_path,
                'file_type': metadata.get('Content-Type', 'unknown'),
                'extracted_text': extracted_text,
                'text_length': len(extracted_text),
                'metadata': metadata,
                'processing_timestamp': datetime.now(timezone.utc).isoformat(),
                'processor_version': 'tika-1.0'
            }

            logger.info("Document processing completed",
                       file_path=file_path,
                       file_type=result['file_type'],
                       text_length=result['text_length'],
                       metadata_keys=list(metadata.keys()))

            return result

        except Exception as e:
            logger.error("Error processing document",
                        file_path=file_path,
                        error=str(e))
            raise DocumentProcessingError(f"Error processing document {file_path}: {str(e)}") from e

    def is_supported_format(self, file_path: str) -> bool:
        """
        Check if the file format is supported.

        Args:
            file_path: Path to the document file

        Returns:
            bool: True if format is supported, False otherwise
        """
        file_extension = os.path.splitext(file_path)[1].lower()
        return file_extension in self.supported_formats

    def extract_text(self, file_path: str) -> str:
        """
        Extract text from a document file.

        Args:
            file_path: Path to the document file

        Returns:
            str: Extracted text content

        Raises:
            DocumentProcessingError: If text extraction fails
        """
        if not os.path.exists(file_path):
            raise DocumentProcessingError(f"File not found: {file_path}")

        try:
            logger.info("Extracting text from document", file_path=file_path)

            with open(file_path, 'rb') as file:
                parsed = parser.from_buffer(file.read())

            extracted_text = (parsed.get('content', '') or '').strip()
            
            logger.info("Text extraction completed",
                       file_path=file_path,
                       text_length=len(extracted_text))

            return extracted_text

        except Exception as e:
            logger.error("Error extracting text from document",
                        file_path=file_path,
                        error=str(e))
            raise DocumentProcessingError(f"Error processing document {file_path}: {str(e)}") from e

    def extract_text_with_result(self, file_path: str) -> DocumentProcessingResult:
        """
        Extract text from a document file and return detailed result.

        Args:
            file_path: Path to the document file

        Returns:
            DocumentProcessingResult: Processing result with extracted text
        """
        if not os.path.exists(file_path):
            return DocumentProcessingResult(
                success=False,
                error_message=f"File not found: {file_path}"
            )

        try:
            start_time = datetime.now()
            logger.info("Extracting text from document", file_path=file_path)

            with open(file_path, 'rb') as file:
                parsed = parser.from_buffer(file.read())

            extracted_text = (parsed.get('content', '') or '').strip()
            metadata = parsed.get('metadata', {}) or {}
            processing_time = (datetime.now() - start_time).total_seconds()
            
            # Extract page count from metadata if available
            page_count = None
            if 'Page-Count' in metadata:
                try:
                    page_count = int(metadata['Page-Count'])
                except (ValueError, TypeError):
                    pass
            
            logger.info("Text extraction completed",
                       file_path=file_path,
                       text_length=len(extracted_text))

            return DocumentProcessingResult(
                extracted_text=extracted_text,
                file_type=metadata.get('Content-Type', 'unknown'),
                page_count=page_count,
                metadata=metadata,
                success=True,
                processing_time=processing_time
            )

        except Exception as e:
            logger.error("Error extracting text from document",
                        file_path=file_path,
                        error=str(e))
            return DocumentProcessingResult(
                success=False,
                error_message=f"Error processing document {file_path}: {str(e)}"
            )

    def extract_metadata(self, file_path: str) -> Dict[str, Any]:
        """
        Extract metadata from a document file.

        Args:
            file_path: Path to the document file

        Returns:
            Dict[str, Any]: Document metadata

        Raises:
            DocumentProcessingError: If metadata extraction fails
        """
        if not os.path.exists(file_path):
            raise DocumentProcessingError(f"File not found: {file_path}")

        try:
            logger.info("Extracting metadata from document", file_path=file_path)

            with open(file_path, 'rb') as file:
                parsed = parser.from_buffer(file.read())

            metadata = parsed.get('metadata', {}) or {}
            
            logger.info("Metadata extraction completed",
                       file_path=file_path,
                       metadata_keys=list(metadata.keys()))

            return metadata

        except Exception as e:
            logger.error("Error extracting metadata from document",
                        file_path=file_path,
                        error=str(e))
            raise DocumentProcessingError(f"Error processing document {file_path}: {str(e)}") from e

    def get_supported_formats(self) -> set:
        """
        Get the set of supported file formats.

        Returns:
            set: Set of supported file extensions
        """
        return self.supported_formats.copy()

    def validate_file_size(self, file_path: str, max_size_mb: int = 100) -> bool:
        """
        Validate that a file is within the size limit.

        Args:
            file_path: Path to the document file
            max_size_mb: Maximum file size in megabytes

        Returns:
            bool: True if file is within size limit, False otherwise
        """
        if not os.path.exists(file_path):
            return False
        
        file_size_mb = os.path.getsize(file_path) / (1024 * 1024)
        return file_size_mb <= max_size_mb
