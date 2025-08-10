"""Apache Tika processor for extracting text and metadata."""

from typing import Dict, Any, Optional
from datetime import datetime, timezone
import os
import subprocess
import time
import requests
import signal
import atexit
import structlog
from dataclasses import dataclass

from tika import parser
import tika
from service_configuration.tika_config import TikaSettings


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
    Uses Tika server in client-only mode for better isolation and performance.
    """

    def __init__(self, settings: Optional[TikaSettings] = None):
        """Initialize the Tika processor with container-optimized settings.
        
        Args:
            settings: Tika configuration settings. If None, will create default settings.
        """
        self.supported_formats = {'.pdf', '.docx', '.doc', '.xlsx', '.xls', '.txt'}
        self.settings = settings or TikaSettings()
        self._server_process: Optional[subprocess.Popen] = None
        
        # Configure Tika for containerized environments
        self._configure_tika_server()
        
        # Start Tika server if auto-start is enabled
        if self.settings.TIKA_SERVER_AUTO_START:
            self._ensure_tika_server_running()
        
        # Register cleanup on exit
        atexit.register(self._cleanup)
        
        logger.info("TikaProcessor initialized", 
                   supported_formats=list(self.supported_formats),
                   tika_version=self.settings.TIKA_VERSION,
                   client_only_mode=self.settings.TIKA_CLIENT_ONLY)

    def _configure_tika_server(self):
        """Configure Tika for client-only mode with external server."""
        try:
            # Configure for client-only mode
            tika.TikaClientOnly = self.settings.TIKA_CLIENT_ONLY
            
            # Set server endpoint from settings
            tika.TikaServerEndpoint = self.settings.TIKA_SERVER_ENDPOINT
            
            # Set timeouts from settings
            tika.TikaServerTimeout = self.settings.TIKA_SERVER_TIMEOUT
            tika.TikaClientTimeout = self.settings.TIKA_CLIENT_TIMEOUT
            
            # Configure log path from settings
            if self.settings.TIKA_LOG_PATH:
                # Ensure the log directory exists and is writable
                log_dir = self.settings.TIKA_LOG_PATH
                if not os.path.exists(log_dir):
                    try:
                        os.makedirs(log_dir, exist_ok=True)
                        logger.info("Created Tika log directory", path=log_dir)
                    except Exception as dir_error:
                        logger.warning("Failed to create Tika log directory", path=log_dir, error=str(dir_error))
                
                try:
                    tika.TikaServerLogPath = self.settings.TIKA_LOG_PATH
                    logger.info("Configured Tika log path", path=self.settings.TIKA_LOG_PATH)
                except Exception as log_error:
                    logger.warning("Failed to set Tika log path", path=self.settings.TIKA_LOG_PATH, error=str(log_error))
            
            logger.info("Tika client configuration applied",
                       client_only=tika.TikaClientOnly,
                       endpoint=tika.TikaServerEndpoint,
                       server_timeout=tika.TikaServerTimeout,
                       client_timeout=tika.TikaClientTimeout,
                       version=self.settings.TIKA_VERSION)
                       
        except Exception as e:
            logger.warning("Failed to configure Tika client settings",
                          error=str(e))

    def _is_server_healthy(self) -> bool:
        """Check if Tika server is running and healthy."""
        try:
            response = requests.get(f"{self.settings.TIKA_SERVER_ENDPOINT}/version", timeout=5)
            return response.status_code == 200
        except Exception:
            return False

    def _start_tika_server(self) -> bool:
        """Start Tika server using the pre-installed JAR."""
        if not os.path.exists(self.settings.TIKA_SERVER_JAR):
            logger.error("Tika server JAR not found", jar_path=self.settings.TIKA_SERVER_JAR)
            return False

        try:
            # Extract host and port from endpoint
            endpoint_parts = self.settings.TIKA_SERVER_ENDPOINT.replace('http://', '').replace('https://', '')
            if ':' in endpoint_parts:
                host, port = endpoint_parts.split(':', 1)
                port = port.split('/')[0]  # Remove any path
            else:
                host = endpoint_parts.split('/')[0]
                port = '9998'  # Default Tika port

            # Prepare the command to start Tika server
            cmd = [
                'java',
                '-jar', self.settings.TIKA_SERVER_JAR,
                '--host', host,
                '--port', port
            ]

            # Set up log file if log path is configured
            log_file = None
            if self.settings.TIKA_LOG_PATH:
                log_file_path = os.path.join(self.settings.TIKA_LOG_PATH, 'tika-server.log')
                log_file = open(log_file_path, 'w')

            # Start the server process
            self._server_process = subprocess.Popen(
                cmd,
                stdout=log_file or subprocess.DEVNULL,
                stderr=subprocess.STDOUT,
                preexec_fn=os.setsid if os.name != 'nt' else None  # Create process group on Unix
            )

            logger.info("Started Tika server process", 
                       pid=self._server_process.pid,
                       endpoint=self.settings.TIKA_SERVER_ENDPOINT,
                       jar_path=self.settings.TIKA_SERVER_JAR)
            
            return True
            
        except Exception as e:
            logger.error("Failed to start Tika server", error=str(e))
            return False

    def _ensure_tika_server_running(self) -> bool:
        """Ensure Tika server is running, start it if necessary."""
        # Check if server is already healthy
        if self._is_server_healthy():
            logger.info("Tika server is already running and healthy")
            return True

        # Try to start the server
        if not self._start_tika_server():
            logger.error("Failed to start Tika server")
            return False

        # Wait for server to become healthy
        start_time = time.time()
        while time.time() - start_time < self.settings.TIKA_SERVER_STARTUP_TIMEOUT:
            if self._is_server_healthy():
                logger.info("Tika server started successfully", 
                           startup_time=time.time() - start_time)
                return True
            time.sleep(1)

        logger.error("Tika server failed to start within timeout", 
                    timeout=self.settings.TIKA_SERVER_STARTUP_TIMEOUT)
        
        # Clean up failed process
        if self._server_process:
            try:
                self._server_process.terminate()
                self._server_process.wait(timeout=5)
            except Exception as e:
                logger.warning("Failed to terminate failed server process", error=str(e))
            self._server_process = None
            
        return False

    def _cleanup(self):
        """Clean up Tika server process on shutdown."""
        if self._server_process:
            try:
                logger.info("Shutting down Tika server", pid=self._server_process.pid)
                
                # Try graceful shutdown first
                if os.name != 'nt':
                    # On Unix, kill the process group
                    os.killpg(os.getpgid(self._server_process.pid), signal.SIGTERM)
                else:
                    # On Windows, terminate the process
                    self._server_process.terminate()
                
                # Wait for graceful shutdown
                try:
                    self._server_process.wait(timeout=10)
                    logger.info("Tika server shut down gracefully")
                except subprocess.TimeoutExpired:
                    # Force kill if graceful shutdown fails
                    logger.warning("Forcing Tika server shutdown")
                    if os.name != 'nt':
                        os.killpg(os.getpgid(self._server_process.pid), signal.SIGKILL)
                    else:
                        self._server_process.kill()
                    self._server_process.wait()
                    
            except Exception as e:
                logger.warning("Error during Tika server cleanup", error=str(e))
            finally:
                self._server_process = None

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
            # Check if server is still running and try to restart if needed
            if not self._is_server_healthy() and self.settings.TIKA_SERVER_AUTO_START:
                logger.warning("Tika server appears unhealthy, attempting restart")
                self._ensure_tika_server_running()
                
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
        # Get a fresh logger instance for this processing operation
        process_logger = structlog.get_logger(__name__)
        
        process_logger.info("TIKA PROCESSING STARTED - extracting text from document", 
                           file_path=file_path,
                           worker_pid=os.getpid())
        
        if not os.path.exists(file_path):
            process_logger.error("TIKA PROCESSING ERROR - file not found", file_path=file_path)
            return DocumentProcessingResult(
                success=False,
                error_message=f"File not found: {file_path}"
            )

        try:
            start_time = datetime.now()
            process_logger.info("TIKA PROCESSING - beginning text extraction", file_path=file_path)

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
            
            process_logger.info("TIKA PROCESSING COMPLETED - text extraction completed",
                       file_path=file_path,
                       text_length=len(extracted_text),
                       processing_time=processing_time)

            return DocumentProcessingResult(
                extracted_text=extracted_text,
                file_type=metadata.get('Content-Type', 'unknown'),
                page_count=page_count,
                metadata=metadata,
                success=True,
                processing_time=processing_time
            )

        except Exception as e:
            process_logger.error("TIKA PROCESSING FAILED - error extracting text from document",
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
