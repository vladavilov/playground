"""
Tests for Apache Tika document processor.
"""

import os
import tempfile
from unittest.mock import patch

import pytest

from services.tika_processor import TikaProcessor, DocumentProcessingError


class TestTikaProcessor:
    """Test cases for Apache Tika document processor."""

    @pytest.fixture
    def tika_processor(self):
        """Create a TikaProcessor instance for testing."""
        return TikaProcessor()

    @pytest.fixture
    def sample_pdf_content(self):
        """Sample PDF content for testing."""
        return (
            b"%PDF-1.4\n1 0 obj\n<<\n/Type /Catalog\n/Pages 2 0 R\n>>\nendobj\n"
            b"2 0 obj\n<<\n/Type /Pages\n/Kids [3 0 R]\n/Count 1\n>>\nendobj\n"
            b"3 0 obj\n<<\n/Type /Page\n/Parent 2 0 R\n/MediaBox [0 0 612 792]\n"
            b"/Contents 4 0 R\n>>\nendobj\n4 0 obj\n<<\n/Length 44\n>>\nstream\n"
            b"BT\n/F1 12 Tf\n72 720 Td\n(Hello World) Tj\nET\nendstream\nendobj\n"
            b"xref\n0 5\n0000000000 65535 f \n0000000009 00000 n \n"
            b"0000000058 00000 n \n0000000115 00000 n \n0000000206 00000 n \n"
            b"trailer\n<<\n/Size 5\n/Root 1 0 R\n>>\nstartxref\n299\n%%EOF"
        )

    @pytest.fixture
    def sample_text_content(self):
        """Sample text content for testing."""
        return "This is a sample text document for testing purposes."

    def test_tika_processor_initialization(self, tika_processor):
        """Test TikaProcessor initialization."""
        assert tika_processor is not None
        assert hasattr(tika_processor, 'extract_text')
        assert hasattr(tika_processor, 'extract_metadata')
        assert hasattr(tika_processor, 'process_document')

    @patch('services.tika_processor.parser')
    def test_extract_text_from_pdf(self, mock_parser, tika_processor, sample_pdf_content):
        """Test text extraction from PDF file."""
        # Mock Tika parser response
        mock_parser.from_buffer.return_value = {
            'content': 'Hello World\n\nThis is extracted text from PDF.',
            'metadata': {'Content-Type': 'application/pdf'}
        }

        temp_file = tempfile.NamedTemporaryFile(suffix='.pdf', delete=False)
        try:
            temp_file.write(sample_pdf_content)
            temp_file.flush()
            temp_file.close()  # Close the file before processing

            result = tika_processor.extract_text(temp_file.name)
            
            assert result is not None
            assert 'Hello World' in result
            assert 'extracted text from PDF' in result
            mock_parser.from_buffer.assert_called_once()
        finally:
            try:
                os.unlink(temp_file.name)
            except (OSError, PermissionError):
                pass  # Ignore cleanup errors on Windows

    @patch('services.tika_processor.parser')
    def test_extract_text_from_txt(self, mock_parser, tika_processor, sample_text_content):
        """Test text extraction from TXT file."""
        # Mock Tika parser response
        mock_parser.from_buffer.return_value = {
            'content': sample_text_content,
            'metadata': {'Content-Type': 'text/plain'}
        }

        temp_file = tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False, encoding='utf-8')
        try:
            temp_file.write(sample_text_content)
            temp_file.flush()
            temp_file.close()  # Close the file before processing

            result = tika_processor.extract_text(temp_file.name)
            
            assert result is not None
            assert sample_text_content in result
            mock_parser.from_buffer.assert_called_once()
        finally:
            try:
                os.unlink(temp_file.name)
            except (OSError, PermissionError):
                pass  # Ignore cleanup errors on Windows

    @patch('services.tika_processor.parser')
    def test_extract_metadata(self, mock_parser, tika_processor, sample_pdf_content):
        """Test metadata extraction from document."""
        # Mock Tika parser response with metadata
        mock_metadata = {
            'Content-Type': 'application/pdf',
            'Creation-Date': '2024-01-01T10:00:00Z',
            'Author': 'Test Author',
            'Title': 'Test Document',
            'Page-Count': '1'
        }
        mock_parser.from_buffer.return_value = {
            'content': 'Sample content',
            'metadata': mock_metadata
        }

        temp_file = tempfile.NamedTemporaryFile(suffix='.pdf', delete=False)
        try:
            temp_file.write(sample_pdf_content)
            temp_file.flush()
            temp_file.close()  # Close the file before processing

            result = tika_processor.extract_metadata(temp_file.name)
            
            assert result is not None
            assert result['Content-Type'] == 'application/pdf'
            assert result['Author'] == 'Test Author'
            assert result['Title'] == 'Test Document'
            assert result['Page-Count'] == '1'
            mock_parser.from_buffer.assert_called_once()
        finally:
            try:
                os.unlink(temp_file.name)
            except (OSError, PermissionError):
                pass  # Ignore cleanup errors on Windows

    @patch('services.tika_processor.parser')
    def test_process_document_structured_output(self, mock_parser, tika_processor, sample_pdf_content):
        """Test complete document processing with structured JSON output."""
        # Mock Tika parser response
        mock_parser.from_buffer.return_value = {
            'content': (
                'This is a test document with requirements.\n\n'
                'Requirement 1: System shall authenticate users.\n'
                'Requirement 2: System shall log all activities.'
            ),
            'metadata': {
                'Content-Type': 'application/pdf',
                'Author': 'Test Author',
                'Title': 'Requirements Document',
                'Page-Count': '2'
            }
        }

        temp_file = tempfile.NamedTemporaryFile(suffix='.pdf', delete=False)
        try:
            temp_file.write(sample_pdf_content)
            temp_file.flush()
            temp_file.close()  # Close the file before processing

            result = tika_processor.process_document(temp_file.name)
            
            assert result is not None
            assert isinstance(result, dict)
            
            # Check structured output format
            assert 'file_path' in result
            assert 'file_type' in result
            assert 'extracted_text' in result
            assert 'metadata' in result
            assert 'processing_timestamp' in result
            assert 'text_length' in result
            
            # Verify content
            assert result['file_type'] == 'application/pdf'
            assert 'authenticate users' in result['extracted_text']
            assert 'log all activities' in result['extracted_text']
            assert result['metadata']['Author'] == 'Test Author'
            assert result['text_length'] > 0
            
            mock_parser.from_buffer.assert_called_once()
        finally:
            try:
                os.unlink(temp_file.name)
            except (OSError, PermissionError):
                pass  # Ignore cleanup errors on Windows

    def test_extract_text_file_not_found(self, tika_processor):
        """Test error handling when file doesn't exist."""
        with pytest.raises(DocumentProcessingError) as exc_info:
            tika_processor.extract_text('/nonexistent/file.pdf')
        
        assert 'File not found' in str(exc_info.value)

    @patch('services.tika_processor.parser')
    def test_extract_text_tika_error(self, mock_parser, tika_processor, sample_pdf_content):
        """Test error handling when Tika fails."""
        # Mock Tika parser to raise an exception
        mock_parser.from_buffer.side_effect = Exception("Tika parsing failed")

        temp_file = tempfile.NamedTemporaryFile(suffix='.pdf', delete=False)
        try:
            temp_file.write(sample_pdf_content)
            temp_file.flush()
            temp_file.close()  # Close the file before processing

            with pytest.raises(DocumentProcessingError) as exc_info:
                tika_processor.extract_text(temp_file.name)
            
            assert 'Error processing document' in str(exc_info.value)
            assert 'Tika parsing failed' in str(exc_info.value)
        finally:
            try:
                os.unlink(temp_file.name)
            except (OSError, PermissionError):
                pass  # Ignore cleanup errors on Windows

    @patch('services.tika_processor.parser')
    def test_supported_file_formats(self, mock_parser, tika_processor):
        """Test that processor supports multiple file formats."""
        supported_formats = ['.pdf', '.docx', '.doc', '.xlsx', '.xls', '.txt']
        
        for file_format in supported_formats:
            # Mock Tika parser response
            mock_parser.from_buffer.return_value = {
                'content': f'Sample content for {file_format}',
                'metadata': {'Content-Type': f'application/{file_format[1:]}'}
            }

            temp_file = tempfile.NamedTemporaryFile(suffix=file_format, delete=False)
            try:
                temp_file.write(b'sample content')
                temp_file.flush()
                temp_file.close()  # Close the file before processing

                result = tika_processor.extract_text(temp_file.name)
                assert result is not None
                assert f'Sample content for {file_format}' in result
            finally:
                try:
                    os.unlink(temp_file.name)
                except (OSError, PermissionError):
                    pass  # Ignore cleanup errors on Windows

    @patch('services.tika_processor.parser')
    def test_empty_document_handling(self, mock_parser, tika_processor):
        """Test handling of empty documents."""
        # Mock Tika parser response for empty document
        mock_parser.from_buffer.return_value = {
            'content': '',
            'metadata': {'Content-Type': 'application/pdf'}
        }

        temp_file = tempfile.NamedTemporaryFile(suffix='.pdf', delete=False)
        try:
            temp_file.write(b'%PDF-1.4\n%%EOF')  # Minimal empty PDF
            temp_file.flush()
            temp_file.close()  # Close the file before processing

            result = tika_processor.process_document(temp_file.name)
            
            assert result is not None
            assert result['extracted_text'] == ''
            assert result['text_length'] == 0
        finally:
            try:
                os.unlink(temp_file.name)
            except (OSError, PermissionError):
                pass  # Ignore cleanup errors on Windows

    @patch('services.tika_processor.parser')
    def test_large_document_handling(self, mock_parser, tika_processor):
        """Test handling of large documents."""
        # Mock Tika parser response for large document
        large_content = 'This is a large document. ' * 1000  # Simulate large content
        mock_parser.from_buffer.return_value = {
            'content': large_content,
            'metadata': {'Content-Type': 'application/pdf', 'Page-Count': '100'}
        }

        temp_file = tempfile.NamedTemporaryFile(suffix='.pdf', delete=False)
        try:
            temp_file.write(b'%PDF-1.4\nLarge document content\n%%EOF')
            temp_file.flush()
            temp_file.close()  # Close the file before processing

            result = tika_processor.process_document(temp_file.name)
            
            assert result is not None
            assert len(result['extracted_text']) > 10000  # Should be large
            assert result['text_length'] == len(large_content) - 1
        finally:
            try:
                os.unlink(temp_file.name)
            except (OSError, PermissionError):
                pass  # Ignore cleanup errors on Windows