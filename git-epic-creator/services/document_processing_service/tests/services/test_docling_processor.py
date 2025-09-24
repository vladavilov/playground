"""Tests for Docling-backed document processor."""
import os
import tempfile
from unittest.mock import patch, MagicMock

import pytest

from services.docling_processor import DoclingProcessor, DocumentProcessingError


@pytest.fixture
def processor():
    return DoclingProcessor()


def _write_bytes_temp(suffix: str, data: bytes) -> str:
    tmp = tempfile.NamedTemporaryFile(suffix=suffix, delete=False)
    tmp.write(data)
    tmp.flush()
    tmp.close()
    return tmp.name


@patch("services.docling_processor.DocumentReader")
@patch("services.docling_processor.InputDocument")
def test_pdf_via_docling(mock_input_doc, mock_reader_cls, processor):
    mock_reader = MagicMock()
    mock_reader.read.return_value = MagicMock(pages=[MagicMock(text="Hello"), MagicMock(text="World")], metadata={"foo": "bar"})
    mock_reader_cls.return_value = mock_reader
    mock_input_doc.from_file.return_value = MagicMock()

    path = _write_bytes_temp(".pdf", b"%PDF-1.4\nHello\n%%EOF")
    try:
        result = processor.extract_text_with_result(path)
        assert result.success
        assert "Hello" in (result.extracted_text or "")
        assert result.file_type == "application/pdf"
        assert result.metadata.get("foo") == "bar"
    finally:
        try:
            os.unlink(path)
        except Exception:
            pass


@patch("services.docling_processor.DocumentReader")
@patch("services.docling_processor.InputDocument")
def test_image_via_docling(mock_input_doc, mock_reader_cls, processor):
    mock_reader = MagicMock()
    mock_reader.read.return_value = MagicMock(pages=[MagicMock(text="ImageText")], metadata={})
    mock_reader_cls.return_value = mock_reader
    mock_input_doc.from_file.return_value = MagicMock()

    path = _write_bytes_temp(".png", b"\x89PNG\r\n\x1a\n")
    try:
        result = processor.extract_text_with_result(path)
        assert result.success
        assert "ImageText" in (result.extracted_text or "")
        assert result.file_type == "image/*"
    finally:
        try:
            os.unlink(path)
        except Exception:
            pass


@patch("services.tika_processor.parser")
@patch("services.docling_processor.DocumentReader", side_effect=Exception("docling boom"))
def test_pdf_fallback_to_tika(mock_reader_cls, mock_tika_parser, processor):
    # When Docling fails, fall back to Tika for PDFs
    mock_tika_parser.from_buffer.return_value = {"content": "Fallback text", "metadata": {"Content-Type": "application/pdf"}}

    path = _write_bytes_temp(".pdf", b"%PDF-1.4\n%%EOF")
    try:
        result = processor.extract_text_with_result(path)
        assert result.success
        assert "Fallback text" in (result.extracted_text or "")
    finally:
        try:
            os.unlink(path)
        except Exception:
            pass


@patch("services.docling_processor.DocumentReader", side_effect=Exception("docling fail"))
def test_image_failure_no_fallback(mock_reader_cls, processor):
    path = _write_bytes_temp(".jpg", b"\xff\xd8\xff\xe0")
    try:
        result = processor.extract_text_with_result(path)
        assert not result.success
        assert "Docling failed" in (result.error_message or "")
    finally:
        try:
            os.unlink(path)
        except Exception:
            pass


@patch("services.tika_processor.parser")
def test_non_image_non_pdf_delegates_to_tika(mock_tika_parser, processor):
    mock_tika_parser.from_buffer.return_value = {"content": "Docx text", "metadata": {"Content-Type": "application/vnd.openxmlformats-officedocument.wordprocessingml.document"}}
    path = _write_bytes_temp(".docx", b"PK\x03\x04")
    try:
        txt = processor.extract_text(path)
        assert "Docx text" in txt
    finally:
        try:
            os.unlink(path)
        except Exception:
            pass


