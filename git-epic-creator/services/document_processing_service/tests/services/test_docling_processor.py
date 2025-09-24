"""Tests for Docling-backed document processor."""
import os
import tempfile
from unittest.mock import patch, MagicMock

import pytest

from services.docling_processor import DoclingProcessor


@pytest.fixture
def processor():
    return DoclingProcessor()


def _write_bytes_temp(suffix: str, data: bytes) -> str:
    tmp = tempfile.NamedTemporaryFile(suffix=suffix, delete=False)
    tmp.write(data)
    tmp.flush()
    tmp.close()
    return tmp.name


@patch("services.docling_processor.DocumentConverter")
def test_pdf_via_docling(mock_converter_cls, processor):
    mock_converter = MagicMock()
    mock_doc = MagicMock()
    mock_doc.export_to_markdown.return_value = "Hello\n\nWorld"
    mock_doc.metadata = {"foo": "bar"}
    mock_result = MagicMock(document=mock_doc)
    mock_converter.convert.return_value = mock_result
    mock_converter_cls.return_value = mock_converter

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


@patch("services.docling_processor.DocumentConverter")
def test_image_via_docling(mock_converter_cls, processor):
    mock_converter = MagicMock()
    mock_doc = MagicMock()
    mock_doc.export_to_markdown.return_value = "ImageText"
    mock_doc.metadata = {}
    mock_result = MagicMock(document=mock_doc)
    mock_converter.convert.return_value = mock_result
    mock_converter_cls.return_value = mock_converter

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


@patch("services.docling_processor.DocumentConverter", side_effect=Exception("docling boom"))
def test_pdf_failure_no_fallback(mock_converter_cls, processor):
    path = _write_bytes_temp(".pdf", b"%PDF-1.4\n%%EOF")
    try:
        result = processor.extract_text_with_result(path)
        assert not result.success
        assert "Docling failed" in (result.error_message or "")
    finally:
        try:
            os.unlink(path)
        except Exception:
            pass


@patch("services.docling_processor.DocumentConverter", side_effect=Exception("docling fail"))
def test_image_failure_no_fallback(mock_converter_cls, processor):
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
