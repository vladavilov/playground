"""Integration tests for Docling-backed document processor (no local mocks).

Only external web calls are suppressed by setting HF_HUB_OFFLINE.
"""
import os
import tempfile
from pathlib import Path
from services.docling_processor import DoclingProcessor

# Intentionally avoid importing DoclingProcessor at module import time.
# Each test imports it after environment variables are configured.


def _write_bytes_temp(suffix: str, data: bytes) -> str:
    tmp = tempfile.NamedTemporaryFile(suffix=suffix, delete=False)
    tmp.write(data)
    tmp.flush()
    tmp.close()
    return tmp.name


def test_pdf_integration_local_sample(monkeypatch):
    # Isolate HuggingFace caches to a temp dir and disable symlinks/hardlinks on Windows
    tmp_root = tempfile.mkdtemp(prefix="hf-cache-")
    monkeypatch.setenv("HF_HOME", tmp_root)
    monkeypatch.setenv("HUGGINGFACE_HUB_CACHE", os.path.join(tmp_root, "hub"))
    monkeypatch.setenv("TRANSFORMERS_CACHE", os.path.join(tmp_root, "transformers"))
    monkeypatch.setenv("HF_HUB_DISABLE_SYMLINKS", "1")
    monkeypatch.setenv("HF_HUB_DISABLE_HARDLINKS", "1")
    monkeypatch.setenv("HF_HUB_DISABLE_PROGRESS_BARS", "1")

    # Use an existing small sample PDF next to this test file
    path = str(Path(__file__).resolve().parent / "dummy.pdf")

    processor = DoclingProcessor()
    result = processor.extract_text_with_result(path)
    assert result.success is True
    assert result.file_type == "application/pdf"
    assert isinstance(result.extracted_text, str)
    assert "Welcome to Smallpdf" in result.extracted_text

def test_image_integration_png(monkeypatch):
    # This test is opt-in due to VLM requirement; enable with DOC_VLM_TEST=1
    if os.getenv("DOC_VLM_TEST") != "1":
        import pytest
        pytest.skip("Enable DOC_VLM_TEST=1 to run image/VLM integration test.")

    # Isolate HuggingFace caches to a temp dir and disable symlinks/hardlinks on Windows
    tmp_root = tempfile.mkdtemp(prefix="hf-cache-")
    monkeypatch.setenv("HF_HOME", tmp_root)
    monkeypatch.setenv("HUGGINGFACE_HUB_CACHE", os.path.join(tmp_root, "hub"))
    monkeypatch.setenv("TRANSFORMERS_CACHE", os.path.join(tmp_root, "transformers"))
    monkeypatch.setenv("HF_HUB_DISABLE_SYMLINKS", "1")
    monkeypatch.setenv("HF_HUB_DISABLE_HARDLINKS", "1")
    monkeypatch.setenv("HF_HUB_DISABLE_PROGRESS_BARS", "1")

    # Create a small valid PNG via base64 to avoid decode errors
    import base64
    # 1x1 transparent PNG
    png_b64 = (
        b"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMB/ao9oZkAAAAASUVORK5CYII="
    )
    data = base64.b64decode(png_b64)
    path = _write_bytes_temp(".png", data)

    from services.docling_processor import DoclingProcessor
    processor = DoclingProcessor()
    try:
        result = processor.extract_text_with_result(path)
        assert result.success is True
        assert result.file_type == "image/*"
        assert isinstance(result.extracted_text, str)
    finally:
        try:
            os.unlink(path)
        except Exception:
            pass


def test_pdf_unsupported_format_error():
    # Use a plain text file to trigger unsupported format handling
    path = _write_bytes_temp(".txt", b"hello")
    try:
        from services.docling_processor import DoclingProcessor
        processor = DoclingProcessor()
        result = processor.extract_text_with_result(path)
        assert result.success is False
        assert "Unsupported" in (result.error_message or "")
    finally:
        try:
            os.unlink(path)
        except Exception:
            pass


def test_nonexistent_file_error():
    from services.docling_processor import DoclingProcessor
    processor = DoclingProcessor()
    result = processor.extract_text_with_result("/path/does/not/exist.pdf")
    assert result.success is False
    assert "File not found" in (result.error_message or "")
