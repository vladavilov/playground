"""Integration tests for Docling-backed document processor (no local mocks).

Only external web calls are suppressed by setting HF_HUB_OFFLINE.
"""
import os
import tempfile
import shutil
from pathlib import Path
from services.docling_processor import DoclingProcessor


def _write_bytes_temp(suffix: str, data: bytes) -> str:
    tmp = tempfile.NamedTemporaryFile(suffix=suffix, delete=False)
    tmp.write(data)
    tmp.flush()
    tmp.close()
    return tmp.name


def test_pdf_integration_local_sample(monkeypatch):
    tmp_root = tempfile.mkdtemp(prefix="hf-cache-")
    monkeypatch.setenv("HF_HOME", tmp_root)
    monkeypatch.setenv("HUGGINGFACE_HUB_CACHE", os.path.join(tmp_root, "hub"))
    monkeypatch.setenv("TRANSFORMERS_CACHE", os.path.join(tmp_root, "transformers"))
    monkeypatch.setenv("HF_HUB_DISABLE_SYMLINKS", "1")
    monkeypatch.setenv("HF_HUB_DISABLE_HARDLINKS", "1")
    monkeypatch.setenv("HF_HUB_DISABLE_PROGRESS_BARS", "1")

    path = str(Path(__file__).resolve().parent / "dummy.pdf")

    processor = DoclingProcessor()
    result = processor.extract_text_with_result(path)
    assert result.success is True
    assert result.file_type == "application/pdf"
    assert isinstance(result.extracted_text, str)
    assert "Welcome to Smallpdf" in result.extracted_text

def test_image_integration_png(monkeypatch):
    tmp_root = tempfile.mkdtemp(prefix="hf-cache-")
    monkeypatch.setenv("HF_HOME", tmp_root)
    monkeypatch.setenv("HUGGINGFACE_HUB_CACHE", os.path.join(tmp_root, "hub"))
    monkeypatch.setenv("TRANSFORMERS_CACHE", os.path.join(tmp_root, "transformers"))
    monkeypatch.setenv("HF_HUB_DISABLE_SYMLINKS", "1")
    monkeypatch.setenv("HF_HUB_DISABLE_HARDLINKS", "1")
    monkeypatch.setenv("HF_HUB_DISABLE_PROGRESS_BARS", "1")

    src_path = str(Path(__file__).resolve().parent / "01-sequence-diagram-example.png")
    tmp_img_dir = tempfile.mkdtemp(prefix="docling-img-")
    path = os.path.join(tmp_img_dir, "sample.png")
    shutil.copyfile(src_path, path)

    processor = DoclingProcessor()
    try:
        result = processor.extract_text_with_result(path)
        assert result.success is True
        assert "LifeLine" in result.extracted_text
        assert result.file_type == "image/*"
        assert isinstance(result.extracted_text, str)
    finally:
        try:
            shutil.rmtree(tmp_img_dir, ignore_errors=True)
        except Exception:
            pass


def test_pdf_unsupported_format_error():
    path = _write_bytes_temp(".txt", b"hello")
    try:
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
    processor = DoclingProcessor()
    result = processor.extract_text_with_result("/path/does/not/exist.pdf")
    assert result.success is False
    assert "File not found" in (result.error_message or "")
