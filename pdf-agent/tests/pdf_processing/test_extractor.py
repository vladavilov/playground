import pytest
from pathlib import Path

from src.pdf_processing.extractor import extract_text_from_pdf
from src.pdf_processing.exceptions import PDFExtractionError

# Determine the root directory of the project to build absolute paths
PROJECT_ROOT = Path(__file__).resolve().parent.parent.parent
TEST_PDF_DIR = PROJECT_ROOT / "tests" / "test_data" / "pdfs"

SAMPLE_PDF_PATH = TEST_PDF_DIR / "sample.pdf"
CORRUPTED_PDF_PATH = TEST_PDF_DIR / "corrupted.pdf"
EMPTY_PDF_PATH = TEST_PDF_DIR / "empty.pdf"
NON_EXISTENT_PDF_PATH = TEST_PDF_DIR / "non_existent.pdf"

def test_import_function():
    """Tests if the extraction function can be imported."""
    from src.pdf_processing.extractor import extract_text_from_pdf
    assert callable(extract_text_from_pdf)

def test_extract_text_successfully():
    """Tests successful text extraction from a real PDF."""
    # We don't know the exact content, but it should be a non-empty string
    actual_text = extract_text_from_pdf(str(SAMPLE_PDF_PATH))
    assert isinstance(actual_text, str)
    assert actual_text.strip() != ""

def test_extract_text_file_not_found():
    """Tests FileNotFoundError for a non-existent PDF file."""
    with pytest.raises(FileNotFoundError):
        extract_text_from_pdf(str(NON_EXISTENT_PDF_PATH))

def test_extract_text_corrupted_pdf():
    """Tests PDFExtractionError for a corrupted/unparseable PDF."""
    with pytest.raises(PDFExtractionError):
        extract_text_from_pdf(str(CORRUPTED_PDF_PATH))

def test_extract_text_empty_pdf():
    """Tests extraction from an empty PDF (no text content or no pages)."""
    expected_text = ""
    actual_text = extract_text_from_pdf(str(EMPTY_PDF_PATH))
    assert actual_text == expected_text 