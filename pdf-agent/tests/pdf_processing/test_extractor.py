import pytest
import os
from pathlib import Path
from unittest.mock import patch, MagicMock
import fitz

from src.pdf_processing.extractor import extract_text_from_pdf
from src.pdf_processing.exceptions import PDFExtractionError

# Determine the root directory of the project to build absolute paths
# Assuming this test file is in 'tests/pdf_processing/'
# and the project root is two levels up.
PROJECT_ROOT = Path(__file__).resolve().parent.parent.parent
TEST_PDF_DIR = PROJECT_ROOT / "tests" / "test_data" / "pdfs"

SAMPLE_PDF_PATH = TEST_PDF_DIR / "sample.pdf"
CORRUPTED_PDF_PATH = TEST_PDF_DIR / "corrupted.pdf"
EMPTY_PDF_PATH = TEST_PDF_DIR / "empty.pdf"
NON_EXISTENT_PDF_PATH = TEST_PDF_DIR / "non_existent.pdf"

# Ensure test files exist (placeholders for now)
# In a real scenario, these would be actual PDFs.
# For the purpose of this script, we ensure the placeholders are there.
# and rely on mocks for fitz behavior

@pytest.fixture(scope="module", autouse=True)
def create_placeholder_files():
    TEST_PDF_DIR.mkdir(parents=True, exist_ok=True)
    # Placeholder content is not critical as we mock fitz.open
    if not SAMPLE_PDF_PATH.exists():
        SAMPLE_PDF_PATH.write_text("This is a sample PDF content.")
    if not CORRUPTED_PDF_PATH.exists():
        CORRUPTED_PDF_PATH.write_text("This is corrupted data.")
    if not EMPTY_PDF_PATH.exists():
        EMPTY_PDF_PATH.write_text("") # Empty file

def test_import_function():
    """Tests if the extraction function can be imported."""
    from src.pdf_processing.extractor import extract_text_from_pdf
    assert callable(extract_text_from_pdf)

@patch('src.pdf_processing.extractor.fitz.open')
def test_extract_text_successfully(mock_fitz_open):
    """Tests successful text extraction from a PDF."""
    mock_page1 = MagicMock()
    mock_page1.get_text = MagicMock(return_value="Hello World. ")
    mock_page2 = MagicMock()
    mock_page2.get_text = MagicMock(return_value="This is page 2.")

    # This is the object returned by fitz.open()'s context manager
    mock_doc_context = MagicMock()
    mock_doc_context.page_count = 2
    # To support `for page in doc:`
    mock_doc_context.__iter__.return_value = iter([mock_page1, mock_page2])
    # To support `doc.load_page(i)` or `doc[i]` if the implementation uses that
    def get_page(page_num):
        if page_num == 0:
            return mock_page1
        elif page_num == 1:
            return mock_page2
        raise IndexError("Page index out of range")
    mock_doc_context.load_page = MagicMock(side_effect=get_page)
    # mock_doc_context.__getitem__ = MagicMock(side_effect=get_page) # if direct indexing doc[i] is used

    # This is the main mock for fitz.open()
    mock_doc_object = MagicMock()
    mock_doc_object.__enter__.return_value = mock_doc_context # __enter__ returns the context
    mock_doc_object.__exit__.return_value = None

    mock_fitz_open.return_value = mock_doc_object

    # Ensure the dummy file exists for os.path.exists in the function if it checks
    if not SAMPLE_PDF_PATH.exists():
        SAMPLE_PDF_PATH.touch()

    expected_text = "Hello World. This is page 2."
    actual_text = extract_text_from_pdf(str(SAMPLE_PDF_PATH))

    assert actual_text == expected_text
    mock_fitz_open.assert_called_once_with(str(SAMPLE_PDF_PATH))
    mock_page1.get_text.assert_called_once_with("text")
    mock_page2.get_text.assert_called_once_with("text")

def test_extract_text_file_not_found():
    """Tests FileNotFoundError for a non-existent PDF file."""
    with pytest.raises(FileNotFoundError):
        extract_text_from_pdf(str(NON_EXISTENT_PDF_PATH))

@patch('src.pdf_processing.extractor.fitz.open')
def test_extract_text_corrupted_pdf(mock_fitz_open):
    """Tests PDFExtractionError for a corrupted/unparseable PDF."""
    # Simulate fitz.open raising an error typical of unparseable PDFs
    if hasattr(fitz, 'FzError'): # Use fitz directly here
        mock_fitz_open.side_effect = fitz.FzError("Failed to open PDF")
    else: # Fallback if fitz.FzError is not available (e.g. fitz not installed in a way that this check works)
        mock_fitz_open.side_effect = RuntimeError("Simulated PyMuPDF error opening PDF")

    # Ensure the dummy file exists for os.path.exists in the function if it checks
    if not CORRUPTED_PDF_PATH.exists():
      CORRUPTED_PDF_PATH.touch()

    with pytest.raises(PDFExtractionError):
        extract_text_from_pdf(str(CORRUPTED_PDF_PATH))

@patch('src.pdf_processing.extractor.fitz.open')
def test_extract_text_empty_pdf(mock_fitz_open):
    """Tests extraction from an empty PDF (e.g., no text content or no pages)."""
    mock_doc_context = MagicMock()
    mock_doc_context.page_count = 0
    mock_doc_context.__iter__.return_value = iter([]) # No pages
    # Ensure load_page on an empty doc mock also handles returning no pages or raises appropriately if called
    mock_doc_context.load_page = MagicMock(side_effect=IndexError("Page index out of range for empty PDF mock"))

    mock_doc_object = MagicMock()
    mock_doc_object.__enter__.return_value = mock_doc_context
    mock_doc_object.__exit__.return_value = None

    mock_fitz_open.return_value = mock_doc_object

    # Ensure the dummy file exists for os.path.exists in the function if it checks
    if not EMPTY_PDF_PATH.exists():
        EMPTY_PDF_PATH.touch()

    expected_text = ""
    actual_text = extract_text_from_pdf(str(EMPTY_PDF_PATH))

    assert actual_text == expected_text
    mock_fitz_open.assert_called_once_with(str(EMPTY_PDF_PATH))

@patch('src.pdf_processing.extractor.fitz.open')
def test_extract_text_pdf_with_one_empty_page(mock_fitz_open):
    """Tests extraction from a PDF with one page that has no text."""
    mock_page_empty = MagicMock()
    mock_page_empty.get_text = MagicMock(return_value="")

    mock_doc_context = MagicMock()
    mock_doc_context.page_count = 1
    # mock_doc_context.__iter__.return_value = iter([mock_page_empty]) # if iterating doc directly
    mock_doc_context.load_page = MagicMock(return_value=mock_page_empty) # if using load_page

    mock_doc_object = MagicMock()
    mock_doc_object.__enter__.return_value = mock_doc_context
    mock_doc_object.__exit__.return_value = None

    mock_fitz_open.return_value = mock_doc_object

    # Ensure the dummy file exists for os.path.exists in the function if it checks
    # This file doesn't really need to exist if fitz.open is mocked, but good for consistency
    temp_pdf_path = TEST_PDF_DIR / "one_empty_page.pdf"
    if not temp_pdf_path.exists():
        temp_pdf_path.touch()

    expected_text = ""
    actual_text = extract_text_from_pdf(str(temp_pdf_path))

    assert actual_text == expected_text
    mock_fitz_open.assert_called_once_with(str(temp_pdf_path))
    mock_page_empty.get_text.assert_called_once_with("text")

    # Clean up the dummy file if it was created
    if temp_pdf_path.exists():
        os.remove(temp_pdf_path) 