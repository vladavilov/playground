import fitz  # PyMuPDF
import os
from .exceptions import PDFExtractionError

def extract_text_from_pdf(pdf_path: str) -> str:
    """Extracts raw text from a given PDF file.

    Args:
        pdf_path: The path to the PDF file.

    Returns:
        The raw text content of the PDF.

    Raises:
        FileNotFoundError: If the PDF file does not exist.
        PDFExtractionError: If there is an error during PDF parsing or text extraction.
    """
    if not os.path.exists(pdf_path):
        raise FileNotFoundError(f"The file was not found: {pdf_path}")
    if not os.path.isfile(pdf_path):
        raise FileNotFoundError(f"The path is not a file: {pdf_path}")

    try:
        doc = fitz.open(pdf_path)
    except RuntimeError as e:
        raise PDFExtractionError(f"Failed to open PDF '{pdf_path}'. Fitz error: {e}")
    except Exception as e:  # Catch other potential errors during open, though FzError is common
        raise PDFExtractionError(f"An unexpected error occurred while trying to open PDF '{pdf_path}': {e}")

    full_text = []
    try:
        with doc as opened_doc: # Use 'as opened_doc' to get the actual document object
            for page_num in range(opened_doc.page_count): # Use opened_doc here
                page = opened_doc.load_page(page_num)     # And here
                full_text.append(page.get_text("text"))
    except RuntimeError as e:  # Changed from fitz.FzError
        # This might catch errors during page loading or text extraction
        raise PDFExtractionError(f"Failed to extract text from PDF '{pdf_path}'. Fitz error: {e}")
    except Exception as e:
        # Catch other unexpected errors during text extraction
        raise PDFExtractionError(f"An unexpected error occurred during text extraction from PDF '{pdf_path}': {e}")
    
    return "".join(full_text) 