import re
import unicodedata
import ftfy
import nltk
from langchain_text_splitters import RecursiveCharacterTextSplitter

from src.config import get_settings

def clean_text(text: str) -> str:
    """
    Performs a sequence of cleaning operations on a string of text.

    This function chains together multiple cleaning steps to normalize and
    standardize text, which is particularly useful for text extracted from
    PDFs or other sources prone to encoding and formatting issues.

    The cleaning pipeline is as follows:
    1. Normalize Unicode characters to NFKC form. This can sometimes
       decompose characters, so it's done before other steps.
    2. Fix text inconsistencies (e.g., mojibake) that may have been
       present in the original text or introduced by normalization.
    3. Rejoin words hyphenated across newlines
    4. Normalize three or more consecutive newlines to exactly two.
    5. Replace tab characters with single spaces.
    6. Collapse multiple consecutive spaces into a single space.
    7. Strip leading/trailing whitespace from the entire text block.
    8. Strip leading/trailing whitespace from each individual line.
    9. Clean up spaces before punctuation

    Args:
        text: The raw input string to be cleaned.

    Returns:
        The cleaned and normalized string.
    """
    text = unicodedata.normalize('NFKC', text)
    text = ftfy.fix_text(text)
    text = re.sub(r'([a-zA-Z]+)-\n([a-zA-Z]+)', r'\1\2', text)
    text = re.sub(r'\n{3,}', '\n\n', text)
    text = text.replace('\t', ' ')
    text = re.sub(r' {2,}', ' ', text)
    text = text.strip()
    lines = text.split('\n')
    stripped_lines = [line.strip() for line in lines]
    text = '\n'.join(stripped_lines)
    text = re.sub(r'\s+([,.!?;:])', r'\1', text)

    return text 

def chunk_text(text: str, chunk_size: int = None, chunk_overlap: int = None) -> list[str]:
    """
    Splits a given text into smaller chunks based on sentence boundaries.

    This function first tokenizes the text into individual sentences using
    NLTK's 'punkt' tokenizer. It then uses a RecursiveCharacterTextSplitter
    to group these sentences into chunks of a specified size with a defined
    overlap between them.

    The chunk size and overlap can be provided directly or will be taken
    from the application's configuration if not specified.

    Args:
        text: The input text string to be chunked.
        chunk_size: The target maximum size of each chunk (in characters).
                    Defaults to the value in the application settings.
        chunk_overlap: The desired overlap between consecutive chunks
                       (in characters). Defaults to the value in the
                       application settings.

    Returns:
        A list of strings, where each string is a text chunk. Returns an
        empty list if the input text is empty or None.
    """
    if not text:
        return []

    settings = get_settings()
    
    # Use provided values or fall back to settings
    final_chunk_size = chunk_size if chunk_size is not None else settings.CHUNK_SIZE
    final_chunk_overlap = chunk_overlap if chunk_overlap is not None else settings.CHUNK_OVERLAP

    # First, split the text into sentences
    sentences = nltk.sent_tokenize(text)

    # Initialize the text splitter
    text_splitter = RecursiveCharacterTextSplitter(
        chunk_size=final_chunk_size,
        chunk_overlap=final_chunk_overlap,
        length_function=len,
        is_separator_regex=False,
        separators=["\n\n", "\n", ". ", " ", ""],
    )

    # Create documents from sentences to feed into the splitter
    # The splitter works best with a list of documents (strings)
    chunks = text_splitter.create_documents(sentences)
    
    # The splitter returns Document objects, so we extract the page_content
    return [chunk.page_content for chunk in chunks] 