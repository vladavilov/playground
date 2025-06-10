import re
import unicodedata
import ftfy

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