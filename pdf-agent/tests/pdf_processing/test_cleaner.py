from src.pdf_processing.cleaner import clean_text, chunk_text
import pytest
import nltk
from nltk.tokenize.punkt import PunktTokenizer

@pytest.fixture(scope="function", autouse=True)
def set_azure_env_vars(monkeypatch):
    """Sets dummy Azure credentials to satisfy AppSettings validation for tests."""
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test_key")
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "https://test.openai.azure.com/")
    monkeypatch.setenv("AZURE_OPENAI_LLM_DEPLOYMENT_NAME", "test_llm_deployment")
    monkeypatch.setenv("AZURE_OPENAI_EMBEDDINGS_DEPLOYMENT_NAME", "test_embeddings_deployment")

def test_import_function():
    """Tests if the cleaning function can be imported."""
    assert callable(clean_text)

def test_fix_text_inconsistencies_with_ftfy():
    """Tests if ftfy fixes common text issues like mojibake."""
    # "—" (em dash) and "…" (ellipsis) encoded in UTF-8, then decoded as latin-1
    raw_text = "This is a test with strange characters: \xe2\x80\x94 and \xe2\x80\xa6"
    expected_text = "This is a test with strange characters: — and …"
    assert clean_text(raw_text) == expected_text

def test_normalize_unicode():
    """Tests if Unicode characters are normalized using NFKC."""
    # The character 'ﬁ' (a ligature) should be replaced by 'fi'.
    raw_text = "ﬁle"
    expected_text = "file"
    assert clean_text(raw_text) == expected_text

def test_rejoin_hyphenated_words():
    """Tests if words broken by a hyphen and a newline are rejoined."""
    raw_text = "This is a very long imple-\nmentation that was split."
    expected_text = "This is a very long implementation that was split."
    assert clean_text(raw_text) == expected_text

def test_normalize_newlines():
    """Tests if three or more newlines are replaced by exactly two."""
    raw_text = "Paragraph 1.\n\n\n\nParagraph 2."
    expected_text = "Paragraph 1.\n\nParagraph 2."
    assert clean_text(raw_text) == expected_text

def test_replace_tabs_with_spaces():
    """Tests if tab characters are replaced with a single space."""
    raw_text = "Column A\tColumn B"
    expected_text = "Column A Column B"
    assert clean_text(raw_text) == expected_text

def test_remove_excessive_whitespace():
    """Tests if multiple spaces are collapsed into a single space."""
    raw_text = "This   has    too   much   space."
    expected_text = "This has too much space."
    assert clean_text(raw_text) == expected_text

def test_strip_whitespace_from_entire_text():
    """Tests if leading/trailing whitespace is removed from the whole text."""
    raw_text = "\n\n  Start of text. End of text.  \n"
    # Note: the final strip happens after other processing.
    # The expected text here reflects the state *before* the final line-by-line strip
    # but after the main .strip() call.
    expected_text = "Start of text. End of text."
    assert clean_text(raw_text) == expected_text

def test_strip_whitespace_from_each_line():
    """Tests if leading/trailing whitespace is removed from each line."""
    raw_text = "  Line 1 has spaces.  \n\n  Line 2 also has them.  "
    expected_text = "Line 1 has spaces.\n\nLine 2 also has them."
    assert clean_text(raw_text) == expected_text

def test_comprehensive_cleaning_scenario():
    """Tests multiple cleaning operations applied together."""
    raw_text = """
    ﬁle with a lot of issues:
    Here is a real-\nly long line with   extra spaces and a tab	.

    And here is another paragraph af-\nter four newlines.
    It also has weird text like \xe2\x80\x94.
    """
    expected_text = "file with a lot of issues:\nHere is a really long line with extra spaces and a tab.\n\nAnd here is another paragraph after four newlines.\nIt also has weird text like —."
    assert clean_text(raw_text) == expected_text

def test_comprehensive_cleaning_scenario_2():
    """Tests a different comprehensive cleaning scenario."""
    raw_text = "  leading space.  A sentence with \xe2\x80\x94 and \xe2\x80\xa6 and   multiple   spaces.  trailing space.  "
    expected_text = "leading space. A sentence with — and … and multiple spaces. trailing space."
    assert clean_text(raw_text) == expected_text

@pytest.fixture(scope="session", autouse=True)
def download_nltk_punkt():
    """
    Downloads the NLTK 'punkt' model if not already present, ensuring
    the tokenizer can be instantiated.
    """
    try:
        # The underlying error is that the tokenizer itself cannot be loaded
        # without the data, so we try to instantiate it.
        PunktTokenizer()
    except LookupError:
        nltk.download('punkt_tab')

def test_chunk_text_simple():
    """Tests if text is chunked into a list of strings."""
    text = "This is the first sentence. This is the second sentence. This is a third."
    chunks = chunk_text(text)
    assert isinstance(chunks, list)
    assert len(chunks) > 0
    assert isinstance(chunks[0], str)

def test_chunk_text_respects_sentences():
    """
    Tests if the chunker attempts to split along sentence boundaries.
    With a small chunk size, we expect multiple chunks.
    """
    text = "Short sentence. Another short one. Third one is here. Fourth sentence is longer. Fifth is the last."
    # We use a very small chunk size to force splitting.
    # Note: This test is illustrative. The actual chunk content can vary
    # based on the chunker's logic for measuring length (e.g., characters or tokens).
    chunks = chunk_text(text, chunk_size=30, chunk_overlap=10)
    assert len(chunks) > 1
    # The first chunk should be the first sentence or part of it
    assert "Short sentence." in chunks[0]
    # The last chunk should contain the last sentence
    assert "Fifth is the last." in chunks[-1]

def test_chunk_text_with_long_text():
    """Tests chunking on a longer paragraph."""
    text = "This is a long paragraph that will definitely be split. It has multiple sentences to ensure that the logic works as expected. We are testing the RecursiveCharacterTextSplitter. The goal is to see it break this text down into smaller, manageable pieces for further processing. Each piece should be a string. The total number of chunks will depend on the configured size."
    chunks = chunk_text(text, chunk_size=100, chunk_overlap=20)
    assert len(chunks) > 1
    assert text.startswith(chunks[0])
    # Overlap means chunks can share text, but re-joining them will not perfectly equal the original
    # due to separators being added/removed by the chunker.
    # We can check that the second chunk starts with the beginning of a sentence from the original text.
    assert chunks[1].startswith("It has multiple sentences")

def test_empty_string_chunking():
    """Tests if chunking an empty string results in an empty list."""
    text = ""
    chunks = chunk_text(text)
    assert chunks == []

def test_chunk_text_uses_config_defaults(monkeypatch):
    """
    Tests if the chunker uses default values from the config.
    We can't directly test the config values, but we can mock the splitter
    to see what it's called with.
    """
    from langchain_text_splitters import RecursiveCharacterTextSplitter
    from src.config import get_settings

    settings = get_settings()

    class MockRecursiveCharacterTextSplitter(RecursiveCharacterTextSplitter):
        def __init__(self, *args, **kwargs):
            # Assert that the chunk_size and chunk_overlap match our defaults
            assert kwargs.get("chunk_size") == settings.CHUNK_SIZE
            assert kwargs.get("chunk_overlap") == settings.CHUNK_OVERLAP
            super().__init__(*args, **kwargs)

    monkeypatch.setattr(
        "src.pdf_processing.cleaner.RecursiveCharacterTextSplitter",
        MockRecursiveCharacterTextSplitter
    )

    text = "This is a test sentence. It will be used to verify config defaults."
    # This will raise an assertion error inside our mock if the defaults are not used
    chunk_text(text) 