from src.pdf_processing.cleaner import clean_text
import pytest

@pytest.fixture(scope="function", autouse=True)
def set_azure_env_vars(monkeypatch):
    """Sets dummy Azure credentials to satisfy AppSettings validation for tests."""
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test_key")
    monkeypatch.setenv("AZURE_OPENAI_CHAT_ENDPOINT", "https://test.openai.azure.com/")
    monkeypatch.setenv("AZURE_OPENAI_LLM_MODEL_NAME", "test-llm")

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