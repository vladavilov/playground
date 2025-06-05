import pytest
import logging
import io
import re
from src.config import get_settings
from src.logger_config import setup_logging, get_logger # DEFAULT_LOG_FORMAT is used by setup_logging internally

@pytest.fixture
def log_stream():
    """Fixture to provide a StringIO stream for capturing logs."""
    return io.StringIO()

@pytest.fixture(autouse=True)
def reset_logging_for_test(monkeypatch):
    """Fixture to reset logging and app settings for each test."""
    root_logger = logging.getLogger()
    for h in root_logger.handlers[:]:
        root_logger.removeHandler(h)
        h.close()
    
    get_settings.cache_clear()
    monkeypatch.delenv("LOG_LEVEL", raising=False)
    monkeypatch.setenv("AZURE_OPENAI_API_KEY", "test_key_logger")
    monkeypatch.setenv("AZURE_OPENAI_ENDPOINT", "test_endpoint_logger")
    monkeypatch.setenv("AZURE_OPENAI_LLM_DEPLOYMENT_NAME", "test_llm_logger")
    monkeypatch.setenv("AZURE_OPENAI_EMBEDDINGS_DEPLOYMENT_NAME", "test_embeddings_logger")
    
    yield
    
    for h in root_logger.handlers[:]:
        root_logger.removeHandler(h)
        h.close()
    get_settings.cache_clear()
    monkeypatch.delenv("LOG_LEVEL", raising=False)

def test_logger_obtainment(monkeypatch, log_stream):
    monkeypatch.setenv("LOG_LEVEL", "INFO")
    setup_logging(stream=log_stream) # Pass stream to setup
    logger = get_logger("my_test_app")
    assert isinstance(logger, logging.Logger)
    assert logger.name == "my_test_app"

def test_log_message_formatting(monkeypatch, log_stream):
    monkeypatch.setenv("LOG_LEVEL", "INFO")
    setup_logging(stream=log_stream) # Pass stream to setup
    
    logger = get_logger("format_test_logger")
    logger.info("A test formatting message.")
    
    log_output = log_stream.getvalue()
    log_pattern = r"^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2},\d{3} - format_test_logger - INFO - test_logger_config:test_log_message_formatting:\d+ - A test formatting message.\s*\n?$"
    assert re.match(log_pattern, log_output) is not None, f"Log output '{log_output}' did not match: '{log_pattern}'"

def test_log_level_info_behavior(monkeypatch, log_stream):
    monkeypatch.setenv("LOG_LEVEL", "INFO")
    setup_logging(stream=log_stream) # Pass stream to setup

    logger = get_logger("level_test_logger_info")
    logger.setLevel(logging.DEBUG) # This logger *can* generate DEBUG messages

    logger.debug("This debug message should NOT appear due to root logger level.")
    logger.info("This info message SHould appear.")
    
    log_output = log_stream.getvalue()

    assert "This debug message should NOT appear" not in log_output
    assert "This info message SHould appear." in log_output

def test_log_level_debug_behavior(monkeypatch, log_stream):
    monkeypatch.setenv("LOG_LEVEL", "DEBUG")
    setup_logging(stream=log_stream) # Pass stream to setup

    logger = get_logger("level_test_logger_debug")
    logger.setLevel(logging.DEBUG) 

    logger.debug("Debug messages are now shown because root is DEBUG.")
    
    log_output = log_stream.getvalue()
    assert "Debug messages are now shown because root is DEBUG." in log_output

def test_log_level_from_config_sets_logger_level(monkeypatch, log_stream):
    monkeypatch.setenv("LOG_LEVEL", "WARNING")
    setup_logging(stream=log_stream) # Pass stream to setup
    logger = get_logger("config_level_test_logger")
    assert logger.getEffectiveLevel() == logging.WARNING 