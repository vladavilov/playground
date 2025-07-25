import pytest
import json
import io
import logging
import structlog

from configuration.logging_config import configure_logging, filter_sensitive_data

@pytest.fixture(autouse=True)
def reset_logging():
    """Reset logging and structlog configuration after each test."""
    yield
    structlog.reset_defaults()
    # Also, clean up any handlers added to the root logger
    root_logger = logging.getLogger()
    for handler in root_logger.handlers[:]:
        root_logger.removeHandler(handler)

def test_filter_sensitive_data_processor():
    """Unit test for the sensitive data filter processor."""
    event_dict = {
        'password': 'my-secret-password',
        'token': 'my-secret-token',
        'non_sensitive': 'this is fine',
    }
    processed_dict = filter_sensitive_data(None, None, event_dict)
    assert processed_dict['password'] == '[FILTERED]'
    assert processed_dict['token'] == '[FILTERED]'
    assert processed_dict['non_sensitive'] == 'this is fine'

def test_logging_produces_filtered_json():
    """
    Test that the `configure_logging` function sets up logging correctly
    to produce filtered JSON output.
    """
    log_capture_stream = io.StringIO()
    
    # Call the function we want to test, passing our in-memory stream
    configure_logging(log_level=logging.INFO, stream=log_capture_stream)
    
    logger = structlog.get_logger("my_test_logger")
    logger.info(
        "user login attempt",
        user="test",
        password="my-secret-password"
    )
    
    log_output = log_capture_stream.getvalue().strip()

    try:
        log_json = json.loads(log_output)
    except json.JSONDecodeError:
        pytest.fail(f"Log output is not valid JSON: {log_output!r}")

    assert log_json['event'] == 'user login attempt'
    assert log_json['password'] == '[FILTERED]'
    assert log_json['level'] == 'info'
    assert log_json['logger'] == 'my_test_logger'
    assert 'user' in log_json
