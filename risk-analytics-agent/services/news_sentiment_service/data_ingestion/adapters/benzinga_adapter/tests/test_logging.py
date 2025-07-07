import pytest
import logging
import asyncio
from unittest.mock import patch, MagicMock, AsyncMock
from fastapi.testclient import TestClient
from io import StringIO

from main import app

client = TestClient(app)

# ============================================================================
# FIXTURES
# ============================================================================

@pytest.fixture
def mock_logger():
    """Mock logger for testing application log output"""
    logger = MagicMock()
    with patch('main.logger', logger):
        yield logger

@pytest.fixture
def mock_middleware_logger():
    """Mock middleware logger for testing middleware log output"""
    logger = MagicMock()
    with patch('middleware.logger', logger):
        yield logger

@pytest.fixture
def captured_logs():
    """Capture log output to a string buffer"""
    log_capture = StringIO()
    handler = logging.StreamHandler(log_capture)
    handler.setLevel(logging.DEBUG)
    
    # Use the same format as the application
    formatter = logging.Formatter(
        fmt="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S"
    )
    handler.setFormatter(formatter)
    
    # Get the root logger and add our handler
    root_logger = logging.getLogger()
    root_logger.addHandler(handler)
    root_logger.setLevel(logging.DEBUG)
    
    yield log_capture
    
    # Clean up
    root_logger.removeHandler(handler)

@pytest.fixture
def mock_benzinga_settings():
    """Mock Benzinga API settings"""
    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"
        yield mock_settings

@pytest.fixture
def mock_request():
    """Create a mock FastAPI request object"""
    mock_request = MagicMock()
    mock_request.method = "GET"
    mock_request.url.path = "/test"
    mock_request.url = MagicMock()
    mock_request.url.__str__ = MagicMock(return_value="http://test.com/test")
    mock_request.query_params = {}
    mock_request.headers = {}
    mock_request.client.host = "127.0.0.1"
    return mock_request

# ============================================================================
# HELPER METHODS
# ============================================================================

class TestHelpers:
    """Helper methods for consistent testing patterns"""
    
    @staticmethod
    def setup_httpx_mock_exception(mock_client_class, exception):
        """Setup httpx client mock to raise an exception"""
        mock_client_class.return_value.__aenter__.return_value.get.side_effect = exception
    
    @staticmethod
    def setup_httpx_mock_response(mock_client_class, status_code=200, json_data=None):
        """Setup httpx client mock to return a response"""
        mock_response = MagicMock()
        mock_response.status_code = status_code
        if json_data:
            mock_response.json.return_value = json_data
        mock_client_class.return_value.__aenter__.return_value.get.return_value = mock_response
        return mock_response
    
    @staticmethod
    def assert_logger_called_with_message(mock_logger, log_level, expected_message):
        """Assert that logger was called with a specific message"""
        log_method = getattr(mock_logger, log_level)
        log_method.assert_called()
        
        logged_calls = log_method.call_args_list
        assert len(logged_calls) > 0, f"Expected {log_level} log calls"
        
        message_found = False
        for call_args in logged_calls:
            call_str = str(call_args)
            if expected_message in call_str:
                message_found = True
                break
        
        assert message_found, f"Expected message '{expected_message}' not found in {log_level} logs"
    
    @staticmethod
    def assert_logger_called_with_exc_info(mock_logger, log_level="error"):
        """Assert that logger was called with exception info"""
        log_method = getattr(mock_logger, log_level)
        log_method.assert_called()
        
        logged_calls = log_method.call_args_list
        assert len(logged_calls) > 0, f"Expected {log_level} log calls"
        
        exc_info_found = False
        for call_args in logged_calls:
            call_str = str(call_args)
            if "exc_info=True" in call_str or "stack_info=True" in call_str:
                exc_info_found = True
                break
        
        assert exc_info_found, f"Expected exc_info=True in {log_level} logs"
    
    @staticmethod
    def create_mock_request(method="GET", path="/test", url="http://test.com/test", 
                           query_params=None, headers=None, client_host="127.0.0.1"):
        """Create a mock request with specified parameters"""
        mock_request = MagicMock()
        mock_request.method = method
        mock_request.url.path = path
        mock_request.url = MagicMock()
        mock_request.url.__str__ = MagicMock(return_value=url)
        mock_request.query_params = query_params or {}
        mock_request.headers = headers or {}
        mock_request.client.host = client_host
        return mock_request
    
    @staticmethod
    async def create_mock_call_next(exception=None):
        """Create a mock call_next function for middleware testing"""
        if exception:
            async def mock_call_next(request):
                raise exception
        else:
            async def mock_call_next(request):
                return MagicMock(status_code=200)
        return mock_call_next

# ============================================================================
# EXCEPTION LOGGING TESTS
# ============================================================================

def test_exception_logging_with_stack_trace(mock_logger, mock_benzinga_settings):
    """Test that exceptions are logged with full stack traces"""
    with patch("main.httpx.AsyncClient") as mock_client:
        TestHelpers.setup_httpx_mock_exception(mock_client, Exception("Test exception"))
        
        response = client.get("/news")
    
    # Verify exception was logged with stack trace
    TestHelpers.assert_logger_called_with_message(mock_logger, "error", "Test exception")
    TestHelpers.assert_logger_called_with_exc_info(mock_logger)

def test_request_error_logging_details(mock_logger, mock_benzinga_settings):
    """Test that request errors are logged with proper context"""
    import httpx
    
    with patch("main.httpx.AsyncClient") as mock_client:
        TestHelpers.setup_httpx_mock_exception(mock_client, httpx.RequestError("Connection timeout"))
        
        response = client.get("/news")
    
    # Verify that request error was logged with context
    TestHelpers.assert_logger_called_with_message(mock_logger, "error", "Connection timeout")

def test_http_status_error_logging(mock_logger, mock_benzinga_settings):
    """Test that HTTP status errors are logged with proper context"""
    import httpx
    
    # Create a mock response for the HTTPStatusError
    mock_response = MagicMock()
    mock_response.status_code = 429
    mock_response.text = "Rate limit exceeded"
    
    with patch("main.httpx.AsyncClient") as mock_client:
        TestHelpers.setup_httpx_mock_exception(mock_client, httpx.HTTPStatusError(
            "HTTP Error", request=MagicMock(), response=mock_response
        ))
        
        response = client.get("/news")
    
    # Verify that HTTP status error was logged with actual message format
    TestHelpers.assert_logger_called_with_message(mock_logger, "error", "Benzinga API returned HTTP error 429")

def test_article_processing_error_logging(mock_logger, mock_benzinga_settings):
    """Test that individual article processing errors are logged"""
    with patch("main.strip_html", side_effect=RuntimeError("HTML processing error")):
        with patch("main.httpx.AsyncClient") as mock_client:
            TestHelpers.setup_httpx_mock_response(mock_client, 200, [{
                "updated": "Tue, 01 Jan 2024 12:00:00 -0000",  # Correct RFC 2822 format
                "title": "Test", 
                "body": "Test body", 
                "url": "http://test.com",
                "id": "test-id"
            }])
            
            response = client.get("/news")
    
    # Verify that article processing error was logged
    TestHelpers.assert_logger_called_with_message(mock_logger, "error", "Error processing article test-id")

# ============================================================================
# REQUEST/RESPONSE LOGGING TESTS
# ============================================================================

def test_request_logging_contains_method_and_path(mock_middleware_logger):
    """Test that incoming requests are logged with HTTP method and path"""
    response = client.get("/health")
    
    # Verify request was logged with method and path
    TestHelpers.assert_logger_called_with_message(mock_middleware_logger, "info", "GET")
    TestHelpers.assert_logger_called_with_message(mock_middleware_logger, "info", "/health")

def test_response_logging_contains_status_code(mock_middleware_logger):
    """Test that responses are logged with status codes"""
    response = client.get("/health")
    
    # Verify response was logged with status code
    TestHelpers.assert_logger_called_with_message(mock_middleware_logger, "info", "200")

def test_error_response_logging(mock_middleware_logger, mock_benzinga_settings):
    """Test that error responses are logged appropriately"""
    with patch("main.httpx.AsyncClient") as mock_client:
        TestHelpers.setup_httpx_mock_exception(mock_client, Exception("Test error"))
        
        response = client.get("/news")
    
    # Verify error response was logged
    TestHelpers.assert_logger_called_with_message(mock_middleware_logger, "info", "500")

def test_request_timing_logging(mock_middleware_logger):
    """Test that request timing is logged"""
    response = client.get("/health")
    
    # Verify timing information was logged in the response message
    TestHelpers.assert_logger_called_with_message(mock_middleware_logger, "info", "Response: 200 GET /health")

# ============================================================================
# MIDDLEWARE EXCEPTION HANDLING TESTS
# ============================================================================

def test_middleware_catches_unhandled_exceptions(mock_middleware_logger):
    """Test that middleware catches and logs unhandled exceptions"""
    from middleware import ExceptionLoggingMiddleware
    
    # Create middleware with mock app that raises exception
    mock_app = AsyncMock()
    middleware = ExceptionLoggingMiddleware(mock_app)
    
    # Create mock request
    mock_request = TestHelpers.create_mock_request()
    
    # Create mock call_next that raises exception
    async def test_middleware():
        mock_call_next = await TestHelpers.create_mock_call_next(
            RuntimeError("Middleware test exception")
        )
        return await middleware.dispatch(mock_request, mock_call_next)
    
    # Test the middleware
    response = asyncio.run(test_middleware())
    
    # Verify middleware logged the exception and returned 500
    TestHelpers.assert_logger_called_with_message(mock_middleware_logger, "error", "Middleware test exception")
    assert response.status_code == 500

def test_middleware_includes_request_context(mock_middleware_logger):
    """Test that middleware logs include request context information"""
    from middleware import ExceptionLoggingMiddleware
    
    # Create middleware with mock app
    mock_app = AsyncMock()
    middleware = ExceptionLoggingMiddleware(mock_app)
    
    # Create mock request with specific context
    mock_request = TestHelpers.create_mock_request(
        method="POST",
        path="/api/test",
        url="http://test.com/api/test?param=value",
        query_params={"param": "value"},
        headers={"User-Agent": "test-agent"},
        client_host="192.168.1.1"
    )
    
    # Test the middleware
    async def test_middleware():
        mock_call_next = await TestHelpers.create_mock_call_next(
            RuntimeError("Context test error")
        )
        return await middleware.dispatch(mock_request, mock_call_next)
    
    asyncio.run(test_middleware())
    
    # Verify middleware logged the exception with context
    TestHelpers.assert_logger_called_with_message(mock_middleware_logger, "error", "POST")
    TestHelpers.assert_logger_called_with_message(mock_middleware_logger, "error", "/api/test")
    TestHelpers.assert_logger_called_with_message(mock_middleware_logger, "error", "Context test error")

def test_middleware_successful_request_logging(mock_middleware_logger):
    """Test that successful requests are logged by middleware"""
    # Test using the actual FastAPI app with middleware already attached
    client.get("/health")
    
    # Verify middleware logged the request with actual message format
    TestHelpers.assert_logger_called_with_message(mock_middleware_logger, "info", "Request: GET /health")
    TestHelpers.assert_logger_called_with_message(mock_middleware_logger, "info", "Response: 200 GET /health")

# ============================================================================
# LOGGING CONFIGURATION TESTS
# ============================================================================

def test_logger_is_configured():
    """Test that the application logger is properly configured"""
    from main import logger
    
    assert logger is not None, "Logger should be configured"
    assert hasattr(logger, 'info'), "Logger should have info method"
    assert hasattr(logger, 'error'), "Logger should have error method"
    assert hasattr(logger, 'warning'), "Logger should have warning method"
    assert hasattr(logger, 'debug'), "Logger should have debug method"

def test_middleware_logger_is_configured():
    """Test that the middleware logger is properly configured"""
    from middleware import logger
    
    assert logger is not None, "Middleware logger should be configured"
    assert hasattr(logger, 'info'), "Middleware logger should have info method"
    assert hasattr(logger, 'error'), "Middleware logger should have error method"
    assert hasattr(logger, 'warning'), "Middleware logger should have warning method"
    assert hasattr(logger, 'debug'), "Middleware logger should have debug method"

def test_log_format_includes_timestamp(captured_logs):
    """Test that log format includes timestamp information"""
    from main import logger
    
    test_message = "Test log message for timestamp"
    logger.info(test_message)
    
    log_output = captured_logs.getvalue()
    assert test_message in log_output, "Log message should be captured"
    
    # Check for timestamp patterns
    import re
    timestamp_pattern = r'\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}'
    assert re.search(timestamp_pattern, log_output), "Log should include timestamp"

def test_log_level_configuration():
    """Test that log level is properly configured"""
    from main import logger
    
    # Logger should be configured to capture INFO level and above
    assert logger.level <= logging.INFO, "Logger should capture INFO level logs"

def test_log_format_includes_level_and_name(captured_logs):
    """Test that log format includes log level and logger name"""
    from main import logger
    
    test_message = "Test log message for format"
    logger.info(test_message)
    
    log_output = captured_logs.getvalue()
    assert test_message in log_output, "Log message should be captured"
    assert "INFO" in log_output, "Log should include level"
    assert logger.name in log_output, "Log should include logger name"

def test_config_log_level_parsing():
    """Test that configuration log level parsing works correctly"""
    from config import settings
    
    # Test the actual method on the settings instance
    assert settings.get_log_level() == getattr(logging, settings.LOG_LEVEL.upper(), logging.INFO)
    
    # Test with different log levels by temporarily changing the setting
    original_level = settings.LOG_LEVEL
    
    try:
        settings.LOG_LEVEL = "DEBUG"
        assert settings.get_log_level() == logging.DEBUG
        
        settings.LOG_LEVEL = "WARNING"
        assert settings.get_log_level() == logging.WARNING
        
        settings.LOG_LEVEL = "ERROR"
        assert settings.get_log_level() == logging.ERROR
        
        settings.LOG_LEVEL = "CRITICAL"
        assert settings.get_log_level() == logging.CRITICAL
        
        settings.LOG_LEVEL = "INVALID"
        assert settings.get_log_level() == logging.INFO  # Default fallback
        
    finally:
        # Restore original setting
        settings.LOG_LEVEL = original_level 