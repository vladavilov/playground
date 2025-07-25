from utils.error_handler import ErrorHandler
import neo4j

def test_error_handler_initialization():
    """Test error handler initialization."""
    handler = ErrorHandler()
    assert handler is not None

def test_format_service_unavailable_error():
    """Test formatting of ServiceUnavailable error."""
    handler = ErrorHandler()
    error = neo4j.exceptions.ServiceUnavailable("Connection timeout")
    response = handler.format_neo4j_error(error)
    assert response.status_code == 500
    assert "Neo4j service unavailable" in response.body.decode()

def test_format_auth_error():
    """Test formatting of AuthError."""
    handler = ErrorHandler()
    error = neo4j.exceptions.AuthError("Invalid credentials")
    response = handler.format_neo4j_error(error)
    assert response.status_code == 500
    assert "Neo4j authentication failed" in response.body.decode()

def test_format_client_error():
    """Test formatting of ClientError."""
    handler = ErrorHandler()
    error = neo4j.exceptions.ClientError("Schema constraint violation")
    response = handler.format_neo4j_error(error)
    assert response.status_code == 500
    assert "Neo4j client error" in response.body.decode()

def test_format_transient_error():
    """Test formatting of TransientError."""
    handler = ErrorHandler()
    error = neo4j.exceptions.TransientError("Temporary database unavailable")
    response = handler.format_neo4j_error(error)
    assert response.status_code == 500
    assert "Neo4j transient error" in response.body.decode()

def test_format_generic_neo4j_error():
    """Test formatting of generic Neo4jError."""
    handler = ErrorHandler()
    error = neo4j.exceptions.Neo4jError("Generic database error")
    response = handler.format_neo4j_error(error)
    assert response.status_code == 500
    assert "Neo4j error" in response.body.decode()

def test_format_generic_exception():
    """Test formatting of generic exception."""
    handler = ErrorHandler()
    error = Exception("Unexpected error occurred")
    response = handler.format_generic_error(error)
    assert response.status_code == 500
    assert "Unexpected error" in response.body.decode()

def test_format_schema_initialization_error():
    """Test formatting of schema initialization error."""
    handler = ErrorHandler()
    response = handler.format_schema_error("Schema creation failed", "All constraints failed to create")
    assert response.status_code == 500
    assert "Schema creation failed" in response.body.decode()

def test_format_success_response():
    """Test formatting of success response."""
    handler = ErrorHandler()
    data = {"constraints_created": 4, "indexes_created": 5}
    response = handler.format_success_response("Schema initialized", data)
    assert response.status_code == 200
    assert "Schema initialized" in response.body.decode()
    assert "constraints_created" in response.body.decode()

def test_create_error_detail_message():
    """Test creation of error detail message."""
    handler = ErrorHandler()
    detail = handler._create_error_detail("Service error", "Connection failed")
    assert detail == "Service error: Connection failed"
    detail_no_context = handler._create_error_detail("Service error", "")
    assert detail_no_context == "Service error"

def test_determine_error_type_from_exception():
    """Test determination of error type from exception."""
    handler = ErrorHandler()
    assert handler._determine_error_type(neo4j.exceptions.ServiceUnavailable()) == "Neo4j service unavailable"
    assert handler._determine_error_type(neo4j.exceptions.AuthError()) == "Neo4j authentication failed"
    assert handler._determine_error_type(ValueError()) == "Unexpected error"

def test_sanitize_error_message():
    """Test sanitization of error message."""
    handler = ErrorHandler()
    sanitized = handler._sanitize_error_message("Auth failed: password=secret123")
    assert "secret123" not in sanitized
    assert "[REDACTED]" in sanitized

def test_format_validation_error():
    """Test formatting of validation error."""
    handler = ErrorHandler()
    errors = [{"field": "username", "message": "Username required"}]
    response = handler.format_validation_error("Validation failed", errors)
    assert response.status_code == 400
    assert "Validation failed" in response.body.decode()
    assert "validation_errors" in response.body.decode()

def test_format_timeout_error():
    """Test formatting of timeout error."""
    handler = ErrorHandler()
    response = handler.format_timeout_error("Connection timeout", 30.0)
    assert response.status_code == 504
    assert "Connection timeout" in response.body.decode()

def test_is_retryable_error():
    """Test identification of retryable error."""
    handler = ErrorHandler()
    assert handler.is_retryable_error(neo4j.exceptions.TransientError()) is True
    assert handler.is_retryable_error(neo4j.exceptions.AuthError()) is False

def test_get_error_severity():
    """Test classification of error severity."""
    handler = ErrorHandler()
    assert handler.get_error_severity(neo4j.exceptions.AuthError()) == "high"
    assert handler.get_error_severity(neo4j.exceptions.TransientError()) == "medium"
    assert handler.get_error_severity(Exception()) == "low"

def test_format_health_check_error():
    """Test formatting of health check error."""
    handler = ErrorHandler()
    response = handler.format_health_check_error("DB connection failed")
    assert response.status_code == 503
    assert "unhealthy" in response.body.decode() 