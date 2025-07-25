"""
Tests for error handler utilities.
"""

import pytest
import json
import neo4j
from utils.error_handler import ErrorHandler


class TestErrorHandler:
    """Test cases for ErrorHandler."""

    @pytest.fixture
    def error_handler(self):
        """Create ErrorHandler instance."""
        return ErrorHandler()

    def test_error_handler_initialization(self, error_handler):
        """Test error handler initialization."""
        # Assert
        assert error_handler is not None

    def test_format_neo4j_service_unavailable_error(self, error_handler):
        """Test formatting of Neo4j ServiceUnavailable error."""
        # Arrange
        error = neo4j.exceptions.ServiceUnavailable("Connection timeout")
        
        # Act
        response = error_handler.format_neo4j_error(error)
        
        # Assert
        assert response.status_code == 500
        response_data = json.loads(response.body.decode())
        assert response_data["status"] == "error"
        assert "Neo4j service unavailable" in response_data["detail"]

    def test_format_neo4j_auth_error(self, error_handler):
        """Test formatting of Neo4j AuthError."""
        # Arrange
        error = neo4j.exceptions.AuthError("Invalid credentials")
        
        # Act
        response = error_handler.format_neo4j_error(error)
        
        # Assert
        assert response.status_code == 500
        response_data = json.loads(response.body.decode())
        assert response_data["status"] == "error"
        assert "Neo4j authentication failed" in response_data["detail"]

    def test_format_neo4j_client_error(self, error_handler):
        """Test formatting of Neo4j ClientError."""
        # Arrange
        error = neo4j.exceptions.ClientError("Schema constraint violation")
        
        # Act
        response = error_handler.format_neo4j_error(error)
        
        # Assert
        assert response.status_code == 500
        response_data = json.loads(response.body.decode())
        assert response_data["status"] == "error"
        assert "Neo4j client error" in response_data["detail"]

    def test_format_neo4j_transient_error(self, error_handler):
        """Test formatting of Neo4j TransientError."""
        # Arrange
        error = neo4j.exceptions.TransientError("Temporary database unavailable")
        
        # Act
        response = error_handler.format_neo4j_error(error)
        
        # Assert
        assert response.status_code == 500
        response_data = json.loads(response.body.decode())
        assert response_data["status"] == "error"
        assert "Neo4j transient error" in response_data["detail"]

    def test_format_generic_neo4j_error(self, error_handler):
        """Test formatting of generic Neo4jError."""
        # Arrange
        error = neo4j.exceptions.Neo4jError("Generic database error")
        
        # Act
        response = error_handler.format_neo4j_error(error)
        
        # Assert
        assert response.status_code == 500
        response_data = json.loads(response.body.decode())
        assert response_data["status"] == "error"
        assert "Neo4j error" in response_data["detail"]

    def test_format_generic_exception(self, error_handler):
        """Test formatting of generic exception."""
        # Arrange
        error = Exception("Unexpected error occurred")
        
        # Act
        response = error_handler.format_generic_error(error)
        
        # Assert
        assert response.status_code == 500
        response_data = json.loads(response.body.decode())
        assert response_data["status"] == "error"
        assert "Unexpected error" in response_data["detail"]

    def test_format_schema_error(self, error_handler):
        """Test formatting of schema initialization error."""
        # Arrange
        message = "Schema creation failed"
        context = "All constraints failed to create"
        
        # Act
        response = error_handler.format_schema_error(message, context)
        
        # Assert
        assert response.status_code == 500
        response_data = json.loads(response.body.decode())
        assert response_data["status"] == "error"
        assert message in response_data["detail"]
        assert context in response_data["detail"]

    def test_format_success_response(self, error_handler):
        """Test formatting of success response."""
        # Arrange
        message = "Schema initialized"
        data = {"constraints_created": 4, "indexes_created": 5}
        
        # Act
        response = error_handler.format_success_response(message, data)
        
        # Assert
        assert response.status_code == 200
        response_data = json.loads(response.body.decode())
        assert response_data["status"] == message
        assert response_data["constraints_created"] == 4
        assert response_data["indexes_created"] == 5

    def test_format_validation_error(self, error_handler):
        """Test formatting of validation error."""
        # Arrange
        message = "Validation failed"
        errors = [{"field": "username", "message": "Username required"}]
        
        # Act
        response = error_handler.format_validation_error(message, errors)
        
        # Assert
        assert response.status_code == 400
        response_data = json.loads(response.body.decode())
        assert response_data["status"] == "error"
        assert response_data["detail"] == message
        assert response_data["validation_errors"] == errors

    def test_format_timeout_error(self, error_handler):
        """Test formatting of timeout error."""
        # Arrange
        message = "Connection timeout"
        timeout = 30.0
        
        # Act
        response = error_handler.format_timeout_error(message, timeout)
        
        # Assert
        assert response.status_code == 504
        response_data = json.loads(response.body.decode())
        assert response_data["status"] == "error"
        assert message in response_data["detail"]
        assert "30.0" in response_data["detail"]

    def test_format_health_check_error(self, error_handler):
        """Test formatting of health check error."""
        # Arrange
        message = "DB connection failed"
        
        # Act
        response = error_handler.format_health_check_error(message)
        
        # Assert
        assert response.status_code == 503
        response_data = json.loads(response.body.decode())
        assert response_data["status"] == "unhealthy"
        assert response_data["detail"] == message

    def test_create_error_detail_with_context(self, error_handler):
        """Test creation of error detail message with context."""
        # Arrange
        message = "Service error"
        context = "Connection failed"
        
        # Act
        detail = error_handler._create_error_detail(message, context)
        
        # Assert
        assert detail == "Service error: Connection failed"

    def test_create_error_detail_without_context(self, error_handler):
        """Test creation of error detail message without context."""
        # Arrange
        message = "Service error"
        context = ""
        
        # Act
        detail = error_handler._create_error_detail(message, context)
        
        # Assert
        assert detail == "Service error"

    def test_create_error_detail_with_none_context(self, error_handler):
        """Test creation of error detail message with None context."""
        # Arrange
        message = "Service error"
        context = None
        
        # Act
        detail = error_handler._create_error_detail(message, context)
        
        # Assert
        assert detail == "Service error"

    @pytest.mark.parametrize("error_class,expected_type", [
        (neo4j.exceptions.ServiceUnavailable, "Neo4j service unavailable"),
        (neo4j.exceptions.AuthError, "Neo4j authentication failed"),
        (neo4j.exceptions.ClientError, "Neo4j client error"),
        (neo4j.exceptions.TransientError, "Neo4j transient error"),
        (neo4j.exceptions.Neo4jError, "Neo4j error"),
        (ValueError, "Unexpected error"),
        (Exception, "Unexpected error"),
    ])
    def test_determine_error_type(self, error_handler, error_class, expected_type):
        """Test determination of error type from various exception types."""
        # Arrange
        error = error_class("Test error")
        
        # Act
        error_type = error_handler._determine_error_type(error)
        
        # Assert
        assert error_type == expected_type

    @pytest.mark.parametrize("input_message,expected_output", [
        ("Auth failed: password=secret123", "Auth failed: [REDACTED]"),
        ("Token error: token=abc123def", "Token error: [REDACTED]"),
        ("Key issue: key=mykey123", "Key issue: [REDACTED]"),
        ("Secret problem: secret=topsecret", "Secret problem: [REDACTED]"),
        ("Normal message without sensitive data", "Normal message without sensitive data"),
        ("Multiple issues: password=pass123 and token=tok456", "Multiple issues: [REDACTED] and [REDACTED]"),
    ])
    def test_sanitize_error_message(self, error_handler, input_message, expected_output):
        """Test sanitization of error messages with sensitive information."""
        # Act
        sanitized = error_handler._sanitize_error_message(input_message)
        
        # Assert
        assert sanitized == expected_output

    @pytest.mark.parametrize("error_class,expected_retryable", [
        (neo4j.exceptions.TransientError, True),
        (neo4j.exceptions.ServiceUnavailable, True),
        (neo4j.exceptions.AuthError, False),
        (neo4j.exceptions.ClientError, False),
        (ValueError, False),
        (Exception, False),
    ])
    def test_is_retryable_error(self, error_handler, error_class, expected_retryable):
        """Test identification of retryable errors."""
        # Arrange
        error = error_class("Test error")
        
        # Act
        is_retryable = error_handler.is_retryable_error(error)
        
        # Assert
        assert is_retryable == expected_retryable

    @pytest.mark.parametrize("error_class,expected_severity", [
        (neo4j.exceptions.AuthError, "high"),
        (neo4j.exceptions.ClientError, "high"),
        (neo4j.exceptions.ServiceUnavailable, "medium"),
        (neo4j.exceptions.TransientError, "medium"),
        (ValueError, "low"),
        (Exception, "low"),
    ])
    def test_get_error_severity(self, error_handler, error_class, expected_severity):
        """Test classification of error severity."""
        # Arrange
        error = error_class("Test error")
        
        # Act
        severity = error_handler.get_error_severity(error)
        
        # Assert
        assert severity == expected_severity 