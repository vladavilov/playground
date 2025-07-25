"""
Unit tests for ProjectProgressUpdateRequest Pydantic model.
Tests validation logic for project status update requests.
"""

import pytest
from pydantic import ValidationError

from models.project_rest import ProjectProgressUpdateRequest, ProjectStatus


class TestProjectProgressUpdateRequestValidation:
    """Test validation logic for ProjectProgressUpdateRequest model."""

    def test_progress_update_with_valid_counts(self):
        """Test valid progress update with processed and total counts."""
        request = ProjectProgressUpdateRequest(
            status=ProjectStatus.PROCESSING,
            processed_count=50,
            total_count=100
        )
        
        assert request.status == ProjectStatus.PROCESSING
        assert request.processed_count == 50
        assert request.total_count == 100
        assert request.error_message is None

    def test_progress_update_with_zero_processed_count(self):
        """Test progress update with zero processed count is valid."""
        request = ProjectProgressUpdateRequest(
            status=ProjectStatus.PROCESSING,
            processed_count=0,
            total_count=100
        )
        
        assert request.processed_count == 0
        assert request.total_count == 100

    def test_progress_update_with_equal_counts(self):
        """Test progress update where processed equals total count."""
        request = ProjectProgressUpdateRequest(
            status=ProjectStatus.PROCESSING,
            processed_count=100,
            total_count=100
        )
        
        assert request.processed_count == 100
        assert request.total_count == 100

    def test_status_reset_without_counts(self):
        """Test status reset without progress counts."""
        request = ProjectProgressUpdateRequest(
            status=ProjectStatus.ACTIVE
        )
        
        assert request.status == ProjectStatus.ACTIVE
        assert request.processed_count is None
        assert request.total_count is None
        assert request.error_message is None

    def test_status_reset_with_error_message(self):
        """Test status reset with error message."""
        request = ProjectProgressUpdateRequest(
            status=ProjectStatus.INACTIVE,
            error_message="Processing failed due to invalid document format"
        )
        
        assert request.status == ProjectStatus.INACTIVE
        assert request.error_message == "Processing failed due to invalid document format"
        assert request.processed_count is None
        assert request.total_count is None

    def test_processed_count_cannot_exceed_total_count(self):
        """Test that processed_count cannot be greater than total_count."""
        with pytest.raises(ValidationError) as exc_info:
            ProjectProgressUpdateRequest(
                status=ProjectStatus.PROCESSING,
                processed_count=150,
                total_count=100
            )
        
        error = exc_info.value.errors()[0]
        assert "processed_count cannot exceed total_count" in error["msg"]

    def test_negative_processed_count_invalid(self):
        """Test that negative processed_count is invalid."""
        with pytest.raises(ValidationError) as exc_info:
            ProjectProgressUpdateRequest(
                status=ProjectStatus.PROCESSING,
                processed_count=-10,
                total_count=100
            )
        
        error = exc_info.value.errors()[0]
        assert "Input should be greater than or equal to 0" in error["msg"]

    def test_negative_total_count_invalid(self):
        """Test that negative total_count is invalid."""
        with pytest.raises(ValidationError) as exc_info:
            ProjectProgressUpdateRequest(
                status=ProjectStatus.PROCESSING,
                processed_count=50,
                total_count=-100
            )
        
        error = exc_info.value.errors()[0]
        assert "Input should be greater than 0" in error["msg"]

    def test_empty_status_invalid(self):
        """Test that empty status string is invalid."""
        with pytest.raises(ValidationError) as exc_info:
            ProjectProgressUpdateRequest(
                status=""
            )
        
        error = exc_info.value.errors()[0]
        assert "Input should be 'active', 'inactive', 'archived' or 'processing'" in error["msg"]

    def test_whitespace_only_status_invalid(self):
        """Test that whitespace-only status is invalid."""
        with pytest.raises(ValidationError) as exc_info:
            ProjectProgressUpdateRequest(
                status="   "
            )
        
        error = exc_info.value.errors()[0]
        assert "Input should be 'active', 'inactive', 'archived' or 'processing'" in error["msg"]

    def test_empty_error_message_invalid(self):
        """Test that empty error_message string is invalid."""
        with pytest.raises(ValidationError) as exc_info:
            ProjectProgressUpdateRequest(
                status="inactive",
                error_message=""
            )
        
        error = exc_info.value.errors()[0]
        assert "String should have at least 1 character" in error["msg"]

    def test_whitespace_only_error_message_invalid(self):
        """Test that whitespace-only error_message is invalid."""
        with pytest.raises(ValidationError) as exc_info:
            ProjectProgressUpdateRequest(
                status="inactive",
                error_message="   "
            )
        
        error = exc_info.value.errors()[0]
        assert "Value error, error_message cannot be empty" in error["msg"]

    def test_non_processing_status_ignores_counts(self):
        """Test that non-PROCESSING status ignores count fields."""
        request = ProjectProgressUpdateRequest(
            status="active",
            processed_count=50,  # Should be ignored
            total_count=100      # Should be ignored
        )
        
        assert request.status == "active"
        # Counts should be present but not validated for non-PROCESSING status
        assert request.processed_count == 50
        assert request.total_count == 100

    def test_valid_status_values(self):
        """Test that common status values are accepted."""
        valid_statuses = ["active", "inactive", "processing", "archived"]
        
        for status in valid_statuses:
            if status == "processing":
                request = ProjectProgressUpdateRequest(
                    status=status,
                    processed_count=10,
                    total_count=20
                )
            else:
                request = ProjectProgressUpdateRequest(
                    status=status
                )
            assert request.status == status