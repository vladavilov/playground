"""
Tests for Pydantic schemas and model conversions.
Combines schema validation and model conversion tests.
"""

from datetime import datetime, timezone
from uuid import uuid4

import pytest
from pydantic import ValidationError

from models.project_db import Project
from models.project_rest import ProjectSet, ProjectResponse, ProjectStatus


# ProjectSet Schema Tests

def test_project_set_with_required_name_only():
    """Test ProjectSet creation with only required name field."""
    project_data = ProjectSet(name="Test Project")

    assert project_data.name == "Test Project"
    assert project_data.description is None
    assert project_data.gitlab_url is None
    assert project_data.gitlab_repository_url is None
    assert project_data.status == ProjectStatus.ACTIVE


def test_project_set_with_all_fields():
    """Test ProjectSet creation with all fields provided."""
    project_data = ProjectSet(
        name="Full Test Project",
        description="A comprehensive test project",
        gitlab_url="https://gitlab.example.com/group/project",
        gitlab_repository_url="https://gitlab.example.com/group/project.git",
        status=ProjectStatus.INACTIVE
    )

    assert project_data.name == "Full Test Project"
    assert project_data.description == "A comprehensive test project"
    assert str(project_data.gitlab_url) == "https://gitlab.example.com/group/project"
    assert str(project_data.gitlab_repository_url) == "https://gitlab.example.com/group/project.git"
    assert project_data.status == ProjectStatus.INACTIVE


def test_project_set_name_validation_empty_string():
    """Test that empty string name raises ValidationError."""
    with pytest.raises(ValidationError) as exc_info:
        ProjectSet(name="")

    errors = exc_info.value.errors()
    assert len(errors) == 1
    assert errors[0]["type"] == "string_too_short"
    assert "name" in str(errors[0]["loc"])


def test_project_set_name_validation_too_long():
    """Test that name longer than 255 characters raises ValidationError."""
    long_name = "x" * 256
    with pytest.raises(ValidationError) as exc_info:
        ProjectSet(name=long_name)

    errors = exc_info.value.errors()
    assert len(errors) == 1
    assert errors[0]["type"] == "string_too_long"
    assert "name" in str(errors[0]["loc"])


def test_project_set_name_validation_whitespace_only():
    """Test that whitespace-only name raises ValidationError."""
    with pytest.raises(ValidationError) as exc_info:
        ProjectSet(name="   ")

    errors = exc_info.value.errors()
    assert len(errors) == 1
    assert errors[0]["type"] == "value_error"


def test_project_set_gitlab_url_validation_invalid_url():
    """Test that invalid GitLab URL raises ValidationError."""
    with pytest.raises(ValidationError) as exc_info:
        ProjectSet(name="Test", gitlab_url="not-a-url")

    errors = exc_info.value.errors()
    assert len(errors) == 1
    assert errors[0]["type"] == "url_parsing"
    assert "gitlab_url" in str(errors[0]["loc"])


def test_project_set_gitlab_repository_url_validation_invalid_url():
    """Test that invalid GitLab repository URL raises ValidationError."""
    with pytest.raises(ValidationError) as exc_info:
        ProjectSet(name="Test", gitlab_repository_url="invalid-url")

    errors = exc_info.value.errors()
    assert len(errors) == 1
    assert errors[0]["type"] == "url_parsing"
    assert "gitlab_repository_url" in str(errors[0]["loc"])


def test_project_set_status_validation_invalid_value():
    """Test that invalid status value raises ValidationError."""
    with pytest.raises(ValidationError) as exc_info:
        ProjectSet(name="Test", status="invalid_status")

    errors = exc_info.value.errors()
    assert len(errors) == 1
    assert "status" in str(errors[0]["loc"])


def test_project_set_missing_required_name():
    """Test that missing required name field raises ValidationError."""
    with pytest.raises(ValidationError) as exc_info:
        ProjectSet()

    errors = exc_info.value.errors()
    assert len(errors) == 1
    assert errors[0]["type"] == "missing"
    assert "name" in str(errors[0]["loc"])


# ProjectResponse Schema Tests

def test_project_response_from_dict():
    """Test ProjectResponse creation from dictionary."""
    project_dict = {
        "id": str(uuid4()),
        "name": "Test Project",
        "description": "Test description",
        "gitlab_url": "https://gitlab.example.com/test",
        "gitlab_repository_url": "https://gitlab.example.com/test.git",
        "status": "active",
        "created_by": "user123",
        "created_at": datetime.now(),
        "updated_at": datetime.now()
    }

    response = ProjectResponse(**project_dict)

    assert str(response.id) == project_dict["id"]
    assert response.name == project_dict["name"]
    assert response.description == project_dict["description"]
    assert response.gitlab_url == project_dict["gitlab_url"]
    assert response.gitlab_repository_url == project_dict["gitlab_repository_url"]
    assert response.status == project_dict["status"]
    assert response.created_by == project_dict["created_by"]
    assert response.created_at == project_dict["created_at"]
    assert response.updated_at == project_dict["updated_at"]


def test_project_response_from_sqlalchemy_model():
    """Test ProjectResponse creation from SQLAlchemy model using from_attributes."""
    class MockProject:
        """Mock SQLAlchemy Project object"""
        def __init__(self):
            self.id = uuid4()
            self.name = "SQLAlchemy Project"
            self.description = "From SQLAlchemy"
            self.gitlab_url = "https://gitlab.example.com/sqlalchemy"
            self.gitlab_repository_url = "https://gitlab.example.com/sqlalchemy.git"
            self.status = "active"
            self.created_by = "user456"
            self.created_at = datetime.now()
            self.updated_at = datetime.now()

    mock_project = MockProject()
    response = ProjectResponse.model_validate(mock_project)

    assert response.id == mock_project.id
    assert response.name == mock_project.name
    assert response.description == mock_project.description
    assert response.gitlab_url == mock_project.gitlab_url
    assert response.gitlab_repository_url == mock_project.gitlab_repository_url
    assert response.status == mock_project.status
    assert response.created_by == mock_project.created_by
    assert response.created_at == mock_project.created_at
    assert response.updated_at == mock_project.updated_at


def test_project_response_required_fields_validation():
    """Test that missing required fields raise ValidationError."""
    with pytest.raises(ValidationError) as exc_info:
        ProjectResponse()

    errors = exc_info.value.errors()
    required_fields = {"id", "name", "created_by", "created_at", "updated_at"}
    error_fields = {str(error["loc"][0]) for error in errors if error["type"] == "missing"}

    assert required_fields.issubset(error_fields)


def test_project_response_id_validation_invalid_uuid():
    """Test that invalid UUID for id raises ValidationError."""
    with pytest.raises(ValidationError) as exc_info:
        ProjectResponse(
            id="not-a-uuid",
            name="Test",
            description=None,
            gitlab_url=None,
            gitlab_repository_url=None,
            status="active",
            created_by="user",
            created_at=datetime.now(),
            updated_at=datetime.now()
        )

    errors = exc_info.value.errors()
    uuid_errors = [e for e in errors if e["type"] == "uuid_parsing"]
    assert len(uuid_errors) == 1
    assert "id" in str(uuid_errors[0]["loc"])


def test_project_response_serialization():
    """Test that ProjectResponse can be serialized to dict."""
    project_id = uuid4()
    now = datetime.now()

    response = ProjectResponse(
        id=project_id,
        name="Serialization Test",
        description="Test serialization",
        gitlab_url="https://gitlab.example.com/serialize",
        gitlab_repository_url="https://gitlab.example.com/serialize.git",
        status="active",
        created_by="user789",
        created_at=now,
        updated_at=now
    )

    serialized = response.model_dump()

    assert serialized["id"] == project_id
    assert serialized["name"] == "Serialization Test"
    assert serialized["description"] == "Test serialization"
    assert serialized["gitlab_url"] == "https://gitlab.example.com/serialize"
    assert serialized["gitlab_repository_url"] == "https://gitlab.example.com/serialize.git"
    assert serialized["status"] == "active"
    assert serialized["created_by"] == "user789"
    assert serialized["created_at"] == now
    assert serialized["updated_at"] == now


# Model Conversion Tests

def test_project_set_to_sqlalchemy_project_conversion():
    """Test converting ProjectSet to SQLAlchemy Project model."""
    project_set = ProjectSet(
        name="Test Project",
        description="Test project description",
        gitlab_url="https://gitlab.internal/test-project",
        gitlab_repository_url="https://gitlab.internal/test-project.git",
        status=ProjectStatus.ACTIVE
    )
    user_id = "test-user-123"

    # Convert ProjectSet to Project
    project = Project(
        name=project_set.name,
        description=project_set.description,
        gitlab_url=str(project_set.gitlab_url) if project_set.gitlab_url else None,
        gitlab_repository_url=str(project_set.gitlab_repository_url) if project_set.gitlab_repository_url else None,
        status=project_set.status.value,
        created_by=user_id
    )

    assert project.name == project_set.name
    assert project.description == project_set.description
    assert project.gitlab_url == str(project_set.gitlab_url)
    assert project.gitlab_repository_url == str(project_set.gitlab_repository_url)
    assert project.status == project_set.status.value
    assert project.created_by == user_id


def test_sqlalchemy_project_to_project_response_conversion():
    """Test converting SQLAlchemy Project to ProjectResponse model."""
    project = Project(
        id=uuid4(),
        name="Test Project",
        description="Test project description",
        gitlab_url="https://gitlab.internal/test-project",
        gitlab_repository_url="https://gitlab.internal/test-project.git",
        status="active",
        created_by="test-user-123",
        created_at=datetime.now(timezone.utc),
        updated_at=datetime.now(timezone.utc)
    )

    # Convert Project to ProjectResponse
    project_response = ProjectResponse.model_validate(project)

    assert project_response.id == project.id
    assert project_response.name == project.name
    assert project_response.description == project.description
    assert project_response.gitlab_url == project.gitlab_url
    assert project_response.gitlab_repository_url == project.gitlab_repository_url
    assert project_response.status == project.status
    assert project_response.created_by == project.created_by
    assert project_response.created_at == project.created_at
    assert project_response.updated_at == project.updated_at


def test_partial_update_with_project_set():
    """Test partial updates using ProjectSet model."""
    original_project = Project(
        id=uuid4(),
        name="Original Project",
        description="Original description",
        gitlab_url="https://gitlab.internal/original-project",
        gitlab_repository_url="https://gitlab.internal/original-project.git",
        status="active",
        created_by="test-user-123",
        created_at=datetime.now(timezone.utc),
        updated_at=datetime.now(timezone.utc)
    )

    # Create partial update data
    update_data = ProjectSet(
        name="Updated Project Name",
        description="Updated description"
    )

    # Apply partial update
    update_dict = update_data.model_dump(exclude_unset=True)
    for field, value in update_dict.items():
        setattr(original_project, field, value)

    assert original_project.name == "Updated Project Name"
    assert original_project.description == "Updated description"
    # These should remain unchanged
    assert original_project.gitlab_url == "https://gitlab.internal/original-project"
    assert original_project.gitlab_repository_url == "https://gitlab.internal/original-project.git"
    assert original_project.status == "active"
    assert original_project.created_by == "test-user-123"


def test_model_dump_exclude_unset():
    """Test model_dump with exclude_unset for partial updates."""
    # Create ProjectSet with only some fields set
    partial_update = ProjectSet(
        name="Updated Name",
        description="Updated description"
        # gitlab_url and gitlab_repository_url are not set
        # status is not explicitly set, so it will use default but be excluded by exclude_unset
    )

    update_dict = partial_update.model_dump(exclude_unset=True)

    assert "name" in update_dict
    assert "description" in update_dict
    assert "gitlab_url" not in update_dict  # This was not set
    assert "gitlab_repository_url" not in update_dict  # This was not set
    # status is not included because it wasn't explicitly set, even though it has a default

    assert update_dict["name"] == "Updated Name"
    assert update_dict["description"] == "Updated description"

    # Test with status explicitly set
    partial_update_with_status = ProjectSet(
        name="Updated Name",
        description="Updated description",
        status=ProjectStatus.ACTIVE
    )
    update_dict_with_status = partial_update_with_status.model_dump(exclude_unset=True)
    assert "status" in update_dict_with_status
    assert update_dict_with_status["status"] == ProjectStatus.ACTIVE
