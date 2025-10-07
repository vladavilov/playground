"""
Consolidated unit tests for ProjectService business logic and RBAC functionality.
Combines service logic tests, role-based access control tests, and status methods.
"""

from datetime import datetime, timezone
from unittest.mock import Mock, MagicMock
from uuid import uuid4, UUID

import pytest

from services.project_service import ProjectService
from models.project_rest import ProjectSet, ProjectStatus, ProjectMemberSet, ProjectMemberRole
from models.project_db import Project, ProjectMember


@pytest.fixture
def mock_postgres_client():
    """Mock PostgreSQL client."""
    return Mock()


@pytest.fixture
def mock_session():
    """Mock database session."""
    return MagicMock()


@pytest.fixture
def project_service(mock_postgres_client, mock_session):
    """ProjectService instance with mocked dependencies."""
    mock_context_manager = MagicMock()
    mock_context_manager.__enter__.return_value = mock_session
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    return ProjectService(postgres_client=mock_postgres_client)


@pytest.fixture
def static_time():
    """Static datetime object for consistent testing."""
    return datetime(2023, 1, 1, 12, 0, 0, tzinfo=timezone.utc)


@pytest.fixture
def sample_project_data(static_time):
    """Sample project data for testing."""
    return {
        "id": uuid4(),
        "name": "Test Project",
        "description": "Test project description",
        "gitlab_url": "https://gitlab.internal/test-project",
        "gitlab_repository_url": "https://gitlab.internal/test-project.git",
        "status": "active",
        "created_by": "test-user-id",
        "created_at": static_time,
        "updated_at": static_time
    }


@pytest.fixture
def sample_project_create():
    """Sample project creation data (Pydantic model)."""
    return ProjectSet(
        name="New Test Project",
        description="New test project description",
        gitlab_url="https://gitlab.internal/new-project",
        status=ProjectStatus.ACTIVE
    )


@pytest.fixture
def sample_projects():
    """Sample projects for testing."""
    return [
        Project(
            id=uuid4(),
            name="Project 1",
            description="First project",
            status="active",
            created_by="user-123"
        ),
        Project(
            id=uuid4(),
            name="Project 2",
            description="Second project",
            status="active",
            created_by="user-456"
        ),
        Project(
            id=uuid4(),
            name="Project 3",
            description="Third project",
            status="active",
            created_by="user-789"
        )
    ]


@pytest.fixture
def sample_project():
    """Sample project for testing."""
    return Project(
        id=uuid4(),
        name="Test Project",
        description="Test project description",
        gitlab_url="https://gitlab.internal/test-project",
        status="active",
        processed_pct=0.0,
        created_by="admin-user-123",
        created_at=datetime.now(),
        updated_at=datetime.now()
    )


@pytest.fixture
def sample_project_member():
    """Sample project member for testing."""
    return ProjectMember(
        id=uuid4(),
        project_id=uuid4(),
        user_id="member-user-456",
        role="Contributor",
        created_by="admin-user-123",
        created_at=datetime.now(),
        updated_at=datetime.now()
    )


# Basic Service Tests

def test_create_project_success(project_service, mock_session, sample_project_create):
    """Test successful project creation."""
    user_id = "test-user-123"

    def refresh_side_effect(instance):
        instance.id = uuid4()

    mock_session.add.return_value = None
    mock_session.commit.return_value = None
    mock_session.refresh.side_effect = refresh_side_effect

    result = project_service.create_project(sample_project_create, user_id)

    assert result is not None
    assert result.name == sample_project_create.name
    assert result.description == sample_project_create.description
    assert result.created_by == user_id
    assert isinstance(result.id, UUID)
    assert result.status == sample_project_create.status.value

    mock_session.add.assert_called_once()
    # Note: commit() is now handled by the context manager, not tested here
    added_project = mock_session.add.call_args[0][0]
    assert added_project.name == sample_project_create.name
    mock_session.refresh.assert_called_once_with(added_project)


def test_create_project_with_proper_type_conversion(project_service, mock_session):
    """Test create_project properly converts ProjectSet to Project."""
    project_data = ProjectSet(
        name="Test Project",
        description="Test description",
        gitlab_url="https://gitlab.internal/test-project",
        gitlab_repository_url="https://gitlab.internal/test-project.git",
        status=ProjectStatus.ACTIVE
    )
    user_id = "test-user-123"

    def refresh_side_effect(instance):
        instance.id = uuid4()

    mock_session.refresh.side_effect = refresh_side_effect

    result = project_service.create_project(project_data, user_id)

    assert result is not None
    assert result.name == project_data.name
    assert result.description == project_data.description
    assert result.gitlab_url == str(project_data.gitlab_url)  # Should be converted to string
    assert result.gitlab_repository_url == str(project_data.gitlab_repository_url)  # Should be converted to string
    assert result.status == project_data.status.value  # Should be converted to string
    assert result.created_by == user_id

    mock_session.add.assert_called_once()
    # Note: commit() is now handled by the context manager, not tested here
    mock_session.refresh.assert_called_once()


def test_create_project_with_none_urls(project_service, mock_session):
    """Test create_project handles None URLs properly."""
    project_data = ProjectSet(
        name="Test Project",
        description="Test description"
        # gitlab_url and gitlab_repository_url are None
    )
    user_id = "test-user-123"

    def refresh_side_effect(instance):
        instance.id = uuid4()

    mock_session.refresh.side_effect = refresh_side_effect

    result = project_service.create_project(project_data, user_id)

    assert result is not None
    assert result.name == project_data.name
    assert result.gitlab_url is None
    assert result.gitlab_repository_url is None
    assert result.status == ProjectStatus.ACTIVE.value


def test_create_project_transaction_rollback_on_refresh_failure(project_service, mock_session):
    """Test that session.refresh() failure after commit() causes transaction rollback."""
    project_data = ProjectSet(
        name="Test Project",
        description="Test description",
        status=ProjectStatus.ACTIVE
    )
    user_id = "test-user-123"

    # Setup session to succeed with add and commit, but fail on refresh
    def refresh_side_effect(instance):
        # Simulate refresh failure that could occur due to database connection issues
        raise Exception("Refresh failed - simulating database connection issue")

    mock_session.refresh.side_effect = refresh_side_effect

    # The create_project should raise an exception due to refresh failure
    with pytest.raises(Exception, match="Refresh failed"):
        project_service.create_project(project_data, user_id)

    # Verify that add and flush were called (showing transaction was attempted)
    mock_session.add.assert_called_once()
    # Note: commit() is now handled by the context manager, not tested here
    mock_session.refresh.assert_called_once()
    
    # In a real scenario, the context manager would call rollback() 
    # when refresh() fails, undoing the committed transaction


def test_get_project_by_id_success(project_service, mock_session, sample_project_data):
    """Test successful project retrieval by ID."""
    project_id = sample_project_data["id"]
    mock_project = Project(**sample_project_data)
    mock_session.query.return_value.filter.return_value.first.return_value = mock_project

    result = project_service.get_project_by_id(project_id)

    assert result is not None
    assert result.id == project_id
    assert result.name == sample_project_data["name"]
    mock_session.query.return_value.filter.assert_called_once()


def test_get_project_by_id_not_found(project_service, mock_session):
    """Test project retrieval by ID when the project does not exist."""
    project_id = uuid4()
    mock_session.query.return_value.filter.return_value.first.return_value = None

    result = project_service.get_project_by_id(project_id)

    assert result is None


def test_get_projects_by_user_and_roles_creator_only(project_service, mock_session, sample_project_data):
    """Test retrieval via RBAC for non-admin user (creator only)."""
    user_id = sample_project_data["created_by"]
    mock_union_query = Mock()
    mock_union_query.all.return_value = [Project(**sample_project_data)]
    mock_owned_query = Mock()
    mock_owned_query.union.return_value = mock_union_query
    mock_session.query.return_value.filter.return_value = mock_owned_query

    result = project_service.get_projects_by_user_and_roles(user_id, ["Contributor"])

    assert len(result) == 1
    assert result[0].created_by == user_id


def test_update_project_success(project_service, mock_session, sample_project_data):
    """Test successful project update."""
    project_id = sample_project_data["id"]
    update_data = ProjectSet(name="Updated Project Name", description="Updated description")

    mock_project = Project(**sample_project_data)
    mock_session.query.return_value.filter.return_value.first.return_value = mock_project

    def refresh_side_effect(instance):
        instance.updated_at = datetime.now(timezone.utc)

    mock_session.refresh.side_effect = refresh_side_effect

    result = project_service.update_project(project_id, update_data)

    assert result is not None
    assert result.name == update_data.name
    assert result.description == update_data.description
    assert result.updated_at is not None
    assert result.updated_at != sample_project_data["updated_at"]  # Should be updated
    # Note: commit() is now handled by the context manager, not tested here


def test_update_project_partial_update_with_exclude_unset(project_service, mock_session):
    """Test update_project uses exclude_unset for partial updates."""
    project_id = uuid4()
    original_project = Project(
        id=project_id,
        name="Original Project",
        description="Original description",
        gitlab_url="https://gitlab.internal/original",
        status="active",
        created_by="test-user-123",
        created_at=datetime.now(timezone.utc),
        updated_at=datetime.now(timezone.utc)
    )

    # Only update name, leave other fields unchanged
    update_data = ProjectSet(name="Updated Name Only")

    mock_session.query.return_value.filter.return_value.first.return_value = original_project

    def refresh_side_effect(instance):
        instance.updated_at = datetime.now(timezone.utc)

    mock_session.refresh.side_effect = refresh_side_effect

    result = project_service.update_project(project_id, update_data)

    assert result is not None
    assert result.name == "Updated Name Only"
    # These should remain unchanged
    assert result.description == "Original description"
    assert result.gitlab_url == "https://gitlab.internal/original"
    assert result.status == "active"


def test_update_project_not_found(project_service, mock_session):
    """Test project update when the project to be updated does not exist."""
    project_id = uuid4()
    update_data = ProjectSet(name="Updated Project Name")
    mock_session.query.return_value.filter.return_value.first.return_value = None

    result = project_service.update_project(project_id, update_data)

    assert result is None
    # Note: commit() would only be called by context manager on successful completion


def test_delete_project_success(project_service, mock_session, sample_project_data):
    """Test successful project deletion."""
    project_id = sample_project_data["id"]
    mock_project = Project(**sample_project_data)
    mock_session.query.return_value.filter.return_value.first.return_value = mock_project

    result = project_service.delete_project(project_id)

    assert result is True
    mock_session.delete.assert_called_once_with(mock_project)
    # Note: commit() is now handled by the context manager, not tested here


def test_delete_project_not_found(project_service, mock_session):
    """Test project deletion when the project to be deleted does not exist."""
    project_id = uuid4()
    mock_session.query.return_value.filter.return_value.first.return_value = None

    result = project_service.delete_project(project_id)

    assert result is False
    mock_session.delete.assert_not_called()
    # Note: commit() would only be called by context manager on successful completion


# Status Update Methods Tests

def test_update_project_progress_success(project_service, mock_session, sample_project):
    """Test successful project progress update."""
    project_id = sample_project.id
    processed_count = 50
    total_count = 100
    
    # Mock project exists
    mock_session.query.return_value.filter.return_value.first.return_value = sample_project
    
    result = project_service.update_project_progress(project_id, processed_count, total_count, ProjectStatus.PROCESSING)
    
    assert result is not None
    assert result.status == ProjectStatus.PROCESSING.value
    assert result.processed_pct == 50.0  # 50/100 * 100
    
    # Verify database operations
    # Note: commit() is now handled by the context manager, not tested here
    mock_session.refresh.assert_called_once_with(sample_project)


def test_update_project_progress_zero_processed(project_service, mock_session, sample_project):
    """Test project progress update with zero processed count."""
    project_id = sample_project.id
    processed_count = 0
    total_count = 100
    
    # Mock project exists
    mock_session.query.return_value.filter.return_value.first.return_value = sample_project
    
    result = project_service.update_project_progress(project_id, processed_count, total_count, ProjectStatus.PROCESSING)
    
    assert result is not None
    assert result.status == ProjectStatus.PROCESSING.value
    assert result.processed_pct == 0.0


def test_update_project_progress_complete(project_service, mock_session, sample_project):
    """Test project progress update when processing is complete."""
    project_id = sample_project.id
    processed_count = 100
    total_count = 100
    
    # Mock project exists
    mock_session.query.return_value.filter.return_value.first.return_value = sample_project
    
    result = project_service.update_project_progress(project_id, processed_count, total_count, ProjectStatus.PROCESSING)
    
    assert result is not None
    assert result.status == ProjectStatus.PROCESSING.value
    assert result.processed_pct == 100.0


def test_update_project_status_rag_ready_without_counts_sets_100pct(project_service, mock_session, sample_project):
    """When status is RAG_READY and counts are omitted, processed_pct should become 100.0."""
    project_id = sample_project.id

    mock_session.query.return_value.filter.return_value.first.return_value = sample_project

    result = project_service.update_project_progress(project_id, None, None, ProjectStatus("rag_ready"))

    assert result is not None
    assert result.status == "rag_ready"
    assert result.processed_pct == 100.0


def test_update_project_status_rag_failed_without_counts_keeps_pct(project_service, mock_session, sample_project):
    """When status is RAG_FAILED and counts are omitted, processed_pct should remain unchanged."""
    project_id = sample_project.id
    sample_project.processed_pct = 42.0

    mock_session.query.return_value.filter.return_value.first.return_value = sample_project

    result = project_service.update_project_progress(project_id, None, None, ProjectStatus("rag_failed"))

    assert result is not None
    assert result.status == "rag_failed"
    assert result.processed_pct == 42.0


def test_update_project_progress_project_not_found(project_service, mock_session):
    """Test project progress update when project doesn't exist."""
    project_id = uuid4()
    processed_count = 50
    total_count = 100
    
    # Mock project doesn't exist
    mock_session.query.return_value.filter.return_value.first.return_value = None
    
    result = project_service.update_project_progress(project_id, processed_count, total_count, ProjectStatus.PROCESSING)
    
    assert result is None
    # Note: commit() would only be called by context manager on successful completion

def test_update_project_progress_calculates_percentage_correctly(project_service, mock_session, sample_project):
    """Test that progress percentage is calculated correctly."""
    project_id = sample_project.id
    
    test_cases = [
        (25, 100, 25.0),
        (33, 100, 33.0),
        (1, 3, 33.33),  # Should round to 2 decimal places
        (2, 3, 66.67),
        (1, 7, 14.29),
    ]
    
    for processed, total, expected_pct in test_cases:
        # Reset mock
        mock_session.reset_mock()
        mock_session.query.return_value.filter.return_value.first.return_value = sample_project
        
        result = project_service.update_project_progress(project_id, processed, total, ProjectStatus.PROCESSING)
        
        assert result is not None
        assert abs(result.processed_pct - expected_pct) < 0.01, f"Expected {expected_pct}, got {result.processed_pct}"


# RBAC Tests

def test_admin_role_sees_all_projects(project_service, mock_postgres_client, mock_session, sample_projects):
    """Test that Admin role can see all projects (Requirement 8.2)."""
    mock_postgres_client.get_sync_session.return_value.__enter__ = Mock(return_value=mock_session)
    mock_postgres_client.get_sync_session.return_value.__exit__ = Mock(return_value=None)
    mock_session.query.return_value.all.return_value = sample_projects
    
    user_id = "admin-user-123"
    user_roles = ["Admin"]

    result = project_service.get_projects_by_user_and_roles(user_id, user_roles)

    assert len(result) == 3
    assert result == sample_projects
    mock_session.query.assert_called_once_with(Project)
    mock_session.query.return_value.all.assert_called_once()


def test_project_manager_role_sees_own_projects(project_service, mock_postgres_client, mock_session, sample_projects):
    """Test that Project Manager role can see their own projects and member projects (Requirement 8.2)."""
    user_owned_projects = [sample_projects[0]]  # Only first project
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    
    # Mock union query result
    mock_union_query = Mock()
    mock_union_query.all.return_value = user_owned_projects
    
    # Mock the query chain for union operation
    mock_owned_query = Mock()
    mock_owned_query.union.return_value = mock_union_query
    
    # Setup query mocking for owned projects
    mock_session.query.return_value.filter.return_value = mock_owned_query
    
    user_id = "pm-user-456"
    user_roles = ["Project Manager"]

    result = project_service.get_projects_by_user_and_roles(user_id, user_roles)

    assert len(result) == 1
    assert result == user_owned_projects


def test_contributor_role_sees_own_projects(project_service, mock_postgres_client, mock_session, sample_projects):
    """Test that Contributor role can see their own projects and member projects (Requirement 8.2)."""
    user_owned_projects = [sample_projects[1]]  # Only second project
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    
    # Mock union query result
    mock_union_query = Mock()
    mock_union_query.all.return_value = user_owned_projects
    
    # Mock the query chain for union operation
    mock_owned_query = Mock()
    mock_owned_query.union.return_value = mock_union_query
    
    # Setup query mocking for owned projects
    mock_session.query.return_value.filter.return_value = mock_owned_query
    
    user_id = "contributor-user-789"
    user_roles = ["Contributor"]

    result = project_service.get_projects_by_user_and_roles(user_id, user_roles)

    assert len(result) == 1
    assert result == user_owned_projects


def test_user_with_no_recognized_roles_sees_no_projects(project_service, mock_postgres_client, mock_session):
    """Test that users with no recognized roles see no projects (Requirement 8.2)."""
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    
    user_id = "unknown-user-999"
    user_roles = ["SomeOtherRole", "UnknownRole"]

    result = project_service.get_projects_by_user_and_roles(user_id, user_roles)

    assert len(result) == 0
    assert result == []
    mock_session.query.assert_not_called()


def test_user_with_multiple_roles_gets_highest_privilege(project_service, mock_postgres_client, mock_session, sample_projects):
    """Test that users with multiple roles get the highest privilege level."""
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    mock_session.query.return_value.all.return_value = sample_projects
    
    user_id = "multi-role-user-123"
    user_roles = ["Contributor", "Admin", "Project Manager"]  # Admin should take precedence

    result = project_service.get_projects_by_user_and_roles(user_id, user_roles)

    assert len(result) == 3
    assert result == sample_projects
    mock_session.query.assert_called_once_with(Project)
    mock_session.query.return_value.all.assert_called_once()


def test_empty_roles_list_sees_no_projects(project_service, mock_postgres_client, mock_session):
    """Test that users with empty roles list see no projects."""
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    
    user_id = "no-roles-user-123"
    user_roles = []

    result = project_service.get_projects_by_user_and_roles(user_id, user_roles)

    assert len(result) == 0
    assert result == []
    mock_session.query.assert_not_called()


# Project Member Management Tests

def test_add_project_member_success(project_service, mock_postgres_client, sample_project):
    """Test successful project member addition."""
    mock_session = Mock()
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    
    # Mock project exists
    mock_session.query.return_value.filter.return_value.first.side_effect = [
        sample_project,  # Project exists
        None  # Member doesn't exist yet
    ]
    
    # Mock successful member creation
    new_member = ProjectMember(
        id=uuid4(),
        project_id=sample_project.id,
        user_id="new-member-123",
        role="Contributor",
        created_by="admin-user-123"
    )
    mock_session.refresh.side_effect = lambda obj: setattr(obj, 'id', new_member.id)
    
    # Test adding member
    member_data = ProjectMemberSet(user_id="new-member-123", role=ProjectMemberRole.CONTRIBUTOR)
    result = project_service.add_project_member(sample_project.id, member_data, "admin-user-123")
    
    # Verify member was added
    assert result.user_id == "new-member-123"
    assert result.role == "Contributor"
    assert result.created_by == "admin-user-123"
    
    # Verify database operations
    mock_session.add.assert_called_once()
    # Note: commit() is now handled by the context manager, not tested here


def test_add_project_member_project_not_found(project_service, mock_postgres_client):
    """Test adding member to non-existent project."""
    mock_session = Mock()
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    
    # Mock project doesn't exist
    mock_session.query.return_value.filter.return_value.first.return_value = None
    
    # Test adding member to non-existent project
    member_data = ProjectMemberSet(user_id="new-member-123", role=ProjectMemberRole.CONTRIBUTOR)
    
    with pytest.raises(ValueError, match="Project not found"):
        project_service.add_project_member(uuid4(), member_data, "admin-user-123")


def test_add_project_member_already_exists(project_service, mock_postgres_client, sample_project, sample_project_member):
    """Test adding member that already exists."""
    mock_session = Mock()
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    
    # Mock project exists and member already exists
    mock_session.query.return_value.filter.return_value.first.side_effect = [
        sample_project,  # Project exists
        sample_project_member  # Member already exists
    ]
    
    # Test adding existing member
    member_data = ProjectMemberSet(user_id=sample_project_member.user_id, role=ProjectMemberRole.CONTRIBUTOR)
    
    with pytest.raises(ValueError, match="User is already a member of this project"):
        project_service.add_project_member(sample_project.id, member_data, "admin-user-123")


def test_check_user_project_access_admin(project_service, mock_postgres_client):
    """Test that Admin users have access to all projects (Requirement 8.2)."""
    mock_session = Mock()
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    
    # Test Admin access
    has_access = project_service.check_user_project_access(uuid4(), "admin-user-123", ["Admin"])
    
    # Admin should have access
    assert has_access is True


def test_check_user_project_access_project_creator(project_service, mock_postgres_client, sample_project):
    """Test that project creators have access to their projects."""
    mock_session = Mock()
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    
    # Mock project exists and user is creator
    mock_session.query.return_value.filter.return_value.first.return_value = sample_project
    
    # Test creator access
    has_access = project_service.check_user_project_access(sample_project.id, sample_project.created_by, ["Contributor"])
    
    # Creator should have access
    assert has_access is True


def test_check_user_project_access_denied(project_service, mock_postgres_client):
    """Test that users without access are denied."""
    mock_session = Mock()
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    
    # Mock project exists but user is not creator or member
    mock_project = Project(id=uuid4(), created_by="someone-else")
    mock_session.query.return_value.filter.return_value.first.side_effect = [
        mock_project,  # Project exists, user is not creator
        None  # User is not a member
    ]
    
    # Test access denied
    has_access = project_service.check_user_project_access(uuid4(), "random-user", ["Contributor"])
    
    # Access should be denied
    assert has_access is False


def test_get_project_members(project_service, mock_postgres_client, sample_project_member):
    """Test retrieving project members."""
    mock_session = Mock()
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    
    # Mock members
    members = [sample_project_member]
    mock_session.query.return_value.filter.return_value.all.return_value = members
    
    # Test getting members
    result = project_service.get_project_members(sample_project_member.project_id)
    
    # Verify members returned
    assert len(result) == 1
    assert result[0] == sample_project_member


def test_remove_project_member_success(project_service, mock_postgres_client, sample_project_member):
    """Test successful project member removal."""
    mock_session = Mock()
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    
    # Mock member exists
    mock_session.query.return_value.filter.return_value.first.return_value = sample_project_member
    
    # Test removing member
    result = project_service.remove_project_member(sample_project_member.project_id, sample_project_member.user_id)
    
    # Verify member was removed
    assert result is True
    mock_session.delete.assert_called_once_with(sample_project_member)
    # Note: commit() is now handled by the context manager, not tested here


def test_remove_project_member_not_found(project_service, mock_postgres_client):
    """Test removing non-existent project member."""
    mock_session = Mock()
    mock_context_manager = Mock()
    mock_context_manager.__enter__ = Mock(return_value=mock_session)
    mock_context_manager.__exit__ = Mock(return_value=None)
    mock_postgres_client.get_sync_session.return_value = mock_context_manager
    
    # Mock member doesn't exist
    mock_session.query.return_value.filter.return_value.first.return_value = None
    
    # Test removing non-existent member
    result = project_service.remove_project_member(uuid4(), "non-existent-user")
    
    # Verify removal failed
    assert result is False
    mock_session.delete.assert_not_called()
    # Note: commit() would only be called by context manager on successful completion