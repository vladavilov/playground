"""
Business logic service for project management operations.
"""

from typing import List, Optional
from uuid import UUID
import structlog

from models.project_db import Project, ProjectMember
from utils.postgres_client import PostgresClient
from models.project_rest import ProjectSet, ProjectMemberSet, ProjectStatus

logger = structlog.get_logger(__name__)


class ProjectService:
    """
    Service class for project management operations.
    Handles business logic and database interactions.
    """

    def __init__(self, postgres_client: PostgresClient):
        """
        Initialize the project service.

        Args:
            postgres_client: PostgreSQL client instance
        """
        self.postgres_client = postgres_client
        logger.info("ProjectService initialized")

    def create_project(self, project_data: ProjectSet, user_id: str) -> Project:
        """
        Create a new project.

        Args:
            project_data: Project creation data
            user_id: ID of the user creating the project

        Returns:
            Project: Created project instance
        """
        logger.info("Creating new project", project_name=project_data.name, user_id=user_id)

        with self.postgres_client.get_sync_session() as session:
            try:
                # Create new project instance with proper type conversion
                project = Project(
                    name=project_data.name,
                    description=project_data.description,
                    gitlab_url=str(project_data.gitlab_url) if project_data.gitlab_url else None,
                    gitlab_repository_url=str(project_data.gitlab_repository_url) if project_data.gitlab_repository_url else None,
                    status=project_data.status.value,
                    created_by=user_id
                )
            except Exception as e:
                logger.error("Failed to convert ProjectSet to Project", error=str(e), user_id=user_id)
                raise ValueError(f"Invalid project data: {str(e)}") from e

            # Save to database
            session.add(project)
            session.commit()
            session.refresh(project)

            logger.info(
                "Project created successfully", 
                project_id=str(project.id), 
                project_id_type=type(project.id).__name__,
                project_id_repr=repr(project.id),
                project_name=project.name
            )
            return project

    def get_project_by_id(self, project_id: UUID) -> Optional[Project]:
        """
        Get a project by its ID.

        Args:
            project_id: Project ID

        Returns:
            Project: Project instance or None if not found
        """
        logger.info("Retrieving project by ID", project_id=str(project_id))

        with self.postgres_client.get_sync_session() as session:
            project = session.query(Project).filter(Project.id == project_id).first()

            if project:
                logger.info("Project found", project_id=str(project_id), project_name=project.name)
            else:
                logger.warning("Project not found", project_id=str(project_id))

            return project

    def get_projects_by_user(self, user_id: str) -> List[Project]:
        """
        Get all projects accessible by a user.
        Returns all projects where user is creator.
        Note: For role-based access control, use get_projects_by_user_and_roles method.

        Args:
            user_id: User ID

        Returns:
            List[Project]: List of accessible projects
        """
        logger.info("Retrieving projects for user", user_id=user_id)

        with self.postgres_client.get_sync_session() as session:
            projects = session.query(Project).filter(Project.created_by == user_id).all()

            logger.info("Projects retrieved", user_id=user_id, project_count=len(projects))
            return projects

    def get_projects_by_user_and_roles(self, user_id: str, user_roles: List[str]) -> List[Project]:
        """
        Get all projects accessible by a user based on their Azure AD roles.
        Implements role-based access control (Requirements 6.7, 8.2).

        Args:
            user_id: User ID
            user_roles: List of user's Azure AD roles

        Returns:
            List[Project]: List of accessible projects based on roles
        """
        logger.info("Retrieving projects for user with roles", user_id=user_id, roles=user_roles)

        with self.postgres_client.get_sync_session() as session:
            # Role-based access control logic
            if "Admin" in user_roles:
                # Admins can see all projects
                projects = session.query(Project).all()
                logger.info("Admin user - returning all projects", user_id=user_id, project_count=len(projects))
            elif "Project Manager" in user_roles:
                # Project Managers can see projects they created AND projects where they are members
                owned_projects = session.query(Project).filter(Project.created_by == user_id)
                member_projects = session.query(Project).join(ProjectMember).filter(
                    ProjectMember.user_id == user_id
                )
                projects = owned_projects.union(member_projects).all()
                logger.info("Project Manager - returning owned and member projects", user_id=user_id, project_count=len(projects))
            elif "Contributor" in user_roles:
                # Contributors can see projects they created AND projects where they are members
                owned_projects = session.query(Project).filter(Project.created_by == user_id)
                member_projects = session.query(Project).join(ProjectMember).filter(
                    ProjectMember.user_id == user_id
                )
                projects = owned_projects.union(member_projects).all()
                logger.info("Contributor - returning owned and member projects", user_id=user_id, project_count=len(projects))
            else:
                # No recognized roles - no access
                projects = []
                logger.warning("User has no recognized roles - no project access", user_id=user_id, roles=user_roles)

            return projects

    def update_project(self, project_id: UUID, update_data: ProjectSet) -> Optional[Project]:
        """
        Update an existing project.

        Args:
            project_id: Project ID
            update_data: Project update data

        Returns:
            Project: Updated project instance or None if not found
        """
        logger.info("Updating project", project_id=str(project_id))

        with self.postgres_client.get_sync_session() as session:
            project = session.query(Project).filter(Project.id == project_id).first()

            if not project:
                logger.warning("Project not found for update", project_id=str(project_id))
                return None

            # Update fields that are provided with proper type conversion
            try:
                update_dict = update_data.model_dump(exclude_unset=True)
                for field, value in update_dict.items():
                    # Handle special field conversions
                    if field == 'gitlab_url' and value is not None:
                        setattr(project, field, str(value))
                    elif field == 'gitlab_repository_url' and value is not None:
                        setattr(project, field, str(value))
                    elif field == 'status':
                        # Convert enum value to string
                        setattr(project, field, value if isinstance(value, str) else value.value)
                    else:
                        setattr(project, field, value)
            except Exception as e:
                logger.error("Failed to update project fields", error=str(e), project_id=str(project_id))
                raise ValueError(f"Invalid update data: {str(e)}") from e

            session.commit()
            session.refresh(project)

            logger.info("Project updated successfully", project_id=str(project_id), project_name=project.name)
            return project

    def delete_project(self, project_id: UUID) -> bool:
        """
        Delete a project.

        Args:
            project_id: Project ID

        Returns:
            bool: True if deleted, False if not found
        """
        logger.info("Deleting project", project_id=str(project_id))

        with self.postgres_client.get_sync_session() as session:
            project = session.query(Project).filter(Project.id == project_id).first()

            if not project:
                logger.warning("Project not found for deletion", project_id=str(project_id))
                return False

            session.delete(project)
            session.commit()

            logger.info("Project deleted successfully", project_id=str(project_id))
            return True

    def add_project_member(self, project_id: UUID, member_data: ProjectMemberSet, added_by: str) -> ProjectMember:
        """
        Add a member to a project.
        Implements Requirements 6.7 and 8.2.

        Args:
            project_id: Project ID
            member_data: Project member data
            added_by: ID of the user adding the member

        Returns:
            ProjectMember: Created project member instance
        """
        logger.info("Adding member to project", project_id=str(project_id), user_id=member_data.user_id, role=member_data.role.value, added_by=added_by)

        with self.postgres_client.get_sync_session() as session:
            # Check if project exists
            project = session.query(Project).filter(Project.id == project_id).first()
            if not project:
                logger.warning("Project not found for member addition", project_id=str(project_id))
                raise ValueError("Project not found")

            # Check if member already exists
            existing_member = session.query(ProjectMember).filter(
                ProjectMember.project_id == project_id,
                ProjectMember.user_id == member_data.user_id
            ).first()
            
            if existing_member:
                logger.warning("User is already a member of this project", project_id=str(project_id), user_id=member_data.user_id)
                raise ValueError("User is already a member of this project")

            # Create new project member
            project_member = ProjectMember(
                project_id=project_id,
                user_id=member_data.user_id,
                role=member_data.role.value,
                created_by=added_by
            )

            session.add(project_member)
            session.commit()
            session.refresh(project_member)

            logger.info("Project member added successfully", project_id=str(project_id), user_id=member_data.user_id, member_id=str(project_member.id))
            return project_member

    def get_project_members(self, project_id: UUID) -> List[ProjectMember]:
        """
        Get all members of a project.
        Implements Requirements 6.7 and 8.2.

        Args:
            project_id: Project ID

        Returns:
            List[ProjectMember]: List of project members
        """
        logger.info("Retrieving project members", project_id=str(project_id))

        with self.postgres_client.get_sync_session() as session:
            members = session.query(ProjectMember).filter(ProjectMember.project_id == project_id).all()

            logger.info("Project members retrieved", project_id=str(project_id), member_count=len(members))
            return members

    def remove_project_member(self, project_id: UUID, member_id: str) -> bool:
        """
        Remove a member from a project.
        Implements Requirements 6.7 and 8.2.

        Args:
            project_id: Project ID
            member_id: Member user ID to remove

        Returns:
            bool: True if removed, False if not found
        """
        logger.info("Removing member from project", project_id=str(project_id), member_id=member_id)

        with self.postgres_client.get_sync_session() as session:
            member = session.query(ProjectMember).filter(
                ProjectMember.project_id == project_id,
                ProjectMember.user_id == member_id
            ).first()

            if not member:
                logger.warning("Project member not found for removal", project_id=str(project_id), member_id=member_id)
                return False

            session.delete(member)
            session.commit()

            logger.info("Project member removed successfully", project_id=str(project_id), member_id=member_id)
            return True

    def check_user_project_access(self, project_id: UUID, user_id: str, user_roles: List[str]) -> bool:
        """
        Check if a user has access to a project based on their roles and membership.
        Implements Requirements 6.7 and 8.2.

        Args:
            project_id: Project ID
            user_id: User ID
            user_roles: List of user's Azure AD roles

        Returns:
            bool: True if user has access, False otherwise
        """
        logger.info("Checking user project access", project_id=str(project_id), user_id=user_id, roles=user_roles)

        with self.postgres_client.get_sync_session() as session:
            # Admin users have access to all projects
            if "Admin" in user_roles:
                logger.info("Admin user - access granted", project_id=str(project_id), user_id=user_id)
                return True

            # Check if user is the project creator
            project = session.query(Project).filter(Project.id == project_id).first()
            if project and project.created_by == user_id:
                logger.info("Project creator - access granted", project_id=str(project_id), user_id=user_id)
                return True

            # Check if user is a project member
            member = session.query(ProjectMember).filter(
                ProjectMember.project_id == project_id,
                ProjectMember.user_id == user_id
            ).first()

            if member:
                logger.info("Project member - access granted", project_id=str(project_id), user_id=user_id, role=member.role)
                return True

            logger.warning("User access denied", project_id=str(project_id), user_id=user_id)
            return False

    def update_project_progress(self, project_id: UUID, processed_count: int, total_count: int, status: ProjectStatus) -> Optional[Project]:
        """
        Update project progress with processed and total document counts.

        Args:
            project_id: Project ID
            processed_count: Number of processed documents
            total_count: Total number of documents to process
            status: ProjectStatus - new status of project

        Returns:
            Project: Updated project instance or None if not found
        """
        logger.info(
            "Updating project progress",
            project_id=str(project_id),
            project_id_type=type(project_id).__name__,
            project_id_repr=repr(project_id),
            processed_count=processed_count,
            total_count=total_count,
            status=status
        )

        with self.postgres_client.get_sync_session() as session:
            # Add comprehensive debugging for the database query
            logger.info(
                "Executing project lookup query",
                project_id=str(project_id),
                project_id_type=type(project_id).__name__,
                query_filter_value=project_id,
                query_filter_repr=repr(project_id)
            )
            
            project = session.query(Project).filter(Project.id == project_id).first()

            if not project:
                logger.warning(
                    "Project not found for progress update", 
                    project_id=str(project_id),
                    project_id_type=type(project_id).__name__
                )
                return None

            project.status = status.value
            project.processed_pct = round((processed_count / total_count) * 100, 2)

            session.commit()
            session.refresh(project)

            logger.info(
                "Project progress updated successfully",
                project_id=str(project_id),
                new_status=project.status,
                processed_pct=project.processed_pct
            )
            return project
