"""Project management service layer."""

from typing import List, Optional
from uuid import UUID
import structlog
from tenacity import (
    retry,
    stop_after_attempt,
    wait_exponential,
    retry_if_exception_type,
)
from sqlalchemy.exc import OperationalError

from models.project_db import Project, ProjectMember
from utils.postgres_client import PostgresClient
from models.project_rest import ProjectSet, ProjectMemberSet, ProjectStatus
from services.gitlab_client_adapter import GitLabClientAdapter

logger = structlog.get_logger(__name__)


class ProjectService:
    """Business logic and DB interactions for projects."""

    def __init__(
        self,
        postgres_client: PostgresClient,
        gitlab_client_adapter: GitLabClientAdapter
    ):
        self.postgres_client = postgres_client
        self.gitlab_client_adapter = gitlab_client_adapter

    async def create_project(
        self,
        project_data: ProjectSet,
        user_id: str,
        s2s_token: Optional[str] = None
    ) -> Project:
        logger.info("Creating new project", project_name=project_data.name, user_id=user_id)

        # Resolve GitLab project ID if path is provided
        gitlab_project_id = None
        if project_data.gitlab_path and s2s_token:
            try:
                gitlab_project_id = await self.gitlab_client_adapter.resolve_project_id(
                    project_data.gitlab_path,
                    s2s_token
                )
                if gitlab_project_id:
                    logger.info(
                        "Resolved GitLab project ID",
                        gitlab_path=project_data.gitlab_path,
                        gitlab_project_id=gitlab_project_id
                    )
                else:
                    logger.warning(
                        "Failed to resolve GitLab project ID",
                        gitlab_path=project_data.gitlab_path
                    )
            except Exception as e:
                logger.error(
                    "Error resolving GitLab project ID",
                    gitlab_path=project_data.gitlab_path,
                    error=str(e)
                )

        with self.postgres_client.get_sync_session() as session:
            try:
                project = Project(
                    name=project_data.name,
                    description=project_data.description,
                    gitlab_path=project_data.gitlab_path,
                    gitlab_project_id=gitlab_project_id,
                    gitlab_repository_url=str(project_data.gitlab_repository_url) if project_data.gitlab_repository_url else None,
                    status=ProjectStatus.ACTIVE.value,
                    created_by=user_id,
                )
            except Exception as e:
                logger.error("Invalid project data", error=str(e), user_id=user_id)
                raise ValueError(f"Invalid project data: {str(e)}") from e

            # Save to database
            session.add(project)
            session.flush()  # Flush to get the generated ID before commit
            session.refresh(project)

            logger.info("Project created", project_id=str(project.id), project_name=project.name)
            return project

    def get_project_by_id(self, project_id: UUID) -> Optional[Project]:
        logger.info("Retrieving project by ID", project_id=str(project_id))

        with self.postgres_client.get_sync_session() as session:
            project = session.query(Project).filter(Project.id == project_id).first()

            if not project:
                logger.warning("Project not found", project_id=str(project_id))

            return project

    # Removed get_projects_by_user as redundant with RBAC-aware method

    def get_projects_by_user_and_roles(self, user_id: str, user_roles: List[str]) -> List[Project]:
        """RBAC-aware projects listing."""
        logger.info("Retrieving projects for user with roles", user_id=user_id, roles=user_roles)

        with self.postgres_client.get_sync_session() as session:
            if "Admin" in user_roles:
                projects = session.query(Project).all()
                logger.info("Admin user - returning all projects", user_id=user_id, project_count=len(projects))
            elif "Project Manager" in user_roles or "Contributor" in user_roles:
                owned_projects = session.query(Project).filter(Project.created_by == user_id)
                member_projects = session.query(Project).join(ProjectMember).filter(
                    ProjectMember.user_id == user_id
                )
                projects = owned_projects.union(member_projects).all()
                logger.info("Non-admin - returning owned and member projects", user_id=user_id, project_count=len(projects))
            else:
                projects = []
                logger.warning("User has no recognized roles - no project access", user_id=user_id, roles=user_roles)

            return projects

    async def update_project(
        self,
        project_id: UUID,
        update_data: ProjectSet,
        s2s_token: Optional[str] = None
    ) -> Optional[Project]:
        logger.info("Updating project", project_id=str(project_id))

        # First, check if we need to resolve GitLab project ID (outside DB session)
        update_dict = update_data.model_dump(exclude_unset=True)
        resolved_gitlab_project_id = None
        needs_gitlab_resolution = False
        
        if 'gitlab_path' in update_dict:
            # Check current path to determine if resolution is needed
            with self.postgres_client.get_sync_session() as session:
                project = session.query(Project).filter(Project.id == project_id).first()
                if not project:
                    logger.warning("Project not found for update", project_id=str(project_id))
                    return None
                
                new_path = update_dict['gitlab_path']
                if new_path != project.gitlab_path:
                    needs_gitlab_resolution = True
            
            # Resolve GitLab project ID outside of DB session
            if needs_gitlab_resolution and new_path and s2s_token:
                try:
                    resolved_gitlab_project_id = await self.gitlab_client_adapter.resolve_project_id(
                        new_path,
                        s2s_token
                    )
                    if resolved_gitlab_project_id:
                        logger.info(
                            "Re-resolved GitLab project ID",
                            gitlab_path=new_path,
                            gitlab_project_id=resolved_gitlab_project_id
                        )
                    else:
                        logger.warning(
                            "Failed to resolve new GitLab project ID",
                            gitlab_path=new_path
                        )
                except Exception as e:
                    logger.error(
                        "Error resolving GitLab project ID",
                        gitlab_path=new_path,
                        error=str(e)
                    )

        # Now perform the actual update within DB session
        with self.postgres_client.get_sync_session() as session:
            project = session.query(Project).filter(Project.id == project_id).first()

            if not project:
                logger.warning("Project not found for update", project_id=str(project_id))
                return None

            try:
                # Apply GitLab project ID if it was resolved
                if needs_gitlab_resolution:
                    if 'gitlab_path' in update_dict:
                        new_path = update_dict['gitlab_path']
                        if new_path and new_path != project.gitlab_path:
                            project.gitlab_project_id = resolved_gitlab_project_id
                        elif not new_path:
                            # Clear project ID if path is being removed
                            project.gitlab_project_id = None
                
                for field, value in update_dict.items():
                    if field == 'gitlab_repository_url' and value is not None:
                        setattr(project, field, str(value))
                    elif field == 'status':
                        setattr(project, field, value if isinstance(value, str) else value.value)
                    else:
                        setattr(project, field, value)
            except Exception as e:
                logger.error("Invalid update data", error=str(e), project_id=str(project_id))
                raise ValueError(f"Invalid update data: {str(e)}") from e

            session.flush()  # Flush pending changes to database before refresh
            session.refresh(project)

            logger.info("Project updated", project_id=str(project_id), project_name=project.name)
            return project

    def delete_project(self, project_id: UUID) -> bool:
        logger.info("Deleting project", project_id=str(project_id))

        with self.postgres_client.get_sync_session() as session:
            project = session.query(Project).filter(Project.id == project_id).first()

            if not project:
                logger.warning("Project not found for deletion", project_id=str(project_id))
                return False

            session.delete(project)

            logger.info("Project deleted", project_id=str(project_id))
            return True

    def add_project_member(self, project_id: UUID, member_data: ProjectMemberSet, added_by: str) -> ProjectMember:
        logger.info("Adding member to project", project_id=str(project_id), user_id=member_data.user_id, role=member_data.role.value, added_by=added_by)

        with self.postgres_client.get_sync_session() as session:
            project = session.query(Project).filter(Project.id == project_id).first()
            if not project:
                logger.warning("Project not found for member addition", project_id=str(project_id))
                raise ValueError("Project not found")

            existing_member = session.query(ProjectMember).filter(
                ProjectMember.project_id == project_id,
                ProjectMember.user_id == member_data.user_id
            ).first()
            
            if existing_member:
                logger.warning("User is already a member of this project", project_id=str(project_id), user_id=member_data.user_id)
                raise ValueError("User is already a member of this project")

            project_member = ProjectMember(
                project_id=project_id,
                user_id=member_data.user_id,
                role=member_data.role.value,
                created_by=added_by
            )

            session.add(project_member)
            session.flush()  # Flush to get the generated ID before commit
            session.refresh(project_member)

            logger.info("Project member added", project_id=str(project_id), user_id=member_data.user_id, member_id=str(project_member.id))
            return project_member

    def get_project_members(self, project_id: UUID) -> List[ProjectMember]:
        logger.info("Retrieving project members", project_id=str(project_id))

        with self.postgres_client.get_sync_session() as session:
            members = session.query(ProjectMember).filter(ProjectMember.project_id == project_id).all()

            logger.info("Project members retrieved", project_id=str(project_id), member_count=len(members))
            return members

    def remove_project_member(self, project_id: UUID, member_id: str) -> bool:
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

            logger.info("Project member removed", project_id=str(project_id), member_id=member_id)
            return True

    def check_user_project_access(self, project_id: UUID, user_id: str, user_roles: List[str]) -> bool:
        logger.info("Checking user project access", project_id=str(project_id), user_id=user_id, roles=user_roles)

        with self.postgres_client.get_sync_session() as session:
            if "Admin" in user_roles:
                logger.info("Admin user - access granted", project_id=str(project_id), user_id=user_id)
                return True

            project = session.query(Project).filter(Project.id == project_id).first()
            if project and project.created_by == user_id:
                logger.info("Project creator - access granted", project_id=str(project_id), user_id=user_id)
                return True

            member = session.query(ProjectMember).filter(
                ProjectMember.project_id == project_id,
                ProjectMember.user_id == user_id
            ).first()

            if member:
                logger.info("Project member - access granted", project_id=str(project_id), user_id=user_id, role=member.role)
                return True

            logger.warning("User access denied", project_id=str(project_id), user_id=user_id)
            return False

    @retry(
        retry=retry_if_exception_type((OperationalError,)),
        stop=stop_after_attempt(3),
        wait=wait_exponential(multiplier=0.1, max=2),
        reraise=True,
    )
    def update_project_progress(self, project_id: UUID, processed_count: Optional[int], total_count: Optional[int], status: ProjectStatus, processed_pct: Optional[float] = None) -> Optional[Project]:
        logger.info(
            "Updating project progress",
            project_id=str(project_id),
            processed_count=processed_count,
            total_count=total_count,
            status=status
        )

        with self.postgres_client.get_sync_session() as session:
            project = session.query(Project).filter(
                Project.id == project_id
            ).with_for_update().first()

            if not project:
                logger.warning(
                    "Project not found for progress update", 
                    project_id=str(project_id),
                )
                return None

            project.status = status.value

            # Prefer directly provided processed_pct when available; otherwise derive from counts.
            if processed_pct is not None:
                project.processed_pct = max(0.0, min(100.0, float(processed_pct)))
            elif processed_count is not None and total_count is not None:
                project.processed_pct = round((processed_count / total_count) * 100, 2)
            elif status == ProjectStatus.RAG_READY:
                project.processed_pct = 100.0

            # Ensure pending changes are visible to subsequent refresh and publishers
            # Flush before refresh so the in-memory object reflects the latest DB values.
            session.flush()
            session.refresh(project)

            logger.info("Project progress updated", project_id=str(project_id), new_status=project.status, processed_pct=project.processed_pct)
            return project
