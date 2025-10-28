"""GitLab client wrapper for normalized API access."""

from typing import List, Any, Optional
import structlog
import gitlab
from gitlab.v4.objects import Group, Project, ProjectIssue, GroupEpic
from gitlab.exceptions import GitlabGetError, GitlabAuthenticationError

from models import GitLabWorkItem, Pagination, ListResponse
from config import GitLabClientSettings

logger = structlog.get_logger(__name__)


class GitLabClientService:
    """Service for interacting with GitLab API with normalized responses."""
    
    def __init__(self, client: gitlab.Gitlab, settings: GitLabClientSettings):
        """
        Initialize GitLab client service.
        
        Args:
            client: Configured python-gitlab client
            settings: GitLab client settings
        """
        self.client = client
        self.settings = settings
    
    def _normalize_epic(self, epic: GroupEpic) -> GitLabWorkItem:
        """
        Normalize GitLab epic to GitLabWorkItem.
        
        Args:
            epic: GitLab epic object
            
        Returns:
            Normalized work item
        """
        return GitLabWorkItem(
            kind="epic",
            id=str(epic.id),
            title=epic.title,
            description=epic.description or "",
            state=epic.state,
            labels=epic.labels if hasattr(epic, 'labels') else [],
            web_url=epic.web_url,
            title_embedding=[]
        )
    
    def _normalize_issue(self, issue: ProjectIssue) -> GitLabWorkItem:
        """
        Normalize GitLab issue to GitLabWorkItem.
        
        Args:
            issue: GitLab issue object
            
        Returns:
            Normalized work item
        """
        return GitLabWorkItem(
            kind="issue",
            id=str(issue.id),
            title=issue.title,
            description=issue.description or "",
            state=issue.state,
            labels=issue.labels if hasattr(issue, 'labels') else [],
            web_url=issue.web_url,
            title_embedding=[]
        )
    
    def _extract_pagination(
        self,
        gitlab_list: Any,
        current_page: int
    ) -> Pagination:
        """
        Extract pagination metadata from GitLab list response.
        
        Args:
            gitlab_list: GitLab list object with pagination
            current_page: Current page number
            
        Returns:
            Pagination metadata
        """
        total_pages = getattr(gitlab_list, 'total_pages', None)
        next_page = getattr(gitlab_list, 'next_page', None)
        prev_page = getattr(gitlab_list, 'prev_page', None)
        total = getattr(gitlab_list, 'total', None)
        per_page = getattr(gitlab_list, 'per_page', self.settings.DEFAULT_PAGE_SIZE)
        
        return Pagination(
            page=current_page,
            per_page=per_page,
            next_page=next_page,
            prev_page=prev_page,
            total=total
        )
    
    def _get_group_for_project(self, project_id: str) -> Optional[Group]:
        """
        Get the group that owns a project (for epic access).
        
        Args:
            project_id: GitLab project ID
            
        Returns:
            Group object or None if project has no group
        """
        try:
            project: Project = self.client.projects.get(project_id)
            namespace = project.namespace
            
            # Check if namespace is a group (not a user namespace)
            if namespace.get('kind') == 'group':
                group_id = namespace.get('id')
                return self.client.groups.get(group_id)
            
            return None
        except Exception as e:
            logger.warning(
                "Could not resolve group for project",
                project_id=project_id,
                error=str(e)
            )
            return None
    
    def list_project_backlog(
        self,
        project_id: str,
        labels: Optional[List[str]] = None,
        state: str = "opened",
        page: int = 1,
        per_page: Optional[int] = None
    ) -> ListResponse:
        """
        List all epics and issues for a project (combined backlog).
        
        Args:
            project_id: GitLab project ID
            labels: Filter by labels
            state: Filter by state ('opened', 'closed', 'all')
            page: Page number
            per_page: Items per page
            
        Returns:
            List response with epics and issues
        """
        per_page = per_page or self.settings.DEFAULT_PAGE_SIZE
        items: List[GitLabWorkItem] = []
        
        # Get epics from group
        group = self._get_group_for_project(project_id)
        if group:
            try:
                epic_params = {
                    'state': state if state != 'all' else None,
                    'page': page,
                    'per_page': per_page
                }
                if labels:
                    epic_params['labels'] = ','.join(labels)
                
                epics = group.epics.list(**{k: v for k, v in epic_params.items() if v is not None})
                items.extend([self._normalize_epic(e) for e in epics])
                
                logger.debug(
                    "Retrieved epics from group",
                    group_id=group.id,
                    count=len(epics)
                )
            except Exception as e:
                logger.warning(
                    "Could not retrieve epics",
                    group_id=group.id,
                    error=str(e)
                )
        
        # Get issues from project
        try:
            project: Project = self.client.projects.get(project_id)
            issue_params = {
                'state': state if state != 'all' else None,
                'page': page,
                'per_page': per_page
            }
            if labels:
                issue_params['labels'] = labels
            
            issues = project.issues.list(**{k: v for k, v in issue_params.items() if v is not None})
            items.extend([self._normalize_issue(i) for i in issues])
            
            logger.debug(
                "Retrieved issues from project",
                project_id=project_id,
                count=len(issues)
            )
            
            # Extract pagination from issues list (use as overall pagination)
            pagination = self._extract_pagination(issues, page)
            
        except Exception as e:
            logger.error(
                "Could not retrieve issues",
                project_id=project_id,
                error=str(e)
            )
            # Return empty list with basic pagination
            pagination = Pagination(
                page=page,
                per_page=per_page,
                next_page=None,
                prev_page=None,
                total=None
            )
        
        return ListResponse(items=items, pagination=pagination)
    
    def create_epic(
        self,
        group_id: str,
        title: str,
        description: str = "",
        labels: Optional[List[str]] = None
    ) -> GitLabWorkItem:
        """
        Create a new epic in a group.
        
        Args:
            group_id: GitLab group ID
            title: Epic title
            description: Epic description
            labels: Epic labels
            
        Returns:
            Created epic as normalized work item
        """
        group: Group = self.client.groups.get(group_id)
        
        epic_data = {
            'title': title,
            'description': description
        }
        if labels:
            epic_data['labels'] = ','.join(labels)
        
        epic = group.epics.create(epic_data)
        
        logger.info(
            "Epic created",
            group_id=group_id,
            epic_id=epic.id,
            title=title
        )
        
        return self._normalize_epic(epic)
    
    def create_issue(
        self,
        project_id: str,
        title: str,
        description: str = "",
        labels: Optional[List[str]] = None
    ) -> GitLabWorkItem:
        """
        Create a new issue in a project.
        
        Args:
            project_id: GitLab project ID
            title: Issue title
            description: Issue description
            labels: Issue labels
            
        Returns:
            Created issue as normalized work item
        """
        project: Project = self.client.projects.get(project_id)
        
        issue_data = {
            'title': title,
            'description': description
        }
        if labels:
            issue_data['labels'] = ','.join(labels)
        
        issue = project.issues.create(issue_data)
        
        logger.info(
            "Issue created",
            project_id=project_id,
            issue_id=issue.id,
            title=title
        )
        
        return self._normalize_issue(issue)
    
    def update_epic(
        self,
        group_id: str,
        epic_id: str,
        title: Optional[str] = None,
        description: Optional[str] = None,
        labels: Optional[List[str]] = None
    ) -> GitLabWorkItem:
        """
        Update an existing epic (only changed fields).
        
        Args:
            group_id: GitLab group ID
            epic_id: Epic ID
            title: New title (if changed)
            description: New description (if changed)
            labels: New labels (if changed)
            
        Returns:
            Updated epic as normalized work item
        """
        group: Group = self.client.groups.get(group_id)
        epic = group.epics.get(epic_id)
        
        updates = {}
        if title is not None and title != epic.title:
            updates['title'] = title
        if description is not None and description != (epic.description or ""):
            updates['description'] = description
        if labels is not None:
            current_labels = set(epic.labels if hasattr(epic, 'labels') else [])
            new_labels = set(labels)
            if current_labels != new_labels:
                updates['labels'] = ','.join(labels)
        
        if updates:
            for key, value in updates.items():
                setattr(epic, key, value)
            epic.save()
            
            logger.info(
                "Epic updated",
                group_id=group_id,
                epic_id=epic_id,
                changes=list(updates.keys())
            )
        else:
            logger.debug(
                "Epic unchanged (no diff)",
                group_id=group_id,
                epic_id=epic_id
            )
        
        return self._normalize_epic(epic)
    
    def update_issue(
        self,
        project_id: str,
        issue_id: str,
        title: Optional[str] = None,
        description: Optional[str] = None,
        labels: Optional[List[str]] = None
    ) -> GitLabWorkItem:
        """
        Update an existing issue (only changed fields).
        
        Args:
            project_id: GitLab project ID
            issue_id: Issue ID
            title: New title (if changed)
            description: New description (if changed)
            labels: New labels (if changed)
            
        Returns:
            Updated issue as normalized work item
        """
        project: Project = self.client.projects.get(project_id)
        issue = project.issues.get(issue_id)
        
        updates = {}
        if title is not None and title != issue.title:
            updates['title'] = title
        if description is not None and description != (issue.description or ""):
            updates['description'] = description
        if labels is not None:
            current_labels = set(issue.labels if hasattr(issue, 'labels') else [])
            new_labels = set(labels)
            if current_labels != new_labels:
                updates['labels'] = labels
        
        if updates:
            for key, value in updates.items():
                setattr(issue, key, value)
            issue.save()
            
            logger.info(
                "Issue updated",
                project_id=project_id,
                issue_id=issue_id,
                changes=list(updates.keys())
            )
        else:
            logger.debug(
                "Issue unchanged (no diff)",
                project_id=project_id,
                issue_id=issue_id
            )
        
        return self._normalize_issue(issue)

    def resolve_project(self, gitlab_path: str) -> dict[str, Any]:
        """
        Resolve GitLab project path to project details.
        
        Args:
            gitlab_path: Project path in namespace/project format
            
        Returns:
            Dict with project_id, path, name, web_url
            
        Raises:
            ValueError: If path is empty
            GitlabGetError: If project not found
            GitlabAuthenticationError: If token invalid
        """
        
        if not gitlab_path or not gitlab_path.strip():
            raise ValueError("GitLab path is required")
        
        gitlab_path = gitlab_path.strip()
        
        logger.info("Resolving GitLab project", gitlab_path=gitlab_path)
        
        try:
            # Get project using python-gitlab (handles URL encoding)
            project = self.client.projects.get(gitlab_path)
            
            result = {
                "project_id": str(project.id),
                "path": project.path_with_namespace,
                "name": project.name,
                "web_url": project.web_url
            }
            
            logger.info("Project resolved", **result)
            return result
            
        except GitlabAuthenticationError as e:
            logger.error("GitLab authentication failed", gitlab_path=gitlab_path, error=str(e))
            raise
        except GitlabGetError as e:
            if e.response_code == 404:
                logger.warning("GitLab project not found", gitlab_path=gitlab_path)
            else:
                logger.error("GitLab API error", gitlab_path=gitlab_path, status=e.response_code, error=str(e))
            raise


