"""Project management API router."""

from typing import List
from uuid import UUID
from fastapi import APIRouter, Depends, HTTPException, status, Request, UploadFile, File
from fastapi_azure_auth.user import User
import structlog

from middleware.azure_auth_middleware import get_current_active_user, require_roles
from models.project_rest import (
    ProjectSet,
    ProjectResponse,
    ProjectMemberSet,
    ProjectMemberResponse,
    ProjectProgressUpdateRequest
)
from models.document_schemas import BulkUploadResponse
from utils.redis_client import get_redis_client
from services.project_service import ProjectService
from services.document_upload_service import DocumentUploadService
from services.project_status_publisher import ProjectStatusPublisher

logger = structlog.get_logger(__name__)

router = APIRouter(prefix="/projects", tags=["Projects"])

def get_project_service(request: Request) -> ProjectService:
    return ProjectService(request.app.state.postgres_client)

def get_document_upload_service(request: Request) -> DocumentUploadService:
    return DocumentUploadService(request.app.state.blob_storage_client)

def get_project_status_publisher() -> ProjectStatusPublisher:
    return ProjectStatusPublisher(get_redis_client())


@router.post(
    "", response_model=ProjectResponse, status_code=status.HTTP_201_CREATED
)
async def create_project(
    project: ProjectSet,
    current_user: User = Depends(require_roles(["Admin"])),
    project_service: ProjectService = Depends(get_project_service)
) -> ProjectResponse:
    """Create a project (Admin only)."""
    logger.info(
        "Creating project via API",
        project_name=project.name,
        user_id=current_user.oid
    )

    created_project = project_service.create_project(project, current_user.oid)
    return ProjectResponse.model_validate(created_project)


@router.get("", response_model=List[ProjectResponse])
async def list_projects(
    current_user: User = Depends(get_current_active_user),
    project_service: ProjectService = Depends(get_project_service)
) -> List[ProjectResponse]:
    """List projects available to current user (RBAC)."""
    logger.info("Listing projects via API", user_id=current_user.oid)

    projects = project_service.get_projects_by_user_and_roles(
        current_user.oid, current_user.roles
    )
    return [ProjectResponse.model_validate(project) for project in projects]


@router.get("/{project_id}", response_model=ProjectResponse)
async def get_project(
    project_id: UUID,
    current_user: User = Depends(get_current_active_user),
    project_service: ProjectService = Depends(get_project_service)
) -> ProjectResponse:
    """Get project by ID."""
    logger.info(
        "Getting project via API",
        project_id=str(project_id),
        user_id=current_user.oid
    )

    project = project_service.get_project_by_id(project_id)
    if not project:
        logger.warning(
            "Project not found",
            project_id=str(project_id),
            user_id=current_user.oid
        )
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="Project not found"
        )

    return ProjectResponse.model_validate(project)


@router.put("/{project_id}", response_model=ProjectResponse)
async def update_project(
    project_id: UUID,
    project: ProjectSet,
    current_user: User = Depends(get_current_active_user),
    project_service: ProjectService = Depends(get_project_service)
) -> ProjectResponse:
    """Update a project."""
    logger.info(
        "Updating project via API",
        project_id=str(project_id),
        user_id=current_user.oid
    )

    updated_project = project_service.update_project(project_id, project)
    if not updated_project:
        logger.warning(
            "Project not found for update",
            project_id=str(project_id),
            user_id=current_user.oid
        )
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="Project not found"
        )

    return ProjectResponse.model_validate(updated_project)


@router.delete("/{project_id}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_project(
    project_id: UUID,
    current_user: User = Depends(require_roles(["Admin"])),
    project_service: ProjectService = Depends(get_project_service)
) -> None:
    """Delete a project (Admin only)."""
    logger.info(
        "Deleting project via API",
        project_id=str(project_id),
        user_id=current_user.oid
    )

    success = project_service.delete_project(project_id)
    if not success:
        logger.warning(
            "Project not found for deletion",
            project_id=str(project_id),
            user_id=current_user.oid
        )
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="Project not found"
        )

@router.post(
    "/{project_id}/members",
    response_model=ProjectMemberResponse,
    status_code=status.HTTP_201_CREATED
)
async def add_project_member(
    project_id: UUID,
    member_data: ProjectMemberSet,
    current_user: User = Depends(get_current_active_user),
    project_service: ProjectService = Depends(get_project_service)
) -> ProjectMemberResponse:
    """Add a member (Admin or PM for own projects)."""
    logger.info(
        "Adding project member via API",
        project_id=str(project_id),
        user_id=member_data.user_id,
        role=member_data.role.value,
        added_by=current_user.oid
    )

    # Admin can add to any project; PM only to own
    if "Admin" not in current_user.roles:
        if "Project Manager" not in current_user.roles:
            logger.warning(
                "Insufficient permissions to add project member",
                user_id=current_user.oid,
                roles=current_user.roles
            )
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="Insufficient permissions to add project members"
            )

        project = project_service.get_project_by_id(project_id)
        if not project or project.created_by != current_user.oid:
            logger.warning(
                "Project Manager can only add members to own projects",
                user_id=current_user.oid,
                project_id=str(project_id)
            )
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="Project Managers can only add members to their own projects"
            )

    try:
        project_member = project_service.add_project_member(
            project_id, member_data, current_user.oid
        )
        return ProjectMemberResponse.model_validate(project_member)
    except ValueError as e:
        logger.error(
            "Failed to add project member",
            error=str(e),
            project_id=str(project_id),
            user_id=current_user.oid
        )
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=str(e)
        ) from e


@router.get("/{project_id}/members", response_model=List[ProjectMemberResponse])
async def list_project_members(
    project_id: UUID,
    current_user: User = Depends(get_current_active_user),
    project_service: ProjectService = Depends(get_project_service)
) -> List[ProjectMemberResponse]:
    """List project members (requires access)."""
    logger.info(
        "Listing project members via API",
        project_id=str(project_id),
        user_id=current_user.oid
    )

    has_access = project_service.check_user_project_access(
        project_id, current_user.oid, current_user.roles
    )
    if not has_access:
        logger.warning(
            "User does not have access to project",
            user_id=current_user.oid,
            project_id=str(project_id)
        )
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Access denied to this project"
        )

    members = project_service.get_project_members(project_id)
    return [ProjectMemberResponse.model_validate(member) for member in members]


@router.delete(
    "/{project_id}/members/{member_id}",
    status_code=status.HTTP_204_NO_CONTENT
)
async def remove_project_member(
    project_id: UUID,
    member_id: str,
    current_user: User = Depends(require_roles(["Admin"])),
    project_service: ProjectService = Depends(get_project_service)
) -> None:
    """Remove a member (Admin only)."""
    logger.info(
        "Removing project member via API",
        project_id=str(project_id),
        member_id=member_id,
        removed_by=current_user.oid
    )

    success = project_service.remove_project_member(project_id, member_id)
    if not success:
        logger.warning(
            "Project member not found for removal",
            project_id=str(project_id),
            member_id=member_id,
            user_id=current_user.oid
        )
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="Project member not found"
        )


@router.put("/{project_id}/status", response_model=ProjectResponse)
async def update_project_status(
    project_id: UUID,
    update_request: ProjectProgressUpdateRequest,
    current_user: User = Depends(get_current_active_user),
    project_service: ProjectService = Depends(get_project_service),
    project_status_publisher: ProjectStatusPublisher = Depends(get_project_status_publisher)
) -> ProjectResponse:
    """Update project status and publish progress."""
    logger.info(
        "Updating project status via API",
        project_id=str(project_id),
        project_id_type=type(project_id).__name__,
        project_id_repr=repr(project_id),
        status=update_request.status,
        user_id=current_user.oid
    )

    project = project_service.update_project_progress(
        project_id,
        update_request.processed_count,
        update_request.total_count,
        status=update_request.status
    )

    if not project:
        logger.warning(
            "Project not found for status update",
            project_id=str(project_id),
            user_id=current_user.oid
        )
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="Project not found"
        )

    # Publish project update to Redis (non-blocking)
    try:
        publish_success = await project_status_publisher.publish_project_update(
            project_id=project_id,
            status=project.status,
            processed_count=update_request.processed_count,
            total_count=update_request.total_count,
            processed_pct=project.processed_pct,
            process_step=update_request.process_step,
        )

        if not publish_success:
            logger.warning(
                "Publish to Redis failed; API update succeeded",
                project_id=str(project_id),
                status=project.status
            )

    except Exception as e:
        logger.error(
            "Redis publishing failed, but API operation succeeded",
            project_id=str(project_id),
            error=str(e),
            exc_info=True
        )

    return ProjectResponse.model_validate(project)


# Document Upload Endpoint

@router.post("/{project_id}/documents/upload", response_model=BulkUploadResponse)
async def bulk_upload_documents(
    project_id: UUID,
    files: List[UploadFile] = File(...),
    current_user: User = Depends(get_current_active_user),
    document_upload_service: DocumentUploadService = Depends(get_document_upload_service),
    project_service: ProjectService = Depends(get_project_service)
) -> BulkUploadResponse:
    """
    Upload multiple documents for a project.
    
    Args:
        project_id: Project ID to associate the documents with
        files: List of uploaded files
        current_user: Current authenticated user
        document_upload_service: Document upload service instance
        project_service: Project service instance
        
    Returns:
        BulkUploadResponse: Upload response with processing status
    """
    logger.info("Bulk document upload requested",
                project_id=str(project_id),
                file_count=len(files),
                user_id=current_user.oid)

    # Check if project exists before processing upload
    project = project_service.get_project_by_id(project_id)
    if not project:
        logger.warning(
            "Project not found for document upload",
            project_id=str(project_id),
            user_id=current_user.oid
        )
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="Project not found"
        )

    try:
        result = await document_upload_service.bulk_upload_documents(project_id, files)
        logger.info("Bulk document upload successful",
                   project_id=str(project_id),
                   file_count=len(files),
                   user_id=current_user.oid)
        return result
    except Exception as e:  # Let tests assert specific error message
        logger.error("Bulk document upload failed",
                    project_id=str(project_id),
                    file_count=len(files),
                    user_id=current_user.oid,
                    error=str(e))
        raise HTTPException(status_code=500, detail=f"Bulk document upload failed: {str(e)}") from e
