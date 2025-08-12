"""
Core business logic for processing project documents.

This module is framework-agnostic and depends only on injected collaborators,
so it can be tested with lightweight fakes without patching Celery or asyncio.
"""

from __future__ import annotations

from typing import Any, Callable, Dict, List
from uuid import UUID
import os
import tempfile
import structlog


Logger = structlog.stdlib.BoundLogger


def process_project_documents_core(
    project_id: str,
    blob_client: Any,
    tika_processor: Any,
    send_progress_update: Callable[[str, int, int], Dict[str, Any]],
    logger: Logger | None = None,
) -> Dict[str, Any]:
    """
    Process all documents for a project from blob storage.

    Collaborators are injected to facilitate integration-style testing.

    Args:
        project_id: Project ID as string UUID
        blob_client: Object providing list_files, download_file, delete_file methods
        tika_processor: Object providing extract_text_with_result(path) -> result
        send_progress_update: Callable to send progress; returns { 'success': bool, ... }
        logger: Optional structlog logger

    Returns:
        Result dictionary matching the Celery task's contract
    """
    log = logger or structlog.get_logger(__name__)

    # Normalize/validate project_id for downstream collaborators
    try:
        project_uuid = UUID(project_id)
    except Exception as exc:  # pragma: no cover - surfaced as runtime error in calling layer
        return {
            "success": False,
            "project_id": project_id,
            "error_message": f"Invalid project_id: {exc}",
        }

    list_result = blob_client.list_files(project_id=project_uuid)
    if not getattr(list_result, "success", False):
        error_msg = f"Failed to list files for project: {getattr(list_result, 'error_message', 'unknown error')}"
        log.error("Failed to list project files", project_id=project_id, error=error_msg)
        return {
            "success": False,
            "project_id": project_id,
            "error_message": error_msg,
        }

    file_list: List[str] = getattr(list_result, "file_list", []) or []
    total_documents = len(file_list)
    processed_documents = 0
    failed_documents = 0
    documents_for_ingestion: List[Dict[str, Any]] = []

    if total_documents == 0:
        log.info(
            "No files found to process - project may have already been processed",
            project_id=project_id,
        )
        return {
            "success": True,
            "project_id": project_id,
            "total_documents": 0,
            "processed_documents": 0,
            "failed_documents": 0,
            "extracted_data_count": 0,
            "note": "No files found - possibly already processed by another task",
        }

    # Process each file
    for blob_name in file_list:
        temp_file_path = None
        try:
            with tempfile.NamedTemporaryFile(delete=False, suffix=os.path.splitext(blob_name)[1]) as temp_file:
                temp_file_path = temp_file.name

            download_result = blob_client.download_file(blob_name, temp_file_path, project_id=project_uuid)
            if not getattr(download_result, "success", False):
                failed_documents += 1
                log.error("Failed to download file", blob_name=blob_name, error=getattr(download_result, "error_message", None))
                continue

            processing_result = tika_processor.extract_text_with_result(temp_file_path)
            if getattr(processing_result, "success", False):
                # Map to ingestion document schema fields (creation_date unknown upstream)
                documents_for_ingestion.append({
                    "id": None,
                    "title": os.path.basename(blob_name),
                    "text": getattr(processing_result, "extracted_text", None) or "",
                    "creation_date": None,
                    "metadata": getattr(processing_result, "metadata", None),
                })
                processed_documents += 1
            else:
                failed_documents += 1
                log.error("Document processing failed", blob_name=blob_name, error=getattr(processing_result, "error_message", None))

            # Progress update
            try:
                progress_result = send_progress_update(
                    project_id,
                    processed_documents + failed_documents,
                    total_documents,
                )
                if not progress_result.get("success"):
                    log.warning(
                        "Progress update failed, continuing processing",
                        project_id=project_id,
                        error=progress_result.get("error_message"),
                    )
            except Exception as progress_error:  # pragma: no cover - side-effect logging only
                log.error(
                    "Progress update failed with exception, continuing processing",
                    project_id=project_id,
                    error=str(progress_error),
                )
        finally:
            if temp_file_path and os.path.exists(temp_file_path):
                try:
                    os.unlink(temp_file_path)
                except Exception:  # pragma: no cover - best-effort cleanup
                    pass

    # Ingestion is performed asynchronously via Redis job published by the Celery task

    # Cleanup blob storage files
    deleted_count = 0
    already_deleted_count = 0
    for blob_name in file_list:
        try:
            delete_result = blob_client.delete_file(blob_name, project_id=project_uuid)
            if getattr(delete_result, "success", False):
                deleted_count += 1
            else:
                err_text = str(getattr(delete_result, "error_message", ""))
                if "BlobNotFound" in err_text or "does not exist" in err_text:
                    already_deleted_count += 1
                else:
                    log.warning(
                        "Failed to delete file from blob storage",
                        blob_name=blob_name,
                        error=getattr(delete_result, "error_message", None),
                    )
        except Exception as e:  # pragma: no cover - side-effect logging only
            log.error("Error deleting file from blob storage", blob_name=blob_name, error=str(e))

    log.info(
        "Cleanup completed",
        project_id=project_id,
        deleted_count=deleted_count,
        already_deleted_count=already_deleted_count,
        total_files=len(file_list),
    )

    # Final progress update (completion)
    try:
        final_progress_result = send_progress_update(project_id, total_documents, total_documents)
        if not final_progress_result.get("success"):
            log.warning("Final progress update failed", project_id=project_id, error=final_progress_result.get("error_message"))
    except Exception as final_progress_error:  # pragma: no cover - side-effect logging only
        log.error("Final progress update failed with exception", project_id=project_id, error=str(final_progress_error))

    log.info(
        "PROJECT PROCESSING COMPLETED",
        project_id=project_id,
        processed_documents=processed_documents,
        failed_documents=failed_documents,
        deleted_count=deleted_count,
        already_deleted_count=already_deleted_count,
    )

    return {
        "success": True,
        "project_id": project_id,
        "total_documents": total_documents,
        "processed_documents": processed_documents,
        "failed_documents": failed_documents,
        "documents_for_ingestion": documents_for_ingestion,
        "cleanup": {
            "deleted_count": deleted_count,
            "already_deleted_count": already_deleted_count,
            "total_files": len(file_list),
        },
    }


