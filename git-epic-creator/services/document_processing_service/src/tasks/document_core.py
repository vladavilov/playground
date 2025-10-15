"""
Core business logic for processing project documents.

This module is framework-agnostic and depends only on injected collaborators,
so it can be tested with lightweight fakes without patching Celery or asyncio.
"""

from __future__ import annotations

from typing import Any, Callable, Dict, List
from pathlib import Path
import json
from uuid import UUID
import os
import tempfile
import structlog


Logger = structlog.stdlib.BoundLogger


def _extract_filtered_metadata(raw_metadata: Dict[str, Any], filename: str) -> Dict[str, Any]:
    """
    Extract and filter relevant metadata for GraphRAG ingestion.
    
    Args:
        raw_metadata: Raw metadata from Tika processor
        filename: Original filename for file type detection
        
    Returns:
        Filtered metadata with only relevant fields
    """
    if not isinstance(raw_metadata, dict):
        return {}
    
    # Extract content type and determine file type
    content_type = raw_metadata.get("Content-Type", "application/octet-stream")
    file_extension = Path(filename).suffix.lower().lstrip(".")
    
    # Map common content types to file types if extension is not reliable
    if file_extension in ["pdf", "docx", "xlsx", "pptx", "txt", "md"]:
        file_type = file_extension
    elif "pdf" in content_type.lower():
        file_type = "pdf"
    elif "wordprocessingml" in content_type.lower():
        file_type = "docx"
    elif "spreadsheetml" in content_type.lower():
        file_type = "xlsx"
    elif "presentationml" in content_type.lower():
        file_type = "pptx"
    elif "text" in content_type.lower():
        file_type = "txt"
    else:
        file_type = file_extension or "unknown"
    
    # Extract dates from various possible metadata fields
    creation_date = None
    modification_date = None
    
    # Try multiple possible date fields
    date_fields = [
        "xmp:CreateDate", "dcterms:created", "pdf:docinfo:created",
        "xmp:ModifyDate", "dcterms:modified", "pdf:docinfo:modified"
    ]
    
    for field in date_fields[:3]:  # Creation date fields
        if field in raw_metadata and raw_metadata[field]:
            creation_date = raw_metadata[field]
            break
    
    for field in date_fields[3:]:  # Modification date fields
        if field in raw_metadata and raw_metadata[field]:
            modification_date = raw_metadata[field]
            break
    
    return {
        "file_name": filename,
        "file_type": file_type,
        "content_type": content_type,
        "creation_date": creation_date,
        "modification_date": modification_date
    }


def process_project_documents_core(
    project_id: str,
    blob_client: Any,
    document_processor: Any,
    send_progress_update: Callable[[str, int, int], Dict[str, Any]],
    logger: Logger | None = None,
) -> Dict[str, Any]:
    """
    Process all documents for a project from blob storage.

    Collaborators are injected to facilitate integration-style testing.

    Args:
        project_id: Project ID as string UUID
        blob_client: Object providing list_files, download_file, delete_file methods
        document_processor: Object providing extract_text_with_result(path) -> result
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

    list_result = blob_client.list_files(project_id=project_uuid, prefix="input/")
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
    empty_documents = 0
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

    # Publish an initial progress update so UI observes 'processing' before first file completes
    try:
        init_progress_result = send_progress_update(
            project_id,
            0,
            total_documents,
        )
        if not init_progress_result.get("success"):
            log.warning(
                "Initial progress update failed, continuing processing",
                project_id=project_id,
                error=init_progress_result.get("error_message"),
                status_code=init_progress_result.get("status_code")
            )
    except Exception as init_progress_error:  # pragma: no cover - side-effect logging only
        log.error(
            "Initial progress update failed with exception, continuing processing",
            project_id=project_id,
            error=str(init_progress_error),
            error_type=type(init_progress_error).__name__,
            exc_info=True
        )

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

            # Defensive logging: Start document processing
            log.info(
                "DOCUMENT_PROCESSING_START",
                blob_name=blob_name,
                temp_file_path=temp_file_path,
                file_size_bytes=os.path.getsize(temp_file_path) if os.path.exists(temp_file_path) else 0,
                project_id=project_id
            )
            
            # Explicit exception handling to prevent silent failures
            try:
                processing_result = document_processor.extract_text_with_result(temp_file_path)
            except Exception as proc_exc:
                # Catch ANY exception from the processor to prevent silent task loss
                failed_documents += 1
                log.error(
                    "DOCUMENT_PROCESSING_EXCEPTION",
                    blob_name=blob_name,
                    temp_file_path=temp_file_path,
                    error=str(proc_exc),
                    error_type=type(proc_exc).__name__,
                    project_id=project_id,
                    exc_info=True
                )
                # Create a failed result to continue processing other documents
                from dataclasses import dataclass
                @dataclass
                class FailedResult:
                    success: bool = False
                    error_message: str = ""
                    extracted_text: str = ""
                    metadata: dict = None
                
                processing_result = FailedResult(
                    success=False,
                    error_message=f"Processor exception: {type(proc_exc).__name__}: {str(proc_exc)}"
                )
                continue
            
            # Defensive logging: Processing completed (success or failure)
            log.info(
                "DOCUMENT_PROCESSING_COMPLETED",
                blob_name=blob_name,
                success=getattr(processing_result, "success", False),
                has_text=bool(getattr(processing_result, "extracted_text", None)),
                text_length=len(getattr(processing_result, "extracted_text", "") or ""),
                error_message=getattr(processing_result, "error_message", None)
            )
            
            if getattr(processing_result, "success", False):
                extracted_text = getattr(processing_result, "extracted_text", None) or ""
                
                # Check if extracted text is empty or only whitespace
                if not extracted_text.strip():
                    empty_documents += 1
                    log.warning(
                        "Document has empty or whitespace-only text content - skipping upload",
                        blob_name=blob_name,
                        text_length=len(extracted_text)
                    )
                    # Skip JSON creation and upload for empty documents
                    continue
                
                # Map to ingestion document schema fields (creation_date unknown upstream)
                documents_for_ingestion.append({
                    "id": None,
                    "title": os.path.basename(blob_name),
                    "text": extracted_text,
                    "creation_date": None,
                    "metadata": getattr(processing_result, "metadata", None),
                })
                processed_documents += 1

                # Write structured JSON and upload to output/
                try:
                    raw_metadata = getattr(processing_result, "metadata", None) or {}
                    
                    # Extract and filter relevant metadata for GraphRAG
                    filtered_metadata = _extract_filtered_metadata(raw_metadata, os.path.basename(blob_name))

                    output_payload = {
                        "title": os.path.basename(blob_name),
                        "text": extracted_text,
                        "metadata": filtered_metadata
                    }
                    with tempfile.NamedTemporaryFile(mode="w", delete=False, suffix=".json", encoding="utf-8") as json_tmp:
                        json.dump(output_payload, json_tmp)
                        json_tmp.flush()
                        json_tmp_path = json_tmp.name
                    output_name = f"output/{Path(os.path.basename(blob_name)).stem}.json"
                    upload_result = blob_client.upload_file(json_tmp_path, output_name, project_id=project_uuid)
                    if not getattr(upload_result, "success", False):
                        log.warning("Failed to upload output JSON", blob_name=output_name, error=getattr(upload_result, "error_message", None))
                except Exception as upload_exc:  # pragma: no cover - side-effect logging only
                    log.error("Exception while uploading output JSON", error=str(upload_exc))
            else:
                failed_documents += 1
                log.error("Document processing failed", blob_name=blob_name, error=getattr(processing_result, "error_message", None))

            # Progress update
            try:
                progress_result = send_progress_update(
                    project_id,
                    processed_documents + failed_documents + empty_documents,
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

    log.info(
        "PROJECT PROCESSING COMPLETED",
        project_id=project_id,
        processed_documents=processed_documents,
        failed_documents=failed_documents,
        empty_documents=empty_documents,
        deleted_count=deleted_count,
        already_deleted_count=already_deleted_count,
    )

    return {
        "success": True,
        "project_id": project_id,
        "total_documents": total_documents,
        "processed_documents": processed_documents,
        "failed_documents": failed_documents,
        "empty_documents": empty_documents,
        "documents_for_ingestion": documents_for_ingestion,
        "cleanup": {
            "deleted_count": deleted_count,
            "already_deleted_count": already_deleted_count,
            "total_files": len(file_list),
        },
    }


