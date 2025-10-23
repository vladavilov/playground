import structlog
import os
import time
import json
from pathlib import Path
from uuid import UUID
from typing import Any, Dict, List

from utils.neo4j_client import Neo4jClient
from utils.blob_storage import get_blob_storage_client
from ingestion_ms.runner import run_graphrag_pipeline as run_ms_pipeline
from config import get_graphrag_settings

logger = structlog.get_logger(__name__)


def validate_json_inputs(input_dir: Path, required_fields: List[str]) -> Dict[str, Any]:
    """
    Validate that all JSON files in input directory contain required fields.
    
    Args:
        input_dir: Directory containing JSON files
        required_fields: List of required field names (e.g., ['title', 'text'])
        
    Returns:
        Dict with validation results:
        {
            'valid': bool,
            'total_files': int,
            'valid_files': int,
            'invalid_files': List[Dict],  # [{filename, missing_fields, error}]
            'error_summary': str
        }
    """
    json_files = list(input_dir.glob("*.json"))
    total_files = len(json_files)
    valid_files = 0
    invalid_files = []
    
    if total_files == 0:
        return {
            'valid': False,
            'total_files': 0,
            'valid_files': 0,
            'invalid_files': [],
            'error_summary': f"No JSON files found in {input_dir}"
        }
    
    for json_file in json_files:
        try:
            with open(json_file, 'r', encoding='utf-8') as f:
                data = json.load(f)
            
            # Check for required fields
            missing_fields = [field for field in required_fields if field not in data]
            
            if missing_fields:
                invalid_files.append({
                    'filename': json_file.name,
                    'missing_fields': missing_fields,
                    'error': f"Missing required fields: {', '.join(missing_fields)}"
                })
            else:
                # Validate that fields are not None or empty strings
                empty_fields = [
                    field for field in required_fields 
                    if data.get(field) is None or (isinstance(data.get(field), str) and not data[field].strip())
                ]
                
                if empty_fields:
                    invalid_files.append({
                        'filename': json_file.name,
                        'missing_fields': empty_fields,
                        'error': f"Required fields are None or empty: {', '.join(empty_fields)}"
                    })
                else:
                    valid_files += 1
                    
        except json.JSONDecodeError as e:
            invalid_files.append({
                'filename': json_file.name,
                'missing_fields': required_fields,
                'error': f"Invalid JSON syntax: {str(e)}"
            })
        except Exception as e:
            invalid_files.append({
                'filename': json_file.name,
                'missing_fields': required_fields,
                'error': f"Failed to read file: {str(e)}"
            })
    
    is_valid = len(invalid_files) == 0
    error_summary = ""
    
    if not is_valid:
        error_summary = (
            f"Input validation failed: {len(invalid_files)}/{total_files} files invalid. "
            f"First errors: {invalid_files[0]['error']}"
        )
    
    return {
        'valid': is_valid,
        'total_files': total_files,
        'valid_files': valid_files,
        'invalid_files': invalid_files,
        'error_summary': error_summary
    }


class Neo4jIngestionService:
    
    def __init__(self, client: Neo4jClient):
        self.client = client
        self.logger = structlog.get_logger(__name__)

    async def run_graphrag_pipeline(self, project_id: str) -> Dict[str, Any]:
        if not isinstance(project_id, str) or not project_id.strip():
            raise ValueError("project_id must be a non-empty string")

        start = time.time()

        documents = 0
        pipeline_output: Dict[str, Any] = {}

        # 1) Sync inputs from Blob → WORKDIR/{project_id}/input (JSON files only)
        try:
            workspace_root = Path(get_graphrag_settings().RAG_WORKSPACE_ROOT)
            input_dir = workspace_root / project_id / "input"
            input_dir.mkdir(parents=True, exist_ok=True)

            prefix = "output/"

            blob_client = get_blob_storage_client()
            # Try to scope by project-specific container when project_id is a valid UUID
            parsed_project_id = None
            try:
                parsed_project_id = UUID(project_id)
            except Exception:
                self.logger.warning(
                    "project_id is not a UUID; falling back to base container for blob operations (%s)",
                    project_id,
                )

            list_result = blob_client.list_files(parsed_project_id, prefix)
            if not getattr(list_result, "success", False):
                raise RuntimeError("Blob listing failed: success flag is False")
            if getattr(list_result, "success", False):
                blobs_to_delete: list[str] = []
                
                for blob_name in list_result.file_list or []:
                    if not str(blob_name).lower().endswith(".json"):
                        continue
                    dest_path = input_dir / os.path.basename(blob_name)
                    download_result = blob_client.download_file(blob_name, str(dest_path), project_id=parsed_project_id)
                    
                    # Verify download succeeded
                    if not getattr(download_result, "success", False):
                        self.logger.error(
                            "Failed to download JSON file",
                            blob_name=blob_name,
                            error=getattr(download_result, "error_message", "Unknown error")
                        )
                        continue
                    
                    documents += 1
                    # Mark for deletion after successful pipeline run
                    blobs_to_delete.append(blob_name)
                
                if documents:
                    # Validate downloaded JSON files before running GraphRAG pipeline
                    self.logger.info(
                        "Validating JSON inputs before GraphRAG pipeline",
                        input_dir=str(input_dir),
                        document_count=documents
                    )
                    
                    validation_result = validate_json_inputs(
                        input_dir=input_dir,
                        required_fields=['title', 'text']
                    )
                    
                    if not validation_result['valid']:
                        error_msg = validation_result['error_summary']
                        self.logger.error(
                            "JSON input validation failed",
                            validation_result=validation_result,
                            project_id=project_id
                        )
                        raise RuntimeError(
                            f"GraphRAG input validation failed: {error_msg}. "
                            f"Invalid files: {validation_result['invalid_files']}"
                        )
                    
                    self.logger.info(
                        "JSON input validation passed",
                        valid_files=validation_result['valid_files'],
                        total_files=validation_result['total_files']
                    )
                    
                    try:
                        result = await run_ms_pipeline(project_id)
                        if isinstance(result, dict):
                            pipeline_output = result
                        self.logger.info(
                            "Successfully processed documents with Microsoft GraphRAG",
                            documents=documents,
                        )
                        
                        # Cleanup source JSONs only after successful ingestion
                        for blob_name in blobs_to_delete:
                            try:
                                blob_client.delete_file(blob_name, project_id=parsed_project_id)
                            except Exception:
                                pass
                    except Exception as e:
                        self.logger.error("Failed to run Microsoft GraphRAG pipeline", error=str(e))
                        raise
        except Exception as e:
            self.logger.error("Blob ingestion failed", error=str(e))
            raise

        return {
            "success": True,
            "counts": {
                "documents": documents
            },
            "pipeline": pipeline_output,
            "duration_ms": round((time.time() - start) * 1000, 2),
        }
