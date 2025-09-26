"""
Tests for metadata handling in document processing.

These tests verify that document metadata is properly extracted, filtered,
and included in the output JSON for GraphRAG ingestion.
"""

import json
from pathlib import Path
from typing import Any, Dict

from tasks.document_core import process_project_documents_core


class _Result:
    def __init__(self, success: bool, **kwargs: Any) -> None:
        self.success = success
        for k, v in kwargs.items():
            setattr(self, k, v)


class FakeBlobClient:
    def __init__(self, list_success: bool = True, file_list: list = None):
        self._list_success = list_success
        self._file_list = file_list or []
        self.uploaded_blobs = []
        self.uploaded_contents = {}

    def list_files(self, project_id=None, prefix=None):
        if self._list_success:
            return _Result(True, file_list=list(self._file_list))
        return _Result(False, error_message="list failed")

    def download_file(self, blob_name: str, local_path: str, project_id=None):
        return _Result(True)

    def upload_file(self, file_path: str, blob_name: str, project_id=None):
        self.uploaded_blobs.append(blob_name)
        # Read and store the content for verification
        with open(file_path, 'r', encoding='utf-8') as f:
            self.uploaded_contents[blob_name] = f.read()
        return _Result(True)

    def delete_file(self, blob_name: str, project_id=None):
        return _Result(True)


class FakeTikaProcessor:
    def __init__(self, metadata: Dict[str, Any]):
        self._metadata = metadata

    def extract_text_with_result(self, file_path: str) -> _Result:
        return _Result(
            True,
            extracted_text="Sample extracted text content",
            metadata=self._metadata
        )


def _progress_stub(_: str, __: int, ___: int) -> Dict[str, Any]:
    return {"success": True}


def test_metadata_includes_required_fields():
    """Test that output JSON includes required metadata fields."""
    # Sample metadata from Tika with all the fields mentioned in requirements
    sample_metadata = {
        "pdf:PDFVersion": "1.7",
        "xmp:CreatorTool": "Adobe InDesign 15.1 (Macintosh)",
        "dc:format": "application/pdf; version=1.7",
        "xmp:CreateDate": "2020-10-14T15:08:10Z",
        "xmp:ModifyDate": "2020-10-14T15:08:10Z",
        "Content-Length": "69173",
        "Content-Type": "application/pdf",
        "dc:language": "en-US",
        "pdf:producer": "Adobe PDF Library 15.0"
    }

    blob_client = FakeBlobClient(
        list_success=True, 
        file_list=["input/test_document.pdf"]
    )
    
    document_processor = FakeTikaProcessor(sample_metadata)

    result = process_project_documents_core(
        project_id="123e4567-e89b-12d3-a456-426614174000",
        blob_client=blob_client,
        document_processor=document_processor,
        send_progress_update=_progress_stub
    )

    # Verify the result structure
    assert result["success"] is True
    assert result["processed_documents"] == 1
    assert result["failed_documents"] == 0

    # Verify that output JSON was uploaded
    assert len(blob_client.uploaded_blobs) == 1
    uploaded_blob_name = blob_client.uploaded_blobs[0]
    assert uploaded_blob_name == "output/test_document.json"

    # Verify the uploaded JSON content
    uploaded_content = blob_client.uploaded_contents[uploaded_blob_name]
    uploaded_data = json.loads(uploaded_content)

    # Check required fields are present
    assert "title" in uploaded_data
    assert "text" in uploaded_data
    assert "metadata" in uploaded_data

    # Check metadata contains required fields
    metadata = uploaded_data["metadata"]
    assert "file_name" in metadata
    assert "file_type" in metadata
    assert "content_type" in metadata
    assert "creation_date" in metadata
    assert "modification_date" in metadata

    # Verify specific metadata values
    assert metadata["file_name"] == "test_document.pdf"
    assert metadata["file_type"] == "pdf"
    assert metadata["content_type"] == "application/pdf"
    assert metadata["creation_date"] == "2020-10-14T15:08:10Z"
    assert metadata["modification_date"] == "2020-10-14T15:08:10Z"


def test_metadata_filters_irrelevant_fields():
    """Test that only relevant metadata fields are included."""
    # Include both relevant and irrelevant metadata
    sample_metadata = {
        "pdf:PDFVersion": "1.7",
        "xmp:CreatorTool": "Adobe InDesign 15.1 (Macintosh)",
        "pdf:hasXFA": "false",
        "X-TIKA:Parsed-By-Full-Set": ["org.apache.tika.parser.DefaultParser"],
        "dc:format": "application/pdf; version=1.7",
        "xmp:CreateDate": "2020-10-14T15:08:10Z",
        "xmp:ModifyDate": "2020-10-14T15:08:10Z",
        "Content-Length": "69173",
        "Content-Type": "application/pdf",
        "dc:language": "en-US",
        "pdf:producer": "Adobe PDF Library 15.0",
        "pdf:hasMarkedContent": "false",
        "pdf:ocrPageCount": "0"
    }

    blob_client = FakeBlobClient(
        list_success=True, 
        file_list=["input/test_document.pdf"]
    )
    
    document_processor = FakeTikaProcessor(sample_metadata)

    result = process_project_documents_core(
        project_id="123e4567-e89b-12d3-a456-426614174000",
        blob_client=blob_client,
        document_processor=document_processor,
        send_progress_update=_progress_stub
    )

    assert result["success"] is True

    # Get uploaded content
    uploaded_content = blob_client.uploaded_contents["output/test_document.json"]
    uploaded_data = json.loads(uploaded_content)
    metadata = uploaded_data["metadata"]

    # Should only contain the 6 required fields
    expected_fields = {"file_name", "file_type", "content_type", "creation_date", "modification_date"}
    assert set(metadata.keys()) == expected_fields

    # Should not contain irrelevant fields
    assert "pdf:hasXFA" not in metadata
    assert "X-TIKA:Parsed-By-Full-Set" not in metadata
    assert "pdf:hasMarkedContent" not in metadata
    assert "pdf:ocrPageCount" not in metadata


def test_metadata_handles_missing_dates():
    """Test that metadata handles missing creation/modification dates gracefully."""
    sample_metadata = {
        "dc:format": "application/pdf; version=1.7",
        "Content-Type": "application/pdf",
        "dc:language": "en-US"
        # No date fields
    }

    blob_client = FakeBlobClient(
        list_success=True, 
        file_list=["input/test_document.pdf"]
    )
    
    document_processor = FakeTikaProcessor(sample_metadata)

    result = process_project_documents_core(
        project_id="123e4567-e89b-12d3-a456-426614174000",
        blob_client=blob_client,
        document_processor=document_processor,
        send_progress_update=_progress_stub
    )

    assert result["success"] is True

    uploaded_content = blob_client.uploaded_contents["output/test_document.json"]
    uploaded_data = json.loads(uploaded_content)
    metadata = uploaded_data["metadata"]

    # Should still have all required fields
    assert "creation_date" in metadata
    assert "modification_date" in metadata
    
    # Missing dates should be None
    assert metadata["creation_date"] is None
    assert metadata["modification_date"] is None


def test_metadata_handles_different_file_types():
    """Test that metadata correctly identifies different file types."""
    test_cases = [
        ("document.pdf", "application/pdf", "pdf"),
        ("report.docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document", "docx"),
        ("data.xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", "xlsx"),
        ("image.jpg", "image/jpeg", "jpg"),
        ("unknown.xyz", "application/octet-stream", "xyz")
    ]

    for filename, content_type, expected_file_type in test_cases:
        sample_metadata = {
            "Content-Type": content_type
        }

        blob_client = FakeBlobClient(
            list_success=True, 
            file_list=[f"input/{filename}"]
        )
        
        document_processor = FakeTikaProcessor(sample_metadata)

        result = process_project_documents_core(
            project_id="123e4567-e89b-12d3-a456-426614174000",
            blob_client=blob_client,
            document_processor=document_processor,
            send_progress_update=_progress_stub
        )

        assert result["success"] is True

        uploaded_content = blob_client.uploaded_contents[f"output/{Path(filename).stem}.json"]
        uploaded_data = json.loads(uploaded_content)
        metadata = uploaded_data["metadata"]

        assert metadata["file_name"] == filename
        assert metadata["file_type"] == expected_file_type
        assert metadata["content_type"] == content_type


def test_metadata_preserves_original_text_content():
    """Test that the original extracted text content is preserved."""
    sample_metadata = {
        "Content-Type": "application/pdf",
        "xmp:CreateDate": "2020-10-14T15:08:10Z"
    }

    blob_client = FakeBlobClient(
        list_success=True, 
        file_list=["input/test_document.pdf"]
    )
    
    document_processor = FakeTikaProcessor(sample_metadata)

    result = process_project_documents_core(
        project_id="123e4567-e89b-12d3-a456-426614174000",
        blob_client=blob_client,
        document_processor=document_processor,
        send_progress_update=_progress_stub
    )

    assert result["success"] is True

    uploaded_content = blob_client.uploaded_contents["output/test_document.json"]
    uploaded_data = json.loads(uploaded_content)

    # Text content should be preserved exactly as extracted
    assert uploaded_data["text"] == "Sample extracted text content"
    assert uploaded_data["title"] == "test_document.pdf"
