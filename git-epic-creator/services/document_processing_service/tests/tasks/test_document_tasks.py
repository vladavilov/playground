"""
Simplified integration-style tests for document processing core logic.

These tests exercise the business logic by injecting lightweight fakes for
external services (blob storage, Tika, and progress updates), avoiding
framework and infrastructure mocking.
"""

from typing import Any, Callable, Dict, List, Optional


class _Result:
    def __init__(self, success: bool, **kwargs: Any) -> None:
        self.success = success
        for k, v in kwargs.items():
            setattr(self, k, v)


class FakeBlobClient:
    def __init__(
        self,
        list_success: bool,
        file_list: Optional[List[str]] = None,
        download_success: bool = True,
        delete_success: bool = True,
        list_error: Optional[str] = None,
    ) -> None:
        self._list_success = list_success
        self._file_list = file_list or []
        self._download_success = download_success
        self._delete_success = delete_success
        self._list_error = list_error

    def __init__(
        self,
        list_success: bool,
        file_list: Optional[List[str]] = None,
        download_success: bool = True,
        delete_success: bool = True,
        list_error: Optional[str] = None,
    ) -> None:
        self._list_success = list_success
        self._file_list = file_list or []
        self._download_success = download_success
        self._delete_success = delete_success
        self._list_error = list_error
        self.last_list_prefix = None
        self.uploaded_blobs: List[str] = []
        self.deleted_blobs: List[str] = []

    def list_files(self, project_id=None, prefix=None):  # type: ignore[no-untyped-def]
        self.last_list_prefix = prefix
        if self._list_success:
            return _Result(True, file_list=list(self._file_list))
        return _Result(False, error_message=self._list_error or "list failed")

    def download_file(self, blob_name: str, local_path: str, project_id=None):  # type: ignore[no-untyped-def]
        return _Result(self._download_success)

    def upload_file(self, file_path: str, blob_name: str, project_id=None):  # type: ignore[no-untyped-def]
        self.uploaded_blobs.append(blob_name)
        return _Result(True)

    def delete_file(self, blob_name: str, project_id=None):  # type: ignore[no-untyped-def]
        self.deleted_blobs.append(blob_name)
        return _Result(self._delete_success)


class FakeTikaProcessor:
    def __init__(self, outcomes: Dict[str, _Result]) -> None:
        # outcomes keyed by blob_name
        self._outcomes = outcomes

    def extract_text_with_result(self, file_path: str) -> _Result:  # type: ignore[no-untyped-def]
        # Map temp file usage back to blob name via sentinel suffix if provided in outcomes
        # In tests, we configure outcomes directly by blob name and just return the mapped result
        # since core ties the result to the blob being processed.
        # For simplicity, pop an arbitrary success when only one file is expected.
        if len(self._outcomes) == 1:
            return next(iter(self._outcomes.values()))
        # Fallback: return a generic success
        return _Result(True, extracted_text="", file_type="application/octet-stream", page_count=1, metadata={})


def _progress_stub(_: str, __: int, ___: int) -> Dict[str, Any]:
    return {"success": True}


def test_success_single_file():
    from tasks.document_core import process_project_documents_core

    blob = FakeBlobClient(list_success=True, file_list=["input/doc1.pdf"], download_success=True, delete_success=True)

    tika_success = _Result(
        True,
        extracted_text="Hello",
        file_type="application/pdf",
        page_count=1,
        metadata={"k": "v"},
        to_structured_json=lambda: {"text": "Hello"},
    )
    tika = FakeTikaProcessor({"doc1.pdf": tika_success})

    result = process_project_documents_core(
        project_id="00000000-0000-0000-0000-000000000001",
        blob_client=blob,
        document_processor=tika,
        send_progress_update=_progress_stub,
    )

    assert result["success"] is True
    assert result["total_documents"] == 1
    assert result["processed_documents"] == 1
    assert result["failed_documents"] == 0
    assert isinstance(result.get("documents_for_ingestion"), list)
    # Must list from input/
    assert blob.last_list_prefix == "input/"
    # Must upload to output/
    assert any(name.startswith("output/") for name in blob.uploaded_blobs)
    # Must delete the processed input blob
    assert "input/doc1.pdf" in blob.deleted_blobs


def test_list_failure_returns_false():
    from tasks.document_core import process_project_documents_core

    blob = FakeBlobClient(list_success=False, list_error="Storage connection failed")
    tika = FakeTikaProcessor({})

    result = process_project_documents_core(
        project_id="00000000-0000-0000-0000-000000000002",
        blob_client=blob,
        document_processor=tika,
        send_progress_update=_progress_stub,
    )

    assert result["success"] is False
    assert "error_message" in result


def test_partial_failure_two_files():
    from tasks.document_core import process_project_documents_core

    blob = FakeBlobClient(list_success=True, file_list=["input/good.pdf", "input/bad.pdf"], download_success=True, delete_success=True)

    success = _Result(
        True,
        extracted_text="ok",
        file_type="application/pdf",
        page_count=1,
        metadata={},
        to_structured_json=lambda: {"text": "ok"},
    )
    failure = _Result(False, error_message="Corrupted file")
    tika = FakeTikaProcessor({"good.pdf": success, "bad.pdf": failure})

    # Override extract behavior to return success then failure deterministically
    call_count = {"n": 0}

    def side_effect(_: str) -> _Result:
        call_count["n"] += 1
        return success if call_count["n"] == 1 else failure

    tika.extract_text_with_result = side_effect  # type: ignore[assignment]

    result = process_project_documents_core(
        project_id="00000000-0000-0000-0000-000000000003",
        blob_client=blob,
        document_processor=tika,
        send_progress_update=_progress_stub,
    )

    assert result["success"] is True
    assert result["total_documents"] == 2
    assert result["processed_documents"] == 1
    assert result["failed_documents"] == 1
    # List prefix asserted
    assert blob.last_list_prefix == "input/"
    # Output upload attempted at least once
    assert any(name.startswith("output/") for name in blob.uploaded_blobs)
    # Input deletions attempted
    assert "input/good.pdf" in blob.deleted_blobs or "input/bad.pdf" in blob.deleted_blobs


def test_empty_text_document_tracked_separately():
    """Test that documents with empty text are tracked separately and not uploaded."""
    from tasks.document_core import process_project_documents_core

    blob = FakeBlobClient(
        list_success=True, 
        file_list=["input/empty.pdf"], 
        download_success=True, 
        delete_success=True
    )

    empty_result = _Result(
        True,
        extracted_text="",
        file_type="application/pdf",
        page_count=1,
        metadata={"Content-Type": "application/pdf"},
    )
    tika = FakeTikaProcessor({"empty.pdf": empty_result})

    result = process_project_documents_core(
        project_id="00000000-0000-0000-0000-000000000004",
        blob_client=blob,
        document_processor=tika,
        send_progress_update=_progress_stub,
    )

    assert result["success"] is True
    assert result["total_documents"] == 1
    assert result["processed_documents"] == 0
    assert result["failed_documents"] == 0
    assert result["empty_documents"] == 1
    # No output JSON should be uploaded for empty documents
    assert len(blob.uploaded_blobs) == 0
    # Input file should still be deleted
    assert "input/empty.pdf" in blob.deleted_blobs


def test_empty_whitespace_only_text_tracked_as_empty():
    """Test that documents with only whitespace are treated as empty."""
    from tasks.document_core import process_project_documents_core

    blob = FakeBlobClient(
        list_success=True, 
        file_list=["input/whitespace.pdf"], 
        download_success=True, 
        delete_success=True
    )

    whitespace_result = _Result(
        True,
        extracted_text="   \n\t\r   ",
        file_type="application/pdf",
        page_count=1,
        metadata={"Content-Type": "application/pdf"},
    )
    tika = FakeTikaProcessor({"whitespace.pdf": whitespace_result})

    result = process_project_documents_core(
        project_id="00000000-0000-0000-0000-000000000005",
        blob_client=blob,
        document_processor=tika,
        send_progress_update=_progress_stub,
    )

    assert result["success"] is True
    assert result["total_documents"] == 1
    assert result["empty_documents"] == 1
    assert result["processed_documents"] == 0
    # No output JSON should be uploaded
    assert len(blob.uploaded_blobs) == 0


def test_mixed_empty_and_valid_documents():
    """Test processing mix of empty and valid documents."""
    from tasks.document_core import process_project_documents_core

    blob = FakeBlobClient(
        list_success=True, 
        file_list=["input/valid.pdf", "input/empty.pdf", "input/good.pdf"], 
        download_success=True, 
        delete_success=True
    )

    valid_result = _Result(
        True,
        extracted_text="Valid content",
        file_type="application/pdf",
        page_count=1,
        metadata={},
    )
    empty_result = _Result(
        True,
        extracted_text="",
        file_type="application/pdf",
        page_count=1,
        metadata={},
    )

    # Simulate three calls: valid, empty, valid
    call_count = {"n": 0}
    
    def side_effect(_: str) -> _Result:
        call_count["n"] += 1
        if call_count["n"] == 2:
            return empty_result
        return valid_result

    tika = FakeTikaProcessor({})
    tika.extract_text_with_result = side_effect  # type: ignore[assignment]

    result = process_project_documents_core(
        project_id="00000000-0000-0000-0000-000000000006",
        blob_client=blob,
        document_processor=tika,
        send_progress_update=_progress_stub,
    )

    assert result["success"] is True
    assert result["total_documents"] == 3
    assert result["processed_documents"] == 2
    assert result["empty_documents"] == 1
    assert result["failed_documents"] == 0
    # Only 2 output JSONs should be uploaded (not the empty one)
    output_uploads = [b for b in blob.uploaded_blobs if b.startswith("output/")]
    assert len(output_uploads) == 2
    # All input files should be deleted
    assert len(blob.deleted_blobs) == 3


def test_all_empty_documents_no_valid_content():
    """Test project with all documents having empty content."""
    from tasks.document_core import process_project_documents_core

    blob = FakeBlobClient(
        list_success=True, 
        file_list=["input/empty1.pdf", "input/empty2.pdf"], 
        download_success=True, 
        delete_success=True
    )

    empty_result = _Result(
        True,
        extracted_text="",
        file_type="application/pdf",
        page_count=1,
        metadata={},
    )
    tika = FakeTikaProcessor({"empty1.pdf": empty_result})

    result = process_project_documents_core(
        project_id="00000000-0000-0000-0000-000000000007",
        blob_client=blob,
        document_processor=tika,
        send_progress_update=_progress_stub,
    )

    assert result["success"] is True
    assert result["total_documents"] == 2
    assert result["processed_documents"] == 0
    assert result["empty_documents"] == 2
    assert result["failed_documents"] == 0
    # No output JSONs should be uploaded
    assert len(blob.uploaded_blobs) == 0
    # documents_for_ingestion should be empty
    assert len(result["documents_for_ingestion"]) == 0


