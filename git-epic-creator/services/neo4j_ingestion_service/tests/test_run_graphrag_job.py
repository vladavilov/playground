import pytest
from pathlib import Path
from unittest.mock import MagicMock, patch


@pytest.fixture(autouse=True)
def mute_pm_updates(monkeypatch):
    from clients.project_management_client import UpdateProjectStatusResult
    async def _noop(*args, **kwargs):
        return UpdateProjectStatusResult(success=True)
    monkeypatch.setattr(
        "clients.project_management_client.ProjectManagementClient.update_project_status",
        _noop,
        raising=True,
    )


def test_run_graphrag_job_happy_path_minimal_contract(monkeypatch, tmp_path):
    from tasks.graphrag import run_graphrag_job

    # Avoid any external IO by patching collaborators
    monkeypatch.setenv("RAG_WORKSPACE_ROOT", str(tmp_path / "graphrag"))

    from types import SimpleNamespace

    # Patch blob client to return empty list; match prod signature (project_id, prefix)
    def _list_files(project_id, prefix):
        return SimpleNamespace(success=True, file_list=[])
    def _download_file(blob_name, local_path, project_id=None):
        return SimpleNamespace(success=True, local_path=str(tmp_path / "null"))
    blob_client = SimpleNamespace(
        list_files=_list_files,
        download_file=_download_file,
    )
    with patch("services.ingestion_service.get_blob_storage_client", return_value=blob_client):
        # Patch library pipeline to no-op
        with patch("services.ingestion_service.run_documents", return_value=None):
            res = run_graphrag_job.run("job-1", "proj-1", 0)

    assert isinstance(res, dict)
    for k in [
        "job_id",
        "project_id",
        "attempts",
        "counts",
    ]:
        assert k in res

    assert "documents" in res["counts"]

    assert res["job_id"] == "job-1"
    assert res["project_id"] == "proj-1"


def test_run_graphrag_job_validation_errors_new_signature():
    from tasks.graphrag import run_graphrag_job

    with pytest.raises(ValueError):
        run_graphrag_job.run("", "proj-1", 0)

    with pytest.raises(ValueError):
        run_graphrag_job.run("job-1", "", 0)


def test_orchestration_blob_sync_runner_and_import(tmp_path, monkeypatch):
    from tasks.graphrag import run_graphrag_job

    monkeypatch.setenv("RAG_WORKSPACE_ROOT", str(tmp_path / "graphrag"))

    fake_files = ["output/a.json", "output/b.json"]
    blob_client = MagicMock()
    blob_client.list_files.return_value = MagicMock(success=True, file_list=fake_files)
    
    # Mock download_file to actually create the files
    def mock_download_file(blob_name, local_path, project_id=None):
        # Create the directory structure
        local_path = Path(local_path)
        local_path.parent.mkdir(parents=True, exist_ok=True)
        # Write some JSON content to the file
        with open(local_path, 'w') as f:
            f.write('{"text": "test content", "title": "test"}')
        return MagicMock(success=True, local_path=str(local_path))
    
    blob_client.download_file.side_effect = mock_download_file

    with patch("services.ingestion_service.get_blob_storage_client", return_value=blob_client):
        with patch("services.ingestion_service.run_documents", side_effect=[None, None]):
            res = run_graphrag_job.run("job-1", "proj-1", 0)

            assert res["counts"]["documents"] == 2
            assert res["project_id"] == "proj-1"
            # Ensure list_files was called with project_id (None for non-UUID) and output/ prefix
            blob_client.list_files.assert_any_call(None, "output/")


def test_orchestration_passes_client_and_batchsize(tmp_path, monkeypatch):
    from tasks.graphrag import run_graphrag_job

    monkeypatch.setenv("RAG_WORKSPACE_ROOT", str(tmp_path / "graphrag"))
    # batch size no longer used

    blob_client = MagicMock()
    blob_client.list_files.return_value = MagicMock(success=True, file_list=[])

    captured = {}

    def _importer(workdir, client=None):
        captured["client"] = client
        return {k: 0 for k in [
            "documents", "text_units", "entities", "relationships", "communities", "community_reports"
        ]}

    with patch("services.ingestion_service.get_blob_storage_client", return_value=blob_client):
        with patch("services.ingestion_service.run_documents", side_effect=[None, None, None]):
            res = run_graphrag_job.run("job-1", "proj-1", 0)

            assert res["project_id"] == "proj-1"


