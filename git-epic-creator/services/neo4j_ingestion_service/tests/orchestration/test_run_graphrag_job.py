import pytest
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

    # Patch blob client to return empty list
    blob_client = SimpleNamespace(
        list_files=lambda **kwargs: SimpleNamespace(success=True, file_list=[]),
        download_file=lambda *args, **kwargs: SimpleNamespace(success=True, local_path=str(tmp_path / "null")),
    )
    with patch("services.ingestion_service.get_blob_storage_client", return_value=blob_client):
        # Patch graphrag runner to return a valid workdir
        with patch(
            "services.ingestion_service.run_index",
            return_value=tmp_path / "graphrag" / "proj-1",
        ):
            # Patch importer to return zero counts
            with patch(
                "services.ingestion_service.import_graphrag_outputs",
                return_value={
                    "documents": 0,
                    "text_units": 0,
                    "entities": 0,
                    "relationships": 0,
                    "communities": 0,
                    "community_reports": 0,
                },
            ):
                res = run_graphrag_job.run("job-1", "proj-1", 0)

    assert isinstance(res, dict)
    for k in [
        "job_id",
        "project_id",
        "attempts",
        "counts",
    ]:
        assert k in res

    for ck in [
        "documents",
        "text_units",
        "entities",
        "relationships",
        "communities",
        "community_reports",
    ]:
        assert ck in res["counts"]

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

    fake_files = ["inputs/proj-1/a.json", "inputs/proj-1/b.json"]
    blob_client = MagicMock()
    blob_client.list_files.return_value = MagicMock(success=True, file_list=fake_files)
    blob_client.download_file.return_value = MagicMock(success=True, local_path=str(tmp_path/"null"))

    with patch("services.ingestion_service.get_blob_storage_client", return_value=blob_client):
        with patch("services.ingestion_service.run_index", return_value=tmp_path / "graphrag" / "proj-1"):
            importer_counts = {
                "documents": 2,
                "text_units": 1,
                "entities": 1,
                "relationships": 0,
                "communities": 0,
                "community_reports": 0,
            }
            with patch("services.ingestion_service.import_graphrag_outputs", return_value=importer_counts):
                res = run_graphrag_job.run("job-1", "proj-1", 0)

                assert res["counts"]["documents"] == 2
                assert res["project_id"] == "proj-1"


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
        with patch("services.ingestion_service.run_index") as _runner:
            _runner.return_value = tmp_path / "graphrag" / "proj-1"
            with patch("services.ingestion_service.import_graphrag_outputs", side_effect=_importer):
                res = run_graphrag_job.run("job-1", "proj-1", 0)

                assert res["project_id"] == "proj-1"
                assert captured["client"] is not None
                assert "batch_size" not in captured


