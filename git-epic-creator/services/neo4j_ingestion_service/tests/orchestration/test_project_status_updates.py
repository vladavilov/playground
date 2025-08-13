import pytest
from unittest.mock import patch, MagicMock


@pytest.mark.parametrize(
    "counts",
    [
        {"documents": 0},
        {"documents": 3, "text_units": 10},
    ],
)
def test_status_updates_on_success(monkeypatch, counts):
    from tasks.graphrag import run_graphrag_job

    # Patch the service used by the task to avoid real work
    class DummyService:
        def __init__(self, *args, **kwargs):
            pass

        def run_graphrag_pipeline(self, project_id: str):
            return {"counts": counts, "duration_ms": 123}

    monkeypatch.setattr("tasks.graphrag.Neo4jIngestionService", DummyService)

    # Track calls to update_project_status (async function)
    recorded_calls = []

    async def _fake_update_status(*args, **kwargs):
        recorded_calls.append({"args": args, "kwargs": kwargs})
        from clients.project_management_client import UpdateProjectStatusResult

        return UpdateProjectStatusResult(success=True)

    monkeypatch.setattr(
        "clients.project_management_client.ProjectManagementClient.update_project_status",
        _fake_update_status,
        raising=True,
    )

    res = run_graphrag_job.run("job-1", "proj-1", 0)

    assert res["job_id"] == "job-1"
    assert res["project_id"] == "proj-1"

    # Expect at least two calls: processing at start, and completion with counts
    assert len(recorded_calls) >= 2
    # First call: processing
    assert recorded_calls[0]["kwargs"].get("project_id") == "proj-1"
    assert recorded_calls[0]["kwargs"].get("status") == "processing"

    # Final call should include counts and rag_ready status
    final_call = recorded_calls[-1]["kwargs"]
    assert final_call.get("project_id") == "proj-1"
    # processed_count should equal documents (default 0 if missing)
    expected_processed = int(counts.get("documents", 0))
    assert final_call.get("processed_count") == expected_processed
    assert final_call.get("total_count") == max(expected_processed, 1)
    # Require explicit rag_ready per acceptance criteria
    assert final_call.get("status") == "rag_ready"


def test_status_updates_on_failure(monkeypatch):
    from tasks.graphrag import run_graphrag_job

    class FailingService:
        def __init__(self, *args, **kwargs):
            pass

        def run_graphrag_pipeline(self, project_id: str):
            raise RuntimeError("boom")

    monkeypatch.setattr("tasks.graphrag.Neo4jIngestionService", FailingService)

    recorded_calls = []

    async def _fake_update_status(*args, **kwargs):
        recorded_calls.append({"args": args, "kwargs": kwargs})
        from clients.project_management_client import UpdateProjectStatusResult

        return UpdateProjectStatusResult(success=True)

    monkeypatch.setattr(
        "clients.project_management_client.ProjectManagementClient.update_project_status",
        _fake_update_status,
        raising=True,
    )

    # Avoid hitting Redis/Celery scheduling in this unit test
    monkeypatch.setattr(
        "tasks.graphrag.schedule_ingestion_retry_task.apply_async",
        lambda *args, **kwargs: None,
        raising=True,
    )

    with pytest.raises(RuntimeError):
        run_graphrag_job.run("job-err", "proj-err", 2)

    # Expect a failure status update with rag_failed and error message
    failure_calls = [c for c in recorded_calls if c["kwargs"].get("status")]
    assert any(c["kwargs"].get("status") == "rag_failed" for c in failure_calls)
    assert any("boom" in (c["kwargs"].get("error_message") or "") for c in recorded_calls)


