from pathlib import Path
from unittest.mock import patch, MagicMock

import pytest

def test_run_index_initializes_and_invokes_cli(tmp_path, monkeypatch):
    project_id = "proj-abc"
    root = tmp_path / "graphrag"
    monkeypatch.setenv("RAG_WORKSPACE_ROOT", str(root))
    workdir = root / project_id

    # Simulate graphrag CLI success
    completed = MagicMock()
    completed.returncode = 0
    completed.stdout = "ok"
    completed.stderr = ""

    with patch("ingestion.graphrag_runner.subprocess.run", return_value=completed) as mock_run:
        from ingestion.graphrag_runner import run_index

        result = run_index(project_id)

        assert Path(result) == workdir

        # Expect two CLI calls: init (idempotent) and index
        calls = [c.args[0] for c in mock_run.call_args_list]
        assert any("graphrag" in cmd[0] and cmd[1] == "init" for cmd in calls)
        assert any("graphrag" in cmd[0] and cmd[1] == "index" for cmd in calls)


def test_run_index_raises_on_nonzero_exit(tmp_path, monkeypatch):
    project_id = "proj-fail"

    # First call (init) succeeds, second (index) fails
    ok = MagicMock(returncode=0, stdout="init ok", stderr="")
    fail = MagicMock(returncode=1, stdout="", stderr="boom")

    with patch("ingestion.graphrag_runner.subprocess.run", side_effect=[ok, fail]):
        from ingestion.graphrag_runner import run_index, GraphRAGIndexError

        with pytest.raises(GraphRAGIndexError):
            run_index(project_id)


def test_run_index_writes_settings_yaml(tmp_path, monkeypatch):
    # Arrange: isolated workspace root
    root = tmp_path / "graphrag"
    monkeypatch.setenv("RAG_WORKSPACE_ROOT", str(root))
    project_id = "proj-settings"

    # Mock CLI calls succeed
    ok = MagicMock(returncode=0, stdout="ok", stderr="")
    with patch("ingestion.graphrag_runner.subprocess.run", return_value=ok):
        from ingestion.graphrag_runner import run_index

        workdir = run_index(project_id)

        settings_file = Path(workdir) / "settings.yaml"
        assert settings_file.exists(), "settings.yaml should be created in WORKDIR"

        content = settings_file.read_text()
        # Assert the YAML contains relative input/output paths for the project
        assert "input:" in content
        assert f"input/{project_id}" in content
        assert "output:" in content
        assert f"output/{project_id}" in content
        assert "storage:" in content and "base_dir: output" in content
        assert "reporting:" in content and "base_dir: output" in content


