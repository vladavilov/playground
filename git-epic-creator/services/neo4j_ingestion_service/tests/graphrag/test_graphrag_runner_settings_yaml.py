from pathlib import Path
from unittest.mock import patch, MagicMock


def test_run_index_uses_centralized_workspace_root(tmp_path, monkeypatch):
    # Configure workspace root via env (centralized config will read it)
    monkeypatch.setenv("RAG_WORKSPACE_ROOT", str(tmp_path / "graphrag"))

    # Simulate graphrag CLI success for both init and index
    completed = MagicMock()
    completed.returncode = 0
    completed.stdout = "ok"
    completed.stderr = ""

    with patch("ingestion.graphrag_runner.subprocess.run", return_value=completed):
        from ingestion.graphrag_runner import run_index

        project_id = "proj-x"
        workdir = run_index(project_id)

        # settings.yaml should exist under configured root/project
        expected_workdir = Path(tmp_path / "graphrag" / project_id)
        assert Path(workdir) == expected_workdir

        settings_path = expected_workdir / "settings.yaml"
        assert settings_path.exists()
        contents = settings_path.read_text(encoding="utf-8")

        # Should contain relative input/output paths for the project
        assert f"file_pattern: input/{project_id}" in contents
        assert f"path: output/{project_id}" in contents


