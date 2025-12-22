import importlib
import io
import sys
from unittest.mock import Mock, patch
from uuid import uuid4

import dulwich.porcelain as porcelain
from fastapi.testclient import TestClient
import zipfile


def _import_main_with_mocked_clients():
    sys.modules.pop("main", None)
    with patch("utils.app_factory.get_postgres_client", return_value=Mock()), patch(
        "utils.app_factory.get_neo4j_client", return_value=Mock()
    ):
        import main  # type: ignore

        importlib.reload(main)
        return main


def test_ingest_git_requires_project_id_and_source_language() -> None:
    main = _import_main_with_mocked_clients()
    client = TestClient(main.app)

    missing_project = client.post(
        "/ingest/git",
        json={"source_language": "cobol", "git_url": "https://example.com/repo.git"},
    )
    assert missing_project.status_code == 422

    missing_lang = client.post(
        "/ingest/git",
        json={"project_id": str(uuid4()), "git_url": "https://example.com/repo.git"},
    )
    assert missing_lang.status_code == 422


def test_ingest_git_returns_contract_shape() -> None:
    main = _import_main_with_mocked_clients()
    client = TestClient(main.app)

    pid = uuid4()
    # Use a local git repo path as the "git_url" to keep tests hermetic.
    # (dulwich can clone from a local path)
    resp = client.post(
        "/ingest/git",
        json={
            "project_id": str(pid),
            "source_language": "java",
            "git_url": str(_make_local_git_repo()),
            "ref": None,
        },
    )
    assert resp.status_code == 200
    body = resp.json()
    assert body["project_id"] == str(pid)
    assert isinstance(body["repo_fingerprint"], str) and len(body["repo_fingerprint"]) > 0


def test_ingest_zip_requires_project_id_and_source_language() -> None:
    main = _import_main_with_mocked_clients()
    client = TestClient(main.app)

    pid = uuid4()
    # Missing project_id
    resp1 = client.post(
        "/ingest/zip",
        data={"source_language": "cobol"},
        files={"file": ("repo.zip", b"not-a-real-zip", "application/zip")},
    )
    assert resp1.status_code == 422

    # Missing source_language
    resp2 = client.post(
        "/ingest/zip",
        data={"project_id": str(pid)},
        files={"file": ("repo.zip", b"not-a-real-zip", "application/zip")},
    )
    assert resp2.status_code == 422


def test_ingest_zip_returns_contract_shape() -> None:
    main = _import_main_with_mocked_clients()
    client = TestClient(main.app)

    pid = uuid4()
    zip_bytes = _make_minimal_zip_bytes()
    resp = client.post(
        "/ingest/zip",
        data={"project_id": str(pid), "source_language": "javascript"},
        files={"file": ("repo.zip", zip_bytes, "application/zip")},
    )
    assert resp.status_code == 200
    body = resp.json()
    assert body["project_id"] == str(pid)
    assert isinstance(body["repo_fingerprint"], str) and len(body["repo_fingerprint"]) > 0


def _make_minimal_zip_bytes() -> bytes:
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w") as zf:
        zf.writestr("hello.js", "console.log('hi');\n")
    return buf.getvalue()


def _make_local_git_repo():
    import tempfile
    from pathlib import Path

    repo = Path(tempfile.mkdtemp()) / "repo"
    porcelain.init(str(repo))
    (repo / "A.java").write_text("class A {}", encoding="utf-8")
    porcelain.add(str(repo), paths=["A.java"])
    porcelain.commit(
        str(repo),
        message=b"c1",
        author=b"Test <test@example.com>",
        committer=b"Test <test@example.com>",
    )
    return repo


