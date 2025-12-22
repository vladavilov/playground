import importlib
import io
import sys
import zipfile
from unittest.mock import Mock, patch
from uuid import uuid4

import dulwich.porcelain as porcelain
from fastapi.testclient import TestClient


def _import_main_with_mocked_clients():
    sys.modules.pop("main", None)
    with patch("utils.app_factory.get_postgres_client", return_value=Mock()), patch(
        "utils.app_factory.get_neo4j_client", return_value=Mock()
    ):
        import main  # type: ignore

        importlib.reload(main)
        return main


def _zip_bytes(entries: list[tuple[str, bytes]]) -> bytes:
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w") as zf:
        for name, data in entries:
            zf.writestr(name, data)
    return buf.getvalue()


def test_zip_ingest_changes_across_entry_order() -> None:
    main = _import_main_with_mocked_clients()
    client = TestClient(main.app)

    pid = uuid4()
    z1 = _zip_bytes([("a.js", b"1"), ("b.js", b"2")])
    z2 = _zip_bytes([("b.js", b"2"), ("a.js", b"1")])

    r1 = client.post(
        "/ingest/zip",
        data={"project_id": str(pid), "source_language": "javascript"},
        files={"file": ("repo.zip", z1, "application/zip")},
    ).json()["repo_fingerprint"]
    r2 = client.post(
        "/ingest/zip",
        data={"project_id": str(pid), "source_language": "javascript"},
        files={"file": ("repo.zip", z2, "application/zip")},
    ).json()["repo_fingerprint"]

    # Simplified contract: ZIP fingerprint hashes raw ZIP bytes, so entry order
    # changes the bytes and should change the fingerprint.
    assert r1 != r2


def test_git_ingest_is_deterministic_for_same_ref(tmp_path) -> None:
    repo = tmp_path / "repo"
    porcelain.init(str(repo))
    (repo / "A.java").write_text("class A{}", encoding="utf-8")
    porcelain.add(str(repo), paths=["A.java"])
    sha = porcelain.commit(
        str(repo),
        message=b"c1",
        author=b"Test <test@example.com>",
        committer=b"Test <test@example.com>",
    )
    ref = sha.decode("ascii") if isinstance(sha, (bytes, bytearray)) else str(sha)

    main = _import_main_with_mocked_clients()
    client = TestClient(main.app)
    pid1 = uuid4()
    pid2 = uuid4()

    r1 = client.post(
        "/ingest/git",
        json={"project_id": str(pid1), "source_language": "java", "git_url": str(repo), "ref": ref},
    ).json()["repo_fingerprint"]
    r2 = client.post(
        "/ingest/git",
        json={"project_id": str(pid2), "source_language": "java", "git_url": str(repo), "ref": ref},
    ).json()["repo_fingerprint"]

    assert r1 == r2



