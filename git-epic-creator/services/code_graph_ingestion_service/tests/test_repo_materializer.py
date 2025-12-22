import hashlib
import io
import zipfile
from pathlib import Path

import dulwich.porcelain as porcelain
import pytest

from core.repo_materializer import ZipSlipError, materialize_git, materialize_zip_bytes
from core.workspace import Workspace


def _tree_digest(root: Path) -> str:
    """Deterministic digest of extracted repo contents (paths + bytes)."""

    h = hashlib.sha256()
    files = sorted(
        [p for p in root.rglob("*") if p.is_file() and ".git" not in p.parts],
        key=lambda p: p.as_posix(),
    )
    for p in files:
        rel = p.relative_to(root).as_posix().encode("utf-8")
        h.update(rel + b"\0")
        h.update(p.read_bytes())
        h.update(b"\n")
    return h.hexdigest()


def test_zip_slip_rejected(tmp_path: Path) -> None:
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w") as zf:
        zf.writestr("../evil.txt", "nope")

    with pytest.raises(ZipSlipError):
        materialize_zip_bytes(zip_bytes=buf.getvalue(), dest_dir=tmp_path / "out")


def test_zip_extract_is_deterministic_for_identical_input(tmp_path: Path) -> None:
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w") as zf:
        zf.writestr("b.txt", b"world")
        zf.writestr("a.txt", b"hello")
        zf.writestr("dir/sub.txt", b"sub")

    zip_bytes = buf.getvalue()

    out1 = tmp_path / "out1"
    out2 = tmp_path / "out2"
    materialize_zip_bytes(zip_bytes=zip_bytes, dest_dir=out1)
    materialize_zip_bytes(zip_bytes=zip_bytes, dest_dir=out2)

    assert _tree_digest(out1) == _tree_digest(out2)


def test_git_clone_and_checkout_ref_is_deterministic(tmp_path: Path) -> None:
    # Create a local git repo with two commits.
    src_repo = tmp_path / "src_repo"
    porcelain.init(str(src_repo))

    (src_repo / "file.txt").write_text("v1", encoding="utf-8")
    porcelain.add(str(src_repo), paths=["file.txt"])
    sha1 = porcelain.commit(
        str(src_repo),
        message=b"c1",
        author=b"Test <test@example.com>",
        committer=b"Test <test@example.com>",
    )

    (src_repo / "file.txt").write_text("v2", encoding="utf-8")
    porcelain.add(str(src_repo), paths=["file.txt"])
    _sha2 = porcelain.commit(
        str(src_repo),
        message=b"c2",
        author=b"Test <test@example.com>",
        committer=b"Test <test@example.com>",
    )

    # Materialize twice at the first commit, ensuring identical working tree bytes.
    ref = sha1.decode("ascii") if isinstance(sha1, (bytes, bytearray)) else str(sha1)

    out1 = tmp_path / "clone1"
    out2 = tmp_path / "clone2"
    res1 = materialize_git(git_url=str(src_repo), ref=ref, dest_dir=out1)
    res2 = materialize_git(git_url=str(src_repo), ref=ref, dest_dir=out2)

    assert (res1.repo_root / "file.txt").read_text(encoding="utf-8") == "v1"
    assert (res2.repo_root / "file.txt").read_text(encoding="utf-8") == "v1"
    assert _tree_digest(res1.repo_root) == _tree_digest(res2.repo_root)


def test_workspace_paths_are_deterministic(tmp_path: Path) -> None:
    ws = Workspace(root=tmp_path / "ws")
    p1 = ws.zip_repo_dir("00000000-0000-0000-0000-000000000000")
    p2 = ws.zip_repo_dir("00000000-0000-0000-0000-000000000000")
    assert p1 == p2

    g1 = ws.git_repo_dir("p", "https://example.com/repo.git", "main")
    g2 = ws.git_repo_dir("p", "https://example.com/repo.git", "main")
    assert g1 == g2


