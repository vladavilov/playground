import io
import hashlib
import zipfile
from pathlib import Path

import dulwich.porcelain as porcelain

from core.fingerprint import fingerprint_for_git_repo, fingerprint_for_zip_bytes


def _make_zip(entries: list[tuple[str, bytes]]) -> bytes:
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w") as zf:
        for name, data in entries:
            zf.writestr(name, data)
    return buf.getvalue()


def test_zip_entry_order_changes_fingerprint() -> None:
    z1 = _make_zip([("b.txt", b"2"), ("a.txt", b"1"), ("dir/x.txt", b"3")])
    z2 = _make_zip([("dir/x.txt", b"3"), ("a.txt", b"1"), ("b.txt", b"2")])

    f1 = fingerprint_for_zip_bytes(z1)
    f2 = fingerprint_for_zip_bytes(z2)

    # Simplified contract: fingerprint is based on raw ZIP bytes, so entry order
    # changes (and thus bytes) change the fingerprint.
    assert f1.value != f2.value
    assert f1.anchor == hashlib.sha256(z1).hexdigest()
    assert f2.anchor == hashlib.sha256(z2).hexdigest()
    assert f1.value == f"zip:{f1.anchor}"
    assert f2.value == f"zip:{f2.anchor}"


def test_zip_manifest_change_changes_fingerprint() -> None:
    base = [("src/app.py", b"print('hi')\n"), ("README.md", b"x\n")]
    z1 = _make_zip(base + [("pyproject.toml", b"[project]\nname='a'\n")])
    z2 = _make_zip(base + [("pyproject.toml", b"[project]\nname='b'\n")])

    f1 = fingerprint_for_zip_bytes(z1)
    f2 = fingerprint_for_zip_bytes(z2)
    assert f1.value != f2.value
    assert f1.value.startswith("zip:")
    assert f2.value.startswith("zip:")


def test_git_manifest_change_changes_fingerprint(tmp_path: Path) -> None:
    repo = tmp_path / "repo"
    porcelain.init(str(repo))

    (repo / "pyproject.toml").write_text("[project]\nname='a'\n", encoding="utf-8")
    porcelain.add(str(repo), paths=["pyproject.toml"])
    sha1 = porcelain.commit(
        str(repo),
        message=b"c1",
        author=b"Test <test@example.com>",
        committer=b"Test <test@example.com>",
    )

    fp1 = fingerprint_for_git_repo(
        repo_root=repo,
        head_commit=sha1.decode("ascii") if isinstance(sha1, (bytes, bytearray)) else str(sha1),
    )
    assert fp1.value == f"git:{fp1.anchor}"

    (repo / "pyproject.toml").write_text("[project]\nname='b'\n", encoding="utf-8")
    porcelain.add(str(repo), paths=["pyproject.toml"])
    sha2 = porcelain.commit(
        str(repo),
        message=b"c2",
        author=b"Test <test@example.com>",
        committer=b"Test <test@example.com>",
    )

    fp2 = fingerprint_for_git_repo(
        repo_root=repo,
        head_commit=sha2.decode("ascii") if isinstance(sha2, (bytes, bytearray)) else str(sha2),
    )
    assert fp2.value == f"git:{fp2.anchor}"

    assert fp1.value != fp2.value


