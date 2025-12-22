from pathlib import Path

from core.ignore_rules import build_ignore_rules
from core.inventory import build_inventory


def test_inventory_is_deterministic_and_ignores_git(tmp_path: Path) -> None:
    repo = tmp_path / "repo"
    (repo / ".git").mkdir(parents=True)
    (repo / ".git" / "config").write_text("x", encoding="utf-8")
    (repo / "a.cbl").write_text("       IDENTIFICATION DIVISION.\n", encoding="utf-8")
    (repo / "b.java").write_text("class B {}\n", encoding="utf-8")
    (repo / "node_modules").mkdir()
    (repo / "node_modules" / "x.js").write_text("export {}\n", encoding="utf-8")

    ignore = build_ignore_rules(repo)
    inv1 = build_inventory(repo, ignore=ignore)
    inv2 = build_inventory(repo, ignore=ignore)

    assert [e.path for e in inv1] == [e.path for e in inv2]
    assert all(not p.path.startswith(".git/") for p in inv1)
    assert all(not p.path.startswith("node_modules/") for p in inv1)

    # Language inference sanity
    by_path = {e.path: e for e in inv1}
    assert by_path["a.cbl"].language == "cobol"
    assert by_path["b.java"].language == "java"


def test_gitignore_file_is_respected(tmp_path: Path) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()
    (repo / ".gitignore").write_text("ignored.txt\n", encoding="utf-8")
    (repo / "ignored.txt").write_text("nope\n", encoding="utf-8")
    (repo / "kept.txt").write_text("yep\n", encoding="utf-8")

    inv = build_inventory(repo)
    paths = [e.path for e in inv]
    assert "kept.txt" in paths
    assert "ignored.txt" not in paths


