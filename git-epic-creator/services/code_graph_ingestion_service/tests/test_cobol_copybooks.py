from pathlib import Path

from plugins.cobol.copybooks import expand_copybooks


def test_copybook_expansion_inlines_content_and_records_includes(tmp_path: Path) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()
    (repo / "FOO.cpy").write_text("01 X PIC 9.\n", encoding="utf-8")

    logical = ["       COPY FOO.", "       STOP RUN."]
    spans = [(1, 1), (2, 2)]
    res = expand_copybooks(repo_root=repo, logical_lines=logical, logical_spans=spans)

    assert "01 X PIC 9." in "\n".join(res.logical_lines)
    assert "FOO.cpy" in res.includes
    assert res.expansion_map and res.expansion_map[0].copybook_path == "FOO.cpy"


def test_copy_replacing_basic_substitution(tmp_path: Path) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()
    (repo / "BAR.cpy").write_text("MOVE A TO B.\n", encoding="utf-8")

    logical = ["COPY BAR REPLACING ==A== BY ==X==.", "STOP RUN."]
    spans = [(1, 1), (2, 2)]
    res = expand_copybooks(repo_root=repo, logical_lines=logical, logical_spans=spans)

    joined = "\n".join(res.logical_lines)
    assert "MOVE X TO B." in joined

