from pathlib import Path

from plugins.base import IngestionContext
from plugins.cobol.plugin import CobolPlugin


def test_cobol_plugin_emits_program_node_and_includes_edge(tmp_path: Path) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()
    (repo / "MAIN.cbl").write_text(
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. HELLO.\n"
        "       DATA DIVISION.\n"
        "       WORKING-STORAGE SECTION.\n"
        "       COPY FOO.\n"
        "       PROCEDURE DIVISION.\n"
        '       CALL "X".\n'
        "       STOP RUN.\n",
        encoding="utf-8",
    )
    (repo / "FOO.cpy").write_text("01 X PIC 9.\n", encoding="utf-8")

    ctx = IngestionContext(project_id="p", repo_fingerprint="r", repo_root=repo, source_language="cobol")
    plugin = CobolPlugin()
    files = list(plugin.iter_files(ctx))
    nodes, edges, facts = plugin.ingest(ctx, files)

    assert any(n.kind == "program" for n in nodes)
    assert any(e.rel_type == "INCLUDES" for e in edges)
    assert facts["includes_count"] == 1



