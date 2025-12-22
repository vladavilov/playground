from pathlib import Path

from plugins.base import IngestionContext
from plugins.java.plugin import JavaPlugin


def test_java_plugin_emits_class_and_method_and_config_wires(tmp_path: Path) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()
    (repo / "A.java").write_text(
        "package p;\n"
        "public class A {\n"
        "  void m() {}\n"
        "}\n",
        encoding="utf-8",
    )
    (repo / "beans.xml").write_text(
        '<bean id="a" class="p.A"/>\n',
        encoding="utf-8",
    )

    ctx = IngestionContext(project_id="p", repo_fingerprint="r", repo_root=repo, source_language="java")
    plugin = JavaPlugin()
    files = list(plugin.iter_files(ctx))
    nodes, edges, facts = plugin.ingest(ctx, files)

    assert any(n.kind == "class" and n.symbol == "A" for n in nodes)
    assert any(n.kind == "method" and n.symbol == "m" for n in nodes)
    assert any(e.rel_type == "CONFIG_WIRES" for e in edges)
    assert facts["java_file_count"] == 1
    assert facts["xml_file_count"] == 1



