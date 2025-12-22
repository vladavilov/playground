from pathlib import Path

from plugins.base import IngestionContext
from plugins.javascript.plugin import JavaScriptPlugin


def test_javascript_plugin_emits_imports_edges(tmp_path: Path) -> None:
    repo = tmp_path / "repo"
    repo.mkdir()
    (repo / "a.js").write_text(
        "import x from 'lib-x';\n"
        "const y = require('lib-y');\n"
        "function f() { return 1 }\n",
        encoding="utf-8",
    )
    ctx = IngestionContext(project_id="p", repo_fingerprint="r", repo_root=repo, source_language="javascript")
    plugin = JavaScriptPlugin()
    files = list(plugin.iter_files(ctx))
    nodes, edges, facts = plugin.ingest(ctx, files)

    assert any(e.rel_type == "IMPORTS" for e in edges)
    assert facts["js_file_count"] == 1
    assert any(n.kind == "unresolved" and n.symbol == "lib-x" for n in nodes)
    assert any(n.kind == "unresolved" and n.symbol == "lib-y" for n in nodes)



