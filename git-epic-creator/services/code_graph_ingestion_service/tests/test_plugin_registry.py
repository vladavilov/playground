from dataclasses import dataclass
from pathlib import Path
from unittest.mock import Mock

import pytest

from plugins.base import IngestionContext
from plugins.registry import run_selected_plugin, select_plugin


@dataclass(frozen=True)
class _Plugin:
    name: str
    file_globs: tuple[str, ...] = ()

    def __post_init__(self):
        object.__setattr__(self, "iter_files", Mock(return_value=[]))
        object.__setattr__(self, "ingest", Mock(return_value=([], [], {"ok": True})))


def test_select_plugin_supported_and_unsupported() -> None:
    ctx = IngestionContext(project_id="p", repo_fingerprint="r", repo_root=Path("."), source_language="java")
    p_java = _Plugin(name="java")
    p_cobol = _Plugin(name="cobol")

    assert select_plugin(ctx, [p_java, p_cobol]) is p_java

    bad = IngestionContext(project_id="p", repo_fingerprint="r", repo_root=Path("."), source_language="ruby")
    with pytest.raises(ValueError):
        select_plugin(bad, [p_java, p_cobol])


def test_run_selected_plugin_executes_exactly_one_plugin() -> None:
    ctx = IngestionContext(project_id="p", repo_fingerprint="r", repo_root=Path("."), source_language="cobol")
    p_java = _Plugin(name="java")
    p_cobol = _Plugin(name="cobol")

    nodes, edges, facts = run_selected_plugin(ctx, [p_java, p_cobol])
    assert nodes == []
    assert edges == []
    assert facts == {"cobol": {"ok": True}}

    p_cobol.ingest.assert_called_once()
    p_java.ingest.assert_not_called()


