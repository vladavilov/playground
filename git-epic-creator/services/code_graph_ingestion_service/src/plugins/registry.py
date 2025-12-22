"""Plugin registry and selection (Task 07)."""

from __future__ import annotations

from typing import Iterable

from plugins.base import IngestionContext, LanguagePlugin


def select_plugin(ctx: IngestionContext, plugins: Iterable[LanguagePlugin]) -> LanguagePlugin:
    by_name = {p.name: p for p in plugins}
    try:
        return by_name[ctx.source_language]
    except KeyError as e:
        raise ValueError(
            f"Unsupported source_language={ctx.source_language!r}. Supported={sorted(by_name)}"
        ) from e


def run_selected_plugin(
    ctx: IngestionContext, plugins: Iterable[LanguagePlugin]
) -> tuple[list, list, dict]:
    plugin = select_plugin(ctx, plugins)
    files = sorted(plugin.iter_files(ctx), key=lambda p: p.as_posix())
    nodes, edges, facts = plugin.ingest(ctx, files)
    return nodes, edges, {plugin.name: facts}


