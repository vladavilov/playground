"""JavaScript plugin (Task 18).

Minimal deterministic module resolution:
- Parse static `import ... from "..."` and CommonJS `require("...")`
- Emit IMPORTS edges to explicit unresolved module nodes
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

from tree_sitter_language_pack import get_language, get_parser
from tree_sitter import Query, QueryCursor  # type: ignore

from core.records import CodeNodeRecord, EdgeRecord
from core.stable_ids import snippet_hash, stable_node_id
from plugins.base import IngestionContext


_RE_IMPORT_FROM = re.compile(r'\bimport\b[\s\S]*?\bfrom\s+[\'"]([^\'"]+)[\'"]', re.IGNORECASE)
_RE_REQUIRE = re.compile(r'\brequire\s*\(\s*[\'"]([^\'"]+)[\'"]\s*\)', re.IGNORECASE)


@dataclass(frozen=True)
class JavaScriptPlugin:
    name: str = "javascript"
    file_globs: tuple[str, ...] = (".js", ".jsx", ".mjs", ".cjs", ".ts", ".tsx")

    def iter_files(self, ctx: IngestionContext):
        files = []
        for p in ctx.repo_root.rglob("*"):
            if not p.is_file() or p.is_symlink():
                continue
            rel = p.relative_to(ctx.repo_root).as_posix().lower()
            if rel.endswith(self.file_globs):
                files.append(p)
        return sorted(files, key=lambda x: x.relative_to(ctx.repo_root).as_posix())

    def ingest(self, ctx: IngestionContext, files: list[Path]):
        nodes: dict[str, CodeNodeRecord] = {}
        edges: list[EdgeRecord] = []

        for p in files:
            rel = p.relative_to(ctx.repo_root).as_posix()
            b = p.read_bytes()
            text = b.decode("utf-8", errors="replace").replace("\r\n", "\n").replace("\r", "\n")
            lines = text.split("\n")
            if lines and lines[-1] == "":
                lines = lines[:-1]

            module_id = stable_node_id(ctx.project_id, ctx.repo_fingerprint, rel, "module", "", "1", str(len(lines)))
            module_text = "\n".join(lines) + "\n"
            module_hash = snippet_hash(file_path=rel, start_line=1, end_line=len(lines), text=module_text)
            nodes[module_id] = CodeNodeRecord(
                project_id=ctx.project_id,
                repo_fingerprint=ctx.repo_fingerprint,
                node_id=module_id,
                language="javascript",
                kind="module",
                symbol=None,
                file_path=rel,
                start_line=1,
                end_line=len(lines),
                snippet_hash=module_hash,
                text=module_text,
            )

            # Function declarations via tree-sitter.
            try:
                parser = get_parser("javascript")
                lang = get_language("javascript")
                tree = parser.parse(b)
                q = Query(lang, "(function_declaration name:(identifier) @name) @fn")
                caps = QueryCursor(q).captures(tree.root_node)
                for fn in caps.get("fn", []):
                    name_node = fn.child_by_field_name("name")
                    sym = name_node.text.decode("utf-8", errors="replace") if name_node else None
                    s = int(fn.start_point[0]) + 1
                    e = int(fn.end_point[0]) + 1
                    snippet = "\n".join(lines[s - 1 : e]) + "\n"
                    nid = stable_node_id(ctx.project_id, ctx.repo_fingerprint, rel, "function", sym or "", str(s), str(e))
                    nodes[nid] = CodeNodeRecord(
                        project_id=ctx.project_id,
                        repo_fingerprint=ctx.repo_fingerprint,
                        node_id=nid,
                        language="javascript",
                        kind="function",
                        symbol=sym,
                        file_path=rel,
                        start_line=s,
                        end_line=e,
                        snippet_hash=snippet_hash(file_path=rel, start_line=s, end_line=e, text=snippet),
                        text=snippet,
                    )
            except Exception:
                pass

            # Deterministic import extraction from source text.
            imports: list[tuple[str, float, dict]] = []
            for m in _RE_IMPORT_FROM.finditer(text):
                imports.append((m.group(1).strip(), 0.8, {"kind": "es_import"}))
            for m in _RE_REQUIRE.finditer(text):
                imports.append((m.group(1).strip(), 0.6, {"kind": "require"}))

            # De-dup + stable ordering
            uniq = {}
            for spec, conf, md in imports:
                uniq[(spec, tuple(sorted(md.items())))] = (spec, conf, md)
            for spec, conf, md in sorted(uniq.values(), key=lambda x: x[0]):
                target_id = stable_node_id(ctx.project_id, ctx.repo_fingerprint, rel, "unresolved_module", spec)
                nodes[target_id] = CodeNodeRecord(
                    project_id=ctx.project_id,
                    repo_fingerprint=ctx.repo_fingerprint,
                    node_id=target_id,
                    language="javascript",
                    kind="unresolved",
                    symbol=spec,
                    file_path=rel,
                    start_line=1,
                    end_line=len(lines),
                    snippet_hash=snippet_hash(file_path=rel, start_line=1, end_line=len(lines), text=""),
                    text="",
                )
                edges.append(
                    EdgeRecord(
                        project_id=ctx.project_id,
                        repo_fingerprint=ctx.repo_fingerprint,
                        rel_type="IMPORTS",
                        src_node_id=module_id,
                        dst_node_id=target_id,
                        confidence=conf,
                        metadata=md,
                    )
                )

        facts = {"js_file_count": len(files)}
        return list(nodes.values()), edges, facts

