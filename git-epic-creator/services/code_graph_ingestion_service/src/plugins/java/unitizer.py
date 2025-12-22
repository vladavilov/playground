"""Java unitization via Tree-sitter (Task 16).

Minimal extraction of:
- class declarations
- method declarations
"""

from __future__ import annotations

from dataclasses import dataclass

from tree_sitter_language_pack import get_language, get_parser
from tree_sitter import Query, QueryCursor  # type: ignore

from core.records import CodeNodeRecord
from core.stable_ids import snippet_hash, stable_node_id


@dataclass(frozen=True)
class JavaUnitizeResult:
    nodes: list[CodeNodeRecord]
    module_node_id: str


def unitize_java_file(
    *,
    project_id: str,
    repo_fingerprint: str,
    file_path: str,
    source_bytes: bytes,
) -> JavaUnitizeResult:
    text = source_bytes.decode("utf-8", errors="replace").replace("\r\n", "\n").replace("\r", "\n")
    lines = text.split("\n")
    if lines and lines[-1] == "":
        lines = lines[:-1]

    # Module node (file-level container for edges like IMPORTS).
    module_id = stable_node_id(project_id, repo_fingerprint, file_path, "module", "", "1", str(len(lines)))
    module_text = "\n".join(lines) + "\n"
    module_hash = snippet_hash(file_path=file_path, start_line=1, end_line=len(lines), text=module_text)
    nodes: list[CodeNodeRecord] = [
        CodeNodeRecord(
            project_id=project_id,
            repo_fingerprint=repo_fingerprint,
            node_id=module_id,
            language="java",
            kind="module",
            symbol=None,
            file_path=file_path,
            start_line=1,
            end_line=len(lines),
            snippet_hash=module_hash,
            text=module_text,
        )
    ]

    parser = get_parser("java")
    lang = get_language("java")
    tree = parser.parse(source_bytes)

    q = Query(
        lang,
        """
        (class_declaration) @class
        (method_declaration) @method
        """
    )
    cur = QueryCursor(q)
    caps = cur.captures(tree.root_node)

    for kind, cap_name in (("class", "class"), ("method", "method")):
        for node in caps.get(cap_name, []):
            start_line = int(node.start_point[0]) + 1
            end_line = int(node.end_point[0]) + 1
            if end_line < start_line:
                end_line = start_line

            symbol = None
            name_node = getattr(node, "child_by_field_name", lambda *_: None)("name")
            if name_node is not None:
                symbol = name_node.text.decode("utf-8", errors="replace")

            snippet = "\n".join(lines[start_line - 1 : end_line]) + "\n"
            nid = stable_node_id(
                project_id, repo_fingerprint, file_path, kind, symbol or "", str(start_line), str(end_line)
            )
            sh = snippet_hash(file_path=file_path, start_line=start_line, end_line=end_line, text=snippet)
            nodes.append(
                CodeNodeRecord(
                    project_id=project_id,
                    repo_fingerprint=repo_fingerprint,
                    node_id=nid,
                    language="java",
                    kind=kind,
                    symbol=symbol,
                    file_path=file_path,
                    start_line=start_line,
                    end_line=end_line,
                    snippet_hash=sh,
                    text=snippet,
                )
            )

    # Deterministic de-dup by node_id
    uniq = {n.node_id: n for n in nodes}
    return JavaUnitizeResult(nodes=list(uniq.values()), module_node_id=module_id)

