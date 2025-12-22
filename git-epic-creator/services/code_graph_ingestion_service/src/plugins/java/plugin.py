"""Java plugin integration (Task 16).

Includes XML wiring as part of the Java run (CGI-FR-009B / CGI-FR-016).
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

from core.records import CodeNodeRecord, EdgeRecord
from core.stable_ids import snippet_hash, stable_node_id
from plugins.base import IngestionContext
from plugins.java.unitizer import unitize_java_file
from plugins.xml.parser import extract_class_attributes


@dataclass(frozen=True)
class JavaPlugin:
    name: str = "java"
    file_globs: tuple[str, ...] = (".java", ".xml")

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

        java_files = [p for p in files if p.suffix.lower() == ".java"]
        xml_files = [p for p in files if p.suffix.lower() == ".xml"]

        # Unitize Java files.
        module_nodes: list[tuple[str, str]] = []  # (file_path, module_node_id)
        for p in java_files:
            rel = p.relative_to(ctx.repo_root).as_posix()
            res = unitize_java_file(
                project_id=ctx.project_id,
                repo_fingerprint=ctx.repo_fingerprint,
                file_path=rel,
                source_bytes=p.read_bytes(),
            )
            for n in res.nodes:
                nodes[n.node_id] = n
            module_nodes.append((rel, res.module_node_id))

        # XML wiring nodes + CONFIG_WIRES edges.
        for p in xml_files:
            rel = p.relative_to(ctx.repo_root).as_posix()
            xml_text = p.read_text(encoding="utf-8", errors="replace").replace("\r\n", "\n").replace("\r", "\n")
            lines = xml_text.split("\n")
            if lines and lines[-1] == "":
                lines = lines[:-1]
            xml_node_id = stable_node_id(ctx.project_id, ctx.repo_fingerprint, rel, "xml_config", "", "1", str(len(lines)))
            xml_hash = snippet_hash(file_path=rel, start_line=1, end_line=len(lines), text="\n".join(lines) + "\n")
            xml_node = CodeNodeRecord(
                project_id=ctx.project_id,
                repo_fingerprint=ctx.repo_fingerprint,
                node_id=xml_node_id,
                language="xml",
                kind="xml_config",
                symbol=None,
                file_path=rel,
                start_line=1,
                end_line=len(lines),
                snippet_hash=xml_hash,
                text="\n".join(lines) + "\n",
            )
            nodes[xml_node.node_id] = xml_node

            wiring = extract_class_attributes(xml_text)
            for cls in wiring.class_names:
                target_id = stable_node_id(ctx.project_id, ctx.repo_fingerprint, rel, "unresolved_java_class", cls)
                target_node = CodeNodeRecord(
                    project_id=ctx.project_id,
                    repo_fingerprint=ctx.repo_fingerprint,
                    node_id=target_id,
                    language="java",
                    kind="unresolved",
                    symbol=cls,
                    file_path=rel,
                    start_line=1,
                    end_line=len(lines),
                    snippet_hash=snippet_hash(file_path=rel, start_line=1, end_line=len(lines), text=""),
                    text="",
                    extra_labels=("__UnresolvedCall__",),
                )
                nodes[target_node.node_id] = target_node
                edges.append(
                    EdgeRecord(
                        project_id=ctx.project_id,
                        repo_fingerprint=ctx.repo_fingerprint,
                        rel_type="CONFIG_WIRES",
                        src_node_id=xml_node_id,
                        dst_node_id=target_id,
                        confidence=0.6,
                        metadata={"class": cls},
                    )
                )

        facts = {
            "java_file_count": len(java_files),
            "xml_file_count": len(xml_files),
        }
        return list(nodes.values()), edges, facts



