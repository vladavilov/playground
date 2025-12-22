"""COBOL plugin integration (Task 15)."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

from config import get_code_graph_ingestion_settings
from core.records import CodeNodeRecord, EdgeRecord
from plugins.base import IngestionContext
from plugins.cobol.copybooks import expand_copybooks
from plugins.cobol.edges import extract_cobol_edges
from plugins.cobol.normalizer import preprocess_cobol_bytes
from plugins.cobol.unitizer import unitize_cobol_file


@dataclass(frozen=True)
class CobolPlugin:
    name: str = "cobol"
    file_globs: tuple[str, ...] = (".cbl", ".cob", ".cpy")

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
        settings = get_code_graph_ingestion_settings()

        all_nodes: dict[str, CodeNodeRecord] = {}
        all_edges: list[EdgeRecord] = []

        includes_edges: list[EdgeRecord] = []
        copybook_includes: list[str] = []

        for path in files:
            rel_path = path.relative_to(ctx.repo_root).as_posix()
            raw = path.read_bytes()

            prep = preprocess_cobol_bytes(raw)
            expanded = expand_copybooks(
                repo_root=ctx.repo_root,
                logical_lines=prep.logical_lines,
                logical_spans=prep.logical_spans,
                search_paths=[ctx.repo_root],
            )
            copybook_includes.extend(expanded.includes)

            # Rebuild preprocess object with expanded logical stream for parsing/unitization.
            prep2 = prep.__class__(
                physical_lines=prep.physical_lines,
                logical_lines=expanded.logical_lines,
                logical_spans=expanded.logical_spans,
                parse_bytes=("\n".join(expanded.logical_lines) + "\n").encode("utf-8", errors="replace"),
            )

            unit = unitize_cobol_file(
                project_id=ctx.project_id,
                repo_fingerprint=ctx.repo_fingerprint,
                file_path=rel_path,
                preprocess=prep2,
                max_error_ratio=settings.COBOL_TS_MAX_ERROR_RATIO,
            )

            for n in unit.nodes:
                all_nodes[n.node_id] = n

            # Choose program node as first program kind in this file.
            program_nodes = [n for n in unit.nodes if n.kind == "program" and n.file_path == rel_path]
            program_node_id = program_nodes[0].node_id if program_nodes else unit.nodes[0].node_id

            ed = extract_cobol_edges(
                project_id=ctx.project_id,
                repo_fingerprint=ctx.repo_fingerprint,
                file_path=rel_path,
                program_node_id=program_node_id,
                physical_lines=prep.physical_lines,
            )
            for n in ed.nodes:
                all_nodes[n.node_id] = n
            all_edges.extend(ed.edges)

            # Emit INCLUDES edges to copybook files (dst_node_id uses file_path).
            for inc in expanded.includes:
                includes_edges.append(
                    EdgeRecord(
                        project_id=ctx.project_id,
                        repo_fingerprint=ctx.repo_fingerprint,
                        rel_type="INCLUDES",
                        src_node_id=program_node_id,
                        dst_node_id=inc,
                        confidence=1.0,
                        metadata={},
                    )
                )

        all_edges.extend(includes_edges)

        facts = {
            "file_count": len(files),
            "includes_count": len(set(copybook_includes)),
        }
        return list(all_nodes.values()), all_edges, facts


