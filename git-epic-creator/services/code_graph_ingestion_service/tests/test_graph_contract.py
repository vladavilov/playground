import pytest

from core.graph_contract import GraphContractViolation, validate_graph_contract
from core.inventory import InventoryEntry
from core.records import CodeNodeRecord, EdgeRecord


def test_graph_contract_rejects_includes_to_missing_file() -> None:
    files = [InventoryEntry(path="a.cpy", sha256="x", language="cobol", line_count=1)]
    nodes = [
        CodeNodeRecord(
            project_id="p",
            repo_fingerprint="r",
            node_id="n1",
            language="cobol",
            kind="program",
            symbol=None,
            file_path="a.cbl",
            start_line=1,
            end_line=1,
            snippet_hash="h",
            text="x\n",
        )
    ]
    edges = [
        EdgeRecord(
            project_id="p",
            repo_fingerprint="r",
            rel_type="INCLUDES",
            src_node_id="n1",
            dst_node_id="missing.cpy",
            confidence=1.0,
            metadata={},
        )
    ]
    with pytest.raises(GraphContractViolation):
        validate_graph_contract(project_id="p", repo_fingerprint="r", files=files, nodes=nodes, edges=edges)



