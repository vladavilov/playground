"""Graph contract validation (Task 19).

Validates invariants before persistence to Neo4j/Postgres.
"""

from __future__ import annotations

from dataclasses import dataclass

from core.inventory import InventoryEntry
from core.records import CodeNodeRecord, EdgeRecord


@dataclass(frozen=True)
class GraphContractViolation(Exception):
    message: str

    def __str__(self) -> str:  # pragma: no cover
        return self.message


def validate_graph_contract(
    *,
    project_id: str,
    repo_fingerprint: str,
    files: list[InventoryEntry],
    nodes: list[CodeNodeRecord],
    edges: list[EdgeRecord],
) -> None:
    file_paths = {f.path for f in files}

    for n in nodes:
        if n.project_id != project_id:
            raise GraphContractViolation(f"Node project_id mismatch: {n.node_id}")
        if n.repo_fingerprint != repo_fingerprint:
            raise GraphContractViolation(f"Node repo_fingerprint mismatch: {n.node_id}")
        if not n.node_id:
            raise GraphContractViolation("Node node_id is empty")
        if not n.file_path:
            raise GraphContractViolation(f"Node file_path is empty: {n.node_id}")

    for e in edges:
        if e.project_id != project_id:
            raise GraphContractViolation(f"Edge project_id mismatch: {e.rel_type}")
        if e.repo_fingerprint != repo_fingerprint:
            raise GraphContractViolation(f"Edge repo_fingerprint mismatch: {e.rel_type}")
        if not (0.0 <= float(e.confidence) <= 1.0):
            raise GraphContractViolation(f"Edge confidence out of range: {e.rel_type}")
        if e.metadata is None:
            raise GraphContractViolation(f"Edge metadata must be a dict: {e.rel_type}")
        if e.rel_type == "INCLUDES" and e.dst_node_id not in file_paths:
            raise GraphContractViolation(f"INCLUDES dst must be file_path in inventory: {e.dst_node_id}")

    # Deterministic de-dup check
    node_ids = [n.node_id for n in nodes]
    if len(node_ids) != len(set(node_ids)):
        raise GraphContractViolation("Duplicate node_id detected")



