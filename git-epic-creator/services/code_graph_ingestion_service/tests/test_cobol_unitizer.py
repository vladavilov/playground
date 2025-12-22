from plugins.cobol.normalizer import preprocess_cobol_bytes
from plugins.cobol.unitizer import unitize_cobol_file


def test_unitizer_produces_program_node_and_exec_block() -> None:
    src = (
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. HELLO.\n"
        "       PROCEDURE DIVISION.\n"
        "           EXEC SQL\n"
        "             SELECT 1\n"
        "           END-EXEC.\n"
        "           STOP RUN.\n"
    ).encode("utf-8")
    prep = preprocess_cobol_bytes(src)
    res = unitize_cobol_file(
        project_id="p",
        repo_fingerprint="r",
        file_path="x.cbl",
        preprocess=prep,
        max_error_ratio=0.02,
    )
    kinds = {n.kind for n in res.nodes}
    assert "program" in kinds
    assert "exec_block" in kinds


def test_unitizer_fallback_still_produces_program_node() -> None:
    # Intentionally malformed input that Tree-sitter might error on.
    src = b"THIS IS NOT COBOL!!!!\n"
    prep = preprocess_cobol_bytes(src)
    res = unitize_cobol_file(
        project_id="p",
        repo_fingerprint="r",
        file_path="bad.cbl",
        preprocess=prep,
        max_error_ratio=0.0,  # force permissive gate off
    )
    assert res.nodes and res.nodes[0].kind == "program"

