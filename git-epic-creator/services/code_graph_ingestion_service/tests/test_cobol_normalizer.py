from plugins.cobol.normalizer import preprocess_cobol_bytes


def test_fixed_format_continuation_and_comment_handling() -> None:
    # Column 7 indicator:
    # - '*' comment
    # - '-' continuation
    src = (
        "000100* THIS IS COMMENT\n"
        "000200 IDENTIFICATION DIVISION.\n"
        "000300 PROGRAM-ID. HELLO.\n"
        "000400-    CONTINUED-TEXT\n"
        "000500 PROCEDURE DIVISION.\n"
    ).encode("utf-8")

    res = preprocess_cobol_bytes(src)
    assert res.physical_lines[0].startswith("000100*")
    # First logical line is placeholder for comment
    assert res.logical_lines[0] == ""
    # Continuation merges into previous logical line span
    assert any("PROGRAM-ID" in ln for ln in res.logical_lines)
    # The continuation merged line should span physical lines 3-4 or 4 as part of it.
    assert any(span[0] <= 3 and span[1] >= 4 for span in res.logical_spans)


def test_exec_block_is_collapsed() -> None:
    src = (
        "       IDENTIFICATION DIVISION.\n"
        "       PROGRAM-ID. X.\n"
        "       PROCEDURE DIVISION.\n"
        "           EXEC SQL\n"
        "             SELECT 1\n"
        "           END-EXEC.\n"
        "           STOP RUN.\n"
    ).encode("utf-8")

    res = preprocess_cobol_bytes(src)
    assert "EXEC_BLOCK." in res.logical_lines
    idx = res.logical_lines.index("EXEC_BLOCK.")
    assert res.logical_spans[idx][0] <= 4
    assert res.logical_spans[idx][1] >= 6


