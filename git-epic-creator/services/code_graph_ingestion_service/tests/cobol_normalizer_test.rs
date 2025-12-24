use code_graph_ingestion_service::plugins::cobol::normalizer::preprocess_cobol_bytes;

#[test]
fn fixed_format_continuation_and_comment_handling() {
    let src = concat!(
        "000100* THIS IS COMMENT\n",
        "000200 IDENTIFICATION DIVISION.\n",
        "000300 PROGRAM-ID. HELLO.\n",
        "000400-    CONTINUED-TEXT\n",
        "000500 PROCEDURE DIVISION.\n"
    )
    .as_bytes()
    .to_vec();

    let res = preprocess_cobol_bytes(&src);
    assert!(res.physical_lines[0].starts_with("000100*"));
    // First logical line is placeholder for comment
    assert_eq!(res.logical_lines[0], "");
    // Continuation merges into previous logical line span
    assert!(res.logical_lines.iter().any(|ln| ln.contains("PROGRAM-ID")));
    assert!(res
        .logical_spans
        .iter()
        .any(|(s, e)| *s <= 3 && *e >= 4));
}

#[test]
fn exec_block_is_collapsed() {
    let src = concat!(
        "       IDENTIFICATION DIVISION.\n",
        "       PROGRAM-ID. X.\n",
        "       PROCEDURE DIVISION.\n",
        "           EXEC SQL\n",
        "             SELECT 1\n",
        "           END-EXEC.\n",
        "           STOP RUN.\n"
    )
    .as_bytes()
    .to_vec();

    let res = preprocess_cobol_bytes(&src);
    assert!(res.logical_lines.iter().any(|ln| ln == "EXEC_BLOCK."));
    let idx = res
        .logical_lines
        .iter()
        .position(|ln| ln == "EXEC_BLOCK.")
        .unwrap();
    let (s, e) = res.logical_spans[idx];
    assert!(s <= 4);
    assert!(e >= 6);
}


