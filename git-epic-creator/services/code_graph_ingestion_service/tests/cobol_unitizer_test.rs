use code_graph_ingestion_service::plugins::cobol::normalizer::preprocess_cobol_bytes;
use code_graph_ingestion_service::plugins::cobol::unitizer::unitize_cobol_file;

#[test]
fn unitizer_produces_program_node_and_exec_block() {
    let src = concat!(
        "       IDENTIFICATION DIVISION.\n",
        "       PROGRAM-ID. HELLO.\n",
        "       PROCEDURE DIVISION.\n",
        "           EXEC SQL\n",
        "             SELECT 1\n",
        "           END-EXEC.\n",
        "           STOP RUN.\n",
    )
    .as_bytes()
    .to_vec();

    let prep = preprocess_cobol_bytes(&src);
    let res = unitize_cobol_file("p", "r", "x.cbl", &prep, &[], 0.02);
    let kinds: std::collections::HashSet<String> = res.nodes.iter().map(|n| n.kind.clone()).collect();
    assert!(kinds.contains("program"));
    assert!(kinds.contains("exec_block"));
}

#[test]
fn unitizer_fallback_still_produces_program_node() {
    let src = b"THIS IS NOT COBOL!!!!\n".to_vec();
    let prep = preprocess_cobol_bytes(&src);
    let res = unitize_cobol_file("p", "r", "bad.cbl", &prep, &[], 0.0);
    assert!(!res.nodes.is_empty());
    assert_eq!(res.nodes[0].kind, "program");
}


