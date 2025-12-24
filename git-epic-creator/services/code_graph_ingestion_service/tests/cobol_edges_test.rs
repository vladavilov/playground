use code_graph_ingestion_service::plugins::cobol::edges::extract_cobol_edges;
use code_graph_ingestion_service::core::types::CodeRelType;

#[test]
fn edges_extracts_literal_and_dynamic_calls_and_perform() {
    let lines = vec![
        "       IDENTIFICATION DIVISION.".to_string(),
        "       PROGRAM-ID. HELLO.".to_string(),
        "       CALL \"FOO\".".to_string(),
        "       CALL WS-PGM.".to_string(),
        "       PERFORM INIT-ROUTINE.".to_string(),
        "       READ INFILE.".to_string(),
        "       WRITE OUTREC.".to_string(),
        "       EXEC SQL".to_string(),
        "          SELECT NAME".to_string(),
        "            INTO :WS-NAME".to_string(),
        "            FROM CUSTOMER".to_string(),
        "           WHERE ID = :WS-ID".to_string(),
        "       END-EXEC.".to_string(),
    ];

    let res = extract_cobol_edges("p", "r", "x.cbl", "prog", &lines);
    let rels: std::collections::HashSet<CodeRelType> = res.edges.iter().map(|e| e.rel_type).collect();
    assert!(rels.contains(&CodeRelType::Calls));
    assert!(rels.contains(&CodeRelType::Performs));
    assert!(rels.contains(&CodeRelType::Reads));
    assert!(rels.contains(&CodeRelType::Writes));
    assert!(res.nodes.iter().any(|n| n.kind == "unresolved"));
    assert!(res
        .nodes
        .iter()
        .any(|n| n.extra_labels.iter().any(|l| l == "__UnresolvedCall__")));
    assert!(res
        .nodes
        .iter()
        .any(|n| n.extra_labels.iter().any(|l| l == "__UnresolvedFile__")));
    assert!(res
        .nodes
        .iter()
        .any(|n| n.extra_labels.iter().any(|l| l == "__UnresolvedRecord__")));
    assert!(res
        .nodes
        .iter()
        .any(|n| n.extra_labels.iter().any(|l| l == "__ExecSqlTable__")));
}


