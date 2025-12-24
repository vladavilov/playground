use code_graph_ingestion_service::plugins::cobol::normalizer::preprocess_cobol_bytes;
use code_graph_ingestion_service::plugins::cobol::semantic_linker::link_cobol_semantics;
use code_graph_ingestion_service::core::types::CodeRelType;

#[test]
fn semantic_linker_extracts_paragraphs_and_resolves_perform() {
    let src = concat!(
        "       IDENTIFICATION DIVISION.\n",
        "       PROGRAM-ID. PGM1.\n",
        "       PROCEDURE DIVISION.\n",
        "       1000-INIT.\n",
        "           PERFORM 2000-WORK.\n",
        "           STOP RUN.\n",
        "       2000-WORK.\n",
        "           EXIT.\n",
    )
    .as_bytes()
    .to_vec();

    let prep = preprocess_cobol_bytes(&src);
    let res = link_cobol_semantics("p", "r", "x.cbl", &prep, "PROG");

    let mut para_names: Vec<String> = res
        .nodes
        .iter()
        .filter(|n| n.kind == "paragraph")
        .filter_map(|n| n.symbol.clone())
        .collect();
    para_names.sort();
    assert!(para_names.contains(&"1000-INIT".to_string()));
    assert!(para_names.contains(&"2000-WORK".to_string()));

    assert!(res
        .edges
        .iter()
        .any(|e| e.rel_type == CodeRelType::Performs && (e.confidence - 1.0).abs() < 1e-9));
}

#[test]
fn semantic_linker_resolves_perform_thru_metadata_when_unique() {
    let src = concat!(
        "       IDENTIFICATION DIVISION.\n",
        "       PROGRAM-ID. PGM1.\n",
        "       PROCEDURE DIVISION.\n",
        "       1000-START.\n",
        "           PERFORM 2000-WORK THRU 3000-EXIT.\n",
        "           STOP RUN.\n",
        "       2000-WORK.\n",
        "           CONTINUE.\n",
        "       3000-EXIT.\n",
        "           EXIT.\n",
    )
    .as_bytes()
    .to_vec();

    let prep = preprocess_cobol_bytes(&src);
    let res = link_cobol_semantics("p", "r", "x.cbl", &prep, "PROG");

    let thru_id = res
        .nodes
        .iter()
        .find(|n| n.kind == "paragraph" && n.symbol.as_deref() == Some("3000-EXIT"))
        .map(|n| n.node_id.clone())
        .expect("3000-EXIT paragraph node");

    let performs = res
        .edges
        .iter()
        .find(|e| e.rel_type == CodeRelType::Performs && (e.confidence - 1.0).abs() < 1e-9)
        .expect("resolved PERFORMS edge");

    assert_eq!(
        performs.metadata.get("thru").and_then(|v| v.as_str()),
        Some("3000-EXIT"),
        "expected thru raw metadata, got: {:?}",
        performs.metadata
    );
    assert_eq!(performs.metadata.get("thru_kind").and_then(|v| v.as_str()), Some("paragraph"));
    assert_eq!(
        performs.metadata.get("thru_node_id").and_then(|v| v.as_str()),
        Some(thru_id.as_str())
    );
}

#[test]
fn semantic_linker_resolves_qualified_data_reference() {
    let src = concat!(
        "       IDENTIFICATION DIVISION.\n",
        "       PROGRAM-ID. PGM1.\n",
        "       DATA DIVISION.\n",
        "       WORKING-STORAGE SECTION.\n",
        "       01 REC.\n",
        "          05 INVOICE.\n",
        "             10 AMOUNT.\n",
        "       PROCEDURE DIVISION.\n",
        "       1000-INIT.\n",
        "           MOVE AMOUNT OF INVOICE OF REC TO AMOUNT.\n",
        "           STOP RUN.\n",
    )
    .as_bytes()
    .to_vec();

    let prep = preprocess_cobol_bytes(&src);
    let res = link_cobol_semantics("p", "r", "x.cbl", &prep, "PROG");

    assert!(res.nodes.iter().any(|n| {
        n.kind == "data_def"
            && n.symbol
                .as_deref()
                .unwrap_or("")
                .starts_with("WORKING-STORAGE:AMOUNT OF INVOICE OF REC")
    }));
    assert!(res
        .edges
        .iter()
        .any(|e| e.rel_type == CodeRelType::References && (e.confidence - 1.0).abs() < 1e-9));
}

#[test]
fn semantic_linker_extracts_reads_writes_and_exec_sql_cics_edges() {
    let src = concat!(
        "       IDENTIFICATION DIVISION.\n",
        "       PROGRAM-ID. PGM1.\n",
        "       PROCEDURE DIVISION.\n",
        "       1000-INIT.\n",
        "           READ INFILE.\n",
        "           WRITE OUTREC.\n",
        "           EXEC SQL\n",
        "              SELECT NAME\n",
        "                INTO :WS-NAME\n",
        "                FROM CUSTOMER\n",
        "               WHERE ID = :WS-ID\n",
        "           END-EXEC.\n",
        "           EXEC SQL\n",
        "              UPDATE CUSTOMER\n",
        "                 SET NAME = :WS-NAME\n",
        "               WHERE ID = :WS-ID\n",
        "           END-EXEC.\n",
        "           EXEC SQL\n",
        "              DELETE FROM CUSTOMER WHERE ID = :WS-ID\n",
        "           END-EXEC.\n",
        "           EXEC CICS LINK PROGRAM('FOO') END-EXEC.\n",
        "           STOP RUN.\n",
    )
    .as_bytes()
    .to_vec();

    let prep = preprocess_cobol_bytes(&src);
    let res = link_cobol_semantics("p", "r", "x.cbl", &prep, "PROG");

    assert!(res.edges.iter().any(|e| e.rel_type == CodeRelType::Reads));
    assert!(res.edges.iter().any(|e| e.rel_type == CodeRelType::Writes));
    assert!(res.edges.iter().any(|e| e.rel_type == CodeRelType::Calls));

    assert!(res.nodes.iter().any(|n| n.kind == "sql_table"
        && n.symbol.as_deref().unwrap_or("") == "CUSTOMER"));
    assert!(res.nodes.iter().any(|n| n.kind == "unresolved"
        && n.symbol.as_deref().unwrap_or("").starts_with("CICS:")));
}


