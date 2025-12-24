use code_graph_ingestion_service::plugins::tsg::extract_cobol;

#[test]
fn tsg_extracts_perform_thru_second_label() {
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
    );

    let ex = extract_cobol(src).expect("extract");
    assert!(
        ex.performs.iter().any(|p| p.target == "2000-WORK" && p.thru.as_deref() == Some("3000-EXIT")),
        "expected PERFORM THRU hit, got: {:?}",
        ex.performs
    );
}


