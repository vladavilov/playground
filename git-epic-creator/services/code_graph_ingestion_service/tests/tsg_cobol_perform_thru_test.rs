use code_graph_ingestion_service::plugins::cobol::normalizer::preprocess_cobol_bytes;
use code_graph_ingestion_service::plugins::tsg;

#[test]
fn tsg_extract_cobol_captures_perform_thru() {
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

    // Match the ingestion pipeline behavior: run the COBOL normalizer and feed the parse stream.
    let prep = preprocess_cobol_bytes(src.as_bytes());
    let parse_stream = String::from_utf8_lossy(&prep.parse_bytes).to_string();
    let ex = tsg::extract_cobol(&parse_stream).unwrap();
    let hit = ex
        .performs
        .iter()
        .find(|p| p.target == "2000-WORK")
        .expect("perform target captured");
    assert_eq!(hit.thru.as_deref(), Some("3000-EXIT"));
}


