use code_graph_ingestion_service::plugins::cobol::copybooks::expand_copybooks;
use code_graph_ingestion_service::plugins::cobol::normalizer::{preprocess_cobol_bytes, PreprocessResult};
use code_graph_ingestion_service::plugins::cobol::unitizer::unitize_cobol_file;

#[test]
fn unitizer_emits_sentence_statement_data_item_and_copybook_nodes() {
    let tmp = tempfile::tempdir().unwrap();
    let repo = tmp.path().join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::write(repo.join("CB1.cpy"), "01 CB-REC.\n  05 CB-FIELD PIC 9.\n").unwrap();

    let src = concat!(
        "       IDENTIFICATION DIVISION.\n",
        "       PROGRAM-ID. PGM1.\n",
        "       DATA DIVISION.\n",
        "       WORKING-STORAGE SECTION.\n",
        "       01 WS-REC.\n",
        "          05 WS-FIELD PIC 9.\n",
        "       COPY CB1.\n",
        "       PROCEDURE DIVISION.\n",
        "       1000-INIT.\n",
        "           MOVE WS-FIELD TO WS-FIELD.\n",
        "           CALL \"CALLEE\".\n",
        "           STOP RUN.\n",
    )
    .as_bytes()
    .to_vec();

    let prep = preprocess_cobol_bytes(&src);

    // Simulate the plugin’s expand-copybooks step so unitizer can emit copybook nodes via provenance.
    let expanded = expand_copybooks(&repo, &prep.logical_lines, &prep.logical_spans, Some(&[repo.clone()])).unwrap();
    let prep2 = PreprocessResult {
        physical_lines: prep.physical_lines,
        logical_lines: expanded.logical_lines.clone(),
        logical_spans: expanded.logical_spans.clone(),
        parse_bytes: (expanded.logical_lines.join("\n") + "\n").into_bytes(),
    };

    let res = unitize_cobol_file("p", "r", "x.cbl", &prep2, &expanded.expansion_map, 0.02);

    assert!(res.nodes.iter().any(|n| n.kind == "statement"));
    assert!(res.nodes.iter().any(|n| n.kind == "sentence"));
    assert!(res.nodes.iter().any(|n| n.kind == "data_item"));
    assert!(res.nodes.iter().any(|n| n.kind == "copybook" && n.symbol.as_deref() == Some("CB1.cpy")));
}


