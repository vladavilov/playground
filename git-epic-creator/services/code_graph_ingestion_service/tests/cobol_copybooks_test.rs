use code_graph_ingestion_service::plugins::cobol::copybooks::expand_copybooks;

#[test]
fn copybook_expansion_inlines_content_and_records_includes() {
    let tmp = tempfile::tempdir().unwrap();
    let repo = tmp.path().join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::write(repo.join("FOO.cpy"), "01 X PIC 9.\n").unwrap();

    let logical = vec!["       COPY FOO.".to_string(), "       STOP RUN.".to_string()];
    let spans = vec![(1, 1), (2, 2)];
    let res = expand_copybooks(&repo, &logical, &spans, None).unwrap();

    assert!(res.logical_lines.join("\n").contains("01 X PIC 9."));
    assert!(res.includes.iter().any(|p| p == "FOO.cpy"));
    assert!(!res.expansion_map.is_empty());
    assert_eq!(res.expansion_map[0].copybook_path, "FOO.cpy");
}

#[test]
fn copy_replacing_basic_substitution() {
    let tmp = tempfile::tempdir().unwrap();
    let repo = tmp.path().join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::write(repo.join("BAR.cpy"), "MOVE A TO B.\n").unwrap();

    let logical = vec![
        "COPY BAR REPLACING ==A== BY ==X==.".to_string(),
        "STOP RUN.".to_string(),
    ];
    let spans = vec![(1, 1), (2, 2)];
    let res = expand_copybooks(&repo, &logical, &spans, None).unwrap();

    let joined = res.logical_lines.join("\n");
    assert!(joined.contains("MOVE X TO B."));
}


