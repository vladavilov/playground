use code_graph_ingestion_service::core::workspace::Workspace;

#[test]
fn workspace_paths_are_deterministic() {
    let tmp = tempfile::tempdir().unwrap();
    let ws = Workspace::new(tmp.path().join("ws"));

    let p1 = ws.zip_repo_dir("00000000-0000-0000-0000-000000000000");
    let p2 = ws.zip_repo_dir("00000000-0000-0000-0000-000000000000");
    assert_eq!(p1, p2);

    let g1 = ws.git_repo_dir("p", "https://example.com/repo.git", Some("main"));
    let g2 = ws.git_repo_dir("p", "https://example.com/repo.git", Some("main"));
    assert_eq!(g1, g2);
}


