use code_graph_ingestion_service::core::ignore_rules::build_ignore_rules;
use code_graph_ingestion_service::core::inventory::build_inventory;
use code_graph_ingestion_service::core::types::CodeLanguage;

#[test]
fn inventory_is_deterministic_and_ignores_git() {
    let tmp = tempfile::tempdir().unwrap();
    let repo = tmp.path().join("repo");
    std::fs::create_dir_all(repo.join(".git")).unwrap();
    std::fs::write(repo.join(".git").join("config"), "x").unwrap();
    std::fs::write(repo.join("a.cbl"), "       IDENTIFICATION DIVISION.\n").unwrap();
    std::fs::write(repo.join("b.java"), "class B {}\n").unwrap();
    std::fs::create_dir_all(repo.join("node_modules")).unwrap();
    std::fs::write(repo.join("node_modules").join("x.js"), "export {}\n").unwrap();

    let ignore = build_ignore_rules(&repo, None).unwrap();
    let inv1 = build_inventory(&repo, Some(&ignore)).unwrap();
    let inv2 = build_inventory(&repo, Some(&ignore)).unwrap();

    assert_eq!(
        inv1.iter().map(|e| &e.path).collect::<Vec<_>>(),
        inv2.iter().map(|e| &e.path).collect::<Vec<_>>()
    );
    assert!(inv1.iter().all(|e| !e.path.starts_with(".git/")));
    assert!(inv1.iter().all(|e| !e.path.starts_with("node_modules/")));

    let mut by_path = std::collections::HashMap::<String, _>::new();
    for e in inv1 {
        by_path.insert(e.path.clone(), e);
    }
    assert_eq!(by_path.get("a.cbl").unwrap().language, CodeLanguage::Cobol);
    assert_eq!(by_path.get("b.java").unwrap().language, CodeLanguage::Java);
}

#[test]
fn gitignore_file_is_respected() {
    let tmp = tempfile::tempdir().unwrap();
    let repo = tmp.path().join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::write(repo.join(".gitignore"), "ignored.txt\n").unwrap();
    std::fs::write(repo.join("ignored.txt"), "nope\n").unwrap();
    std::fs::write(repo.join("kept.txt"), "yep\n").unwrap();

    let inv = build_inventory(&repo, None).unwrap();
    let paths = inv.into_iter().map(|e| e.path).collect::<Vec<_>>();
    assert!(paths.contains(&"kept.txt".to_string()));
    assert!(!paths.contains(&"ignored.txt".to_string()));
}


