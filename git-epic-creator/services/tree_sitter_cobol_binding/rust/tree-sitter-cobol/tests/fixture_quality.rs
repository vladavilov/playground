use std::path::Path;

fn main_fixture_root() -> &'static str {
    "../../../tests/fixtures"
}

fn count_error_nodes(root: tree_sitter::Node<'_>) -> (usize, usize) {
    // Returns (total_nodes, error_nodes)
    let mut total = 0usize;
    let mut errors = 0usize;

    fn walk(n: tree_sitter::Node<'_>, total: &mut usize, errors: &mut usize) {
        *total += 1;
        if n.is_error() {
            *errors += 1;
        }
        let mut c = n.walk();
        for ch in n.children(&mut c) {
            walk(ch, total, errors);
        }
    }

    walk(root, &mut total, &mut errors);
    (total, errors)
}

fn parse(src: &str) -> tree_sitter::Tree {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_cobol::LANGUAGE.into())
        .expect("load cobol language");
    parser.parse(src, None).expect("parse")
}

fn read_to_string(p: &Path) -> String {
    let bytes = std::fs::read(p).expect("read fixture");
    String::from_utf8_lossy(&bytes).replace("\r\n", "\n").replace('\r', "\n")
}

#[test]
fn fixtures_cobol_core_are_strict() {
    let root = Path::new(main_fixture_root()).join("cobol_core");
    for ent in walkdir::WalkDir::new(&root).into_iter().filter_map(Result::ok) {
        if !ent.file_type().is_file() {
            continue;
        }
        if ent.path().extension().is_none_or(|e| e.to_string_lossy().to_ascii_lowercase() != "cbl") {
            continue;
        }
        let src = read_to_string(ent.path());
        let tree = parse(&src);
        assert!(
            !tree.root_node().has_error(),
            "expected strict parse for fixture: {}",
            ent.path().display()
        );
    }
}

#[test]
fn fixtures_other_dialects_meet_error_ratio_threshold() {
    // Match the previous Python default: 2% max error-node ratio.
    let max_ratio: f64 = std::env::var("COBOL_TS_MAX_ERROR_RATIO")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(0.02);

    let root = Path::new(main_fixture_root());
    for dialect in ["gnucobol", "ibm_enterprise", "micro_focus"] {
        let dir = root.join(dialect);
        if !dir.exists() {
            continue;
        }
        let mut totals = 0usize;
        let mut errors = 0usize;
        let mut files = 0usize;

        for ent in walkdir::WalkDir::new(&dir).into_iter().filter_map(Result::ok) {
            if !ent.file_type().is_file() {
                continue;
            }
            if ent.path().extension().is_none_or(|e| e.to_string_lossy().to_ascii_lowercase() != "cbl") {
                continue;
            }
            files += 1;
            let src = read_to_string(ent.path());
            let tree = parse(&src);
            let (t, e) = count_error_nodes(tree.root_node());
            totals += t;
            errors += e;
        }

        if files == 0 || totals == 0 {
            continue;
        }
        let ratio = (errors as f64) / (totals as f64);
        assert!(
            ratio <= max_ratio,
            "dialect={dialect} error_ratio={ratio:.4} > max={max_ratio:.4} (errors={errors}, total={totals})"
        );
    }
}

