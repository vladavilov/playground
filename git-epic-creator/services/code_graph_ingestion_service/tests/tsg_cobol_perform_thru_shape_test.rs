use tree_sitter::{Node, Parser};

fn parse_cobol(source: &str) -> tree_sitter::Tree {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_cobol::LANGUAGE.into())
        .expect("set cobol language");
    parser.parse(source, None).expect("parse")
}

fn find_first<'a>(root: Node<'a>, kind: &str) -> Option<Node<'a>> {
    let mut stack = vec![root];
    while let Some(n) = stack.pop() {
        if n.kind() == kind {
            return Some(n);
        }
        let mut c = n.walk();
        for ch in n.children(&mut c) {
            stack.push(ch);
        }
    }
    None
}

#[test]
fn cobol_perform_procedure_child_shape_includes_thru_token() {
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
    let tree = parse_cobol(src);
    let proc = find_first(tree.root_node(), "perform_procedure").expect("perform_procedure present");

    let mut c = proc.walk();
    let kids: Vec<(String, bool)> = proc
        .children(&mut c)
        .map(|n| (n.kind().to_string(), n.is_named()))
        .collect();

    // We expect two label nodes and some token(s) in between (typically THRU).
    let label_named_count = kids
        .iter()
        .filter(|(k, named)| *named && k.as_str() == "label")
        .count();
    assert_eq!(
        label_named_count, 2,
        "expected 2 named label children, got: {:?}",
        kids
    );
    assert!(
        kids.iter().any(|(k, _)| k.eq_ignore_ascii_case("THRU")),
        "expected a THRU token child somewhere, got: {:?}",
        kids
    );
}


