#[test]
fn parses_cobol_core_hello_without_errors() {
    let src = include_str!("../../../tests/fixtures/cobol_core/hello.cbl");

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_cobol::LANGUAGE.into())
        .expect("load cobol language");

    let tree = parser.parse(src, None).expect("parse");
    assert!(
        !tree.root_node().has_error(),
        "expected no parse errors in cobol_core fixture"
    );
}

#[test]
fn call_statement_exposes_target_via_field_x() {
    let src = "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. HELLO.\n       PROCEDURE DIVISION.\n           CALL \"FOO\".\n";

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_cobol::LANGUAGE.into())
        .expect("load cobol language");
    let tree = parser.parse(src, None).expect("parse");

    fn find_call<'a>(n: tree_sitter::Node<'a>) -> Option<tree_sitter::Node<'a>> {
        if n.kind() == "call_statement" {
            return Some(n);
        }
        let mut c = n.walk();
        for ch in n.children(&mut c) {
            if let Some(found) = find_call(ch) {
                return Some(found);
            }
        }
        None
    }

    let call = find_call(tree.root_node()).expect("call_statement present");
    let x = call.child_by_field_name("x").expect("field x present");
    assert!(
        matches!(x.kind(), "string" | "qualified_word" | "function_"),
        "unexpected call target kind: {}",
        x.kind()
    );
}


