//! Deterministic Tree-sitter COBOL language binding.
//!
//! This crate compiles the committed/generated `parser.c` / `scanner.c` that are
//! vendored in `services/tree_sitter_cobol_binding/src/libs/tree-sitter-cobol/`.

use tree_sitter_language::LanguageFn;

extern "C" {
    fn tree_sitter_COBOL() -> *const ();
}

/// Tree-sitter language function handle for COBOL.
///
/// Usage:
/// ```
/// let mut parser = tree_sitter::Parser::new();
/// parser.set_language(&tree_sitter_cobol::LANGUAGE.into()).unwrap();
/// ```
pub const LANGUAGE: LanguageFn = unsafe { LanguageFn::from_raw(tree_sitter_COBOL) };

/// Back-compat helper; returns a `tree_sitter::Language`.
pub fn language() -> tree_sitter::Language {
    LANGUAGE.into()
}

/// Pinned grammar revision (matches the Python wheel metadata).
pub const GRAMMAR_REVISION: &str = "8ba6692cc3c2bded0693d198936c6e26e6501230";


