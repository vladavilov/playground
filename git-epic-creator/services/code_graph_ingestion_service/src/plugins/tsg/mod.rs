use std::sync::OnceLock;

use tree_sitter::Parser;
use tree_sitter_graph::ast::File as TsgFile;
use tree_sitter_graph::Match as TsgMatch;
use tree_sitter_language::LanguageFn;

/// COBOL language handle (re-exported for convenience across the service/tests).
pub const COBOL_LANGUAGE: LanguageFn = tree_sitter_cobol::LANGUAGE;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportHit {
    pub specifier: String,
    pub kind: &'static str, // "es_import" | "export_from" | "require" | "dynamic_import" | "java_import"
}

#[derive(Debug, Clone)]
pub struct JavaExtract {
    pub package: Option<String>,
    pub imports: Vec<String>,
    pub classes: Vec<SpanDecl>,
    pub methods: Vec<SpanDecl>,
    pub call_sites: Vec<CallSite>,
}

#[derive(Debug, Clone)]
pub struct JsExtract {
    pub imports: Vec<ImportHit>,
}

#[derive(Debug, Clone)]
pub struct SpanDecl {
    pub name: String,
    pub start_line: i64,
    pub end_line: i64,
}

#[derive(Debug, Clone)]
pub struct CallSite {
    pub name: String,
    pub start_line: i64,
}

const JS_DSL: &str = r#"
  (import_statement
    source: (string) @js_import_source) @js_import
  {
    node n
    attr (n) match = @js_import, source = @js_import_source
  }

  (export_statement
    source: (string) @js_export_source) @js_export
  {
    node n
    attr (n) match = @js_export, source = @js_export_source
  }

  (call_expression
    function: [(identifier) (import)] @js_call_func
    arguments: (arguments (string) @js_call_arg)) @js_call
  {
    node n
    attr (n) match = @js_call, func = @js_call_func, arg = @js_call_arg
  }
"#;

const JAVA_DSL: &str = r#"
  (package_declaration
    (scoped_identifier) @java_package) @java_package_decl
  {
    node n
    attr (n) match = @java_package_decl, pkg = @java_package
  }

  (package_declaration
    (identifier) @java_package_ident) @java_package_decl2
  {
    node n
    attr (n) match = @java_package_decl2, pkg = @java_package_ident
  }

  (import_declaration) @java_import
  {
    node n
    attr (n) imp = @java_import
  }

  (class_declaration
    name: (identifier) @java_class_name) @java_class
  {
    node n
    attr (n) match = @java_class, name = @java_class_name
  }

  (method_declaration
    name: (identifier) @java_method_name) @java_method
  {
    node n
    attr (n) match = @java_method, name = @java_method_name
  }

  (method_invocation
    name: (identifier) @java_call_name) @java_call
  {
    node n
    attr (n) match = @java_call, name = @java_call_name
  }
"#;

const COBOL_DSL: &str = r#"
  (call_statement
    x: (_) @cobol_call_target) @cobol_call
  {
    node n
    attr (n) match = @cobol_call, target = @cobol_call_target
  }

  (perform_statement_call_proc
    procedure: (perform_procedure
      (label) @cobol_perform_target
      (THRU)
      (label) @cobol_perform_thru)) @cobol_perform
  {
    node n
    attr (n) match = @cobol_perform, target = @cobol_perform_target, thru = @cobol_perform_thru
  }

  (perform_statement_call_proc
    procedure: (perform_procedure
      (label) @cobol_perform_target)) @cobol_perform
  {
    node n
    attr (n) match = @cobol_perform, target = @cobol_perform_target
  }

  (read_statement
    file_name: (WORD) @cobol_read_file) @cobol_read
  {
    node n
    attr (n) match = @cobol_read, file = @cobol_read_file
  }

  (write_statement
    record_name: (qualified_word) @cobol_write_record) @cobol_write
  {
    node n
    attr (n) match = @cobol_write, record = @cobol_write_record
  }

  (rewrite_statement
    record: (qualified_word) @cobol_rewrite_record) @cobol_rewrite
  {
    node n
    attr (n) match = @cobol_rewrite, record = @cobol_rewrite_record
  }

  (delete_statement
    file_name: (WORD) @cobol_delete_file) @cobol_delete
  {
    node n
    attr (n) match = @cobol_delete, file = @cobol_delete_file
  }
"#;

fn js_file() -> &'static TsgFile {
    static FILE: OnceLock<TsgFile> = OnceLock::new();
    FILE.get_or_init(|| TsgFile::from_str(tree_sitter_javascript::LANGUAGE.into(), JS_DSL).expect("tsg js dsl"))
}

fn ts_file() -> &'static TsgFile {
    static FILE: OnceLock<TsgFile> = OnceLock::new();
    FILE.get_or_init(|| {
        TsgFile::from_str(tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into(), JS_DSL).expect("tsg ts dsl")
    })
}

fn tsx_file() -> &'static TsgFile {
    static FILE: OnceLock<TsgFile> = OnceLock::new();
    FILE.get_or_init(|| {
        TsgFile::from_str(tree_sitter_typescript::LANGUAGE_TSX.into(), JS_DSL).expect("tsg tsx dsl")
    })
}

fn java_file() -> &'static TsgFile {
    static FILE: OnceLock<TsgFile> = OnceLock::new();
    FILE.get_or_init(|| TsgFile::from_str(tree_sitter_java::LANGUAGE.into(), JAVA_DSL).expect("tsg java dsl"))
}

fn cobol_file() -> &'static TsgFile {
    static FILE: OnceLock<TsgFile> = OnceLock::new();
    FILE.get_or_init(|| TsgFile::from_str(tree_sitter_cobol::LANGUAGE.into(), COBOL_DSL).expect("tsg cobol dsl"))
}

fn parse(language: tree_sitter::Language, source: &str) -> tree_sitter::Tree {
    let mut parser = Parser::new();
    parser.set_language(&language).expect("set language");
    parser.parse(source, None).expect("parse")
}

fn node_text(source: &str, n: tree_sitter::Node<'_>) -> String {
    n.utf8_text(source.as_bytes())
        .unwrap_or("")
        .to_string()
}

fn normalize_string_literal(raw: &str) -> String {
    let s = raw.trim();
    if (s.starts_with('"') && s.ends_with('"')) || (s.starts_with('\'') && s.ends_with('\'')) {
        return s[1..s.len().saturating_sub(1)].to_string();
    }
    s.to_string()
}

fn js_match_to_import(source: &str, m: &TsgMatch<'_, '_>) -> Option<ImportHit> {
    // We identify the kind based on which capture names are present.
    if let Some((_q, mut it)) = m.named_capture("js_import_source") {
        let lit = it.next()?;
        let spec = normalize_string_literal(&node_text(source, lit));
        if spec.is_empty() {
            return None;
        }
        return Some(ImportHit {
            specifier: spec,
            kind: "es_import",
        });
    }
    if let Some((_q, mut it)) = m.named_capture("js_export_source") {
        let lit = it.next()?;
        let spec = normalize_string_literal(&node_text(source, lit));
        if spec.is_empty() {
            return None;
        }
        return Some(ImportHit {
            specifier: spec,
            kind: "export_from",
        });
    }
    if let (Some((_qf, mut f_it)), Some((_qa, mut a_it))) =
        (m.named_capture("js_call_func"), m.named_capture("js_call_arg"))
    {
        let func = node_text(source, f_it.next()?);
        let arg = normalize_string_literal(&node_text(source, a_it.next()?));
        if arg.is_empty() {
            return None;
        }
        let kind = match func.trim() {
            "require" => "require",
            "import" => "dynamic_import",
            _ => return None,
        };
        return Some(ImportHit {
            specifier: arg,
            kind,
        });
    }
    None
}

pub fn extract_js(source: &str) -> anyhow::Result<JsExtract> {
    extract_js_with(tree_sitter_javascript::LANGUAGE.into(), js_file(), source)
}

pub fn extract_ts(source: &str) -> anyhow::Result<JsExtract> {
    extract_js_with(tree_sitter_typescript::LANGUAGE_TYPESCRIPT.into(), ts_file(), source)
}

pub fn extract_tsx(source: &str) -> anyhow::Result<JsExtract> {
    extract_js_with(tree_sitter_typescript::LANGUAGE_TSX.into(), tsx_file(), source)
}

fn extract_js_with(language: tree_sitter::Language, file: &TsgFile, source: &str) -> anyhow::Result<JsExtract> {
    let source = source.replace("\r\n", "\n").replace('\r', "\n");
    let tree = parse(language, &source);

    let mut imports: Vec<ImportHit> = Vec::new();
    file.try_visit_matches(&tree, &source, false, |m| {
        if let Some(hit) = js_match_to_import(&source, &m) {
            imports.push(hit);
        }
        Ok::<(), anyhow::Error>(())
    })?;

    // Deterministic de-dup + sort.
    imports.sort_by(|a, b| a.kind.cmp(b.kind).then_with(|| a.specifier.cmp(&b.specifier)));
    imports.dedup();

    Ok(JsExtract { imports })
}

fn normalize_java_import_stmt(raw_stmt: &str) -> Option<String> {
    // raw_stmt includes the entire import_declaration, like:
    // - "import a.b.C;"
    // - "import static a.b.C.D;"
    // - "import a.b.*;"
    let mut s = raw_stmt.replace("\r\n", " ").replace('\r', " ").replace('\n', " ");
    s = s.split_whitespace().collect::<Vec<_>>().join(" ");
    let s = s.trim().trim_end_matches(';').trim();
    if !s.starts_with("import ") {
        return None;
    }
    let after_import = s.trim_start_matches("import ").trim();
    let (is_static, path) = if after_import.starts_with("static ") {
        (true, after_import.trim_start_matches("static ").trim())
    } else {
        (false, after_import)
    };
    let mut spec = path.trim().to_string();
    if spec.is_empty() {
        return None;
    }
    if is_static && !spec.ends_with(".*") {
        let parts: Vec<&str> = spec.split('.').collect();
        if parts.len() >= 2 {
            spec = parts[..parts.len() - 1].join(".");
        }
    }
    Some(spec)
}

fn java_decl_from_capture(source: &str, name_node: tree_sitter::Node<'_>, full: tree_sitter::Node<'_>) -> Option<SpanDecl> {
    let name = node_text(source, name_node).trim().to_string();
    if name.is_empty() {
        return None;
    }
    let start_line = (full.start_position().row as i64) + 1;
    let end_line = (full.end_position().row as i64) + 1;
    Some(SpanDecl {
        name,
        start_line,
        end_line: end_line.max(start_line),
    })
}

pub fn extract_java(source: &str) -> anyhow::Result<JavaExtract> {
    let source = source.replace("\r\n", "\n").replace('\r', "\n");
    let tree = parse(tree_sitter_java::LANGUAGE.into(), &source);

    let mut package: Option<String> = None;
    let mut imports: Vec<String> = Vec::new();
    let mut classes: Vec<SpanDecl> = Vec::new();
    let mut methods: Vec<SpanDecl> = Vec::new();
    let mut call_sites: Vec<CallSite> = Vec::new();

    java_file().try_visit_matches(&tree, &source, false, |m| {
        if package.is_none() {
            if let Some((_q, mut it)) = m.named_capture("java_package") {
                let n = it.next().unwrap();
                let p = node_text(&source, n).trim().trim_end_matches('.').to_string();
                if !p.is_empty() {
                    package = Some(p);
                }
            } else if let Some((_q, mut it)) = m.named_capture("java_package_ident") {
                let n = it.next().unwrap();
                let p = node_text(&source, n).trim().trim_end_matches('.').to_string();
                if !p.is_empty() {
                    package = Some(p);
                }
            }
        }

        if let Some((_q, mut it)) = m.named_capture("java_import") {
            if let Some(full) = it.next() {
                if let Some(spec) = normalize_java_import_stmt(&node_text(&source, full)) {
                    imports.push(spec);
                }
            }
        }

        if let (Some((_q1, mut name_it)), Some(full)) =
            (m.named_capture("java_class_name"), Some(m.full_capture()))
        {
            if let Some(name_node) = name_it.next() {
                if let Some(d) = java_decl_from_capture(&source, name_node, full) {
                    classes.push(d);
                }
            }
        }

        if let (Some((_q1, mut name_it)), Some(full)) =
            (m.named_capture("java_method_name"), Some(m.full_capture()))
        {
            if let Some(name_node) = name_it.next() {
                if let Some(d) = java_decl_from_capture(&source, name_node, full) {
                    methods.push(d);
                }
            }
        }

        if let Some((_q, mut it)) = m.named_capture("java_call_name") {
            if let Some(name_node) = it.next() {
                let name = node_text(&source, name_node).trim().to_string();
                if !name.is_empty() {
                    let line = (m.full_capture().start_position().row as i64) + 1;
                    call_sites.push(CallSite { name, start_line: line });
                }
            }
        }

        Ok::<(), anyhow::Error>(())
    })?;

    imports.sort();
    imports.dedup();
    classes.sort_by(|a, b| a.name.cmp(&b.name).then_with(|| a.start_line.cmp(&b.start_line)));
    classes.dedup_by(|a, b| a.name == b.name && a.start_line == b.start_line && a.end_line == b.end_line);
    methods.sort_by(|a, b| a.name.cmp(&b.name).then_with(|| a.start_line.cmp(&b.start_line)));
    methods.dedup_by(|a, b| a.name == b.name && a.start_line == b.start_line && a.end_line == b.end_line);
    call_sites.sort_by(|a, b| a.name.cmp(&b.name).then_with(|| a.start_line.cmp(&b.start_line)));
    call_sites.dedup_by(|a, b| a.name == b.name && a.start_line == b.start_line);

    Ok(JavaExtract {
        package,
        imports,
        classes,
        methods,
        call_sites,
    })
}

#[derive(Debug, Clone)]
pub struct CobolCallHit {
    pub callee: String,
    pub call_type: &'static str, // "literal" | "dynamic"
    pub logical_row: usize,      // 0-based line in parse stream
}

#[derive(Debug, Clone)]
pub struct CobolPerformHit {
    pub target: String,
    pub thru: Option<String>,
    pub logical_row: usize, // 0-based line in parse stream
}

#[derive(Debug, Clone)]
pub struct CobolIoHit {
    pub op: &'static str, // "READ" | "WRITE" | "REWRITE" | "DELETE"
    pub target: String,   // file-name (READ/DELETE) or record-name (WRITE/REWRITE)
    pub logical_row: usize, // 0-based line in parse stream
}

#[derive(Debug, Clone)]
pub struct CobolExtract {
    pub calls: Vec<CobolCallHit>,
    pub performs: Vec<CobolPerformHit>,
    pub io: Vec<CobolIoHit>,
}

fn strip_quotes(s: &str) -> String {
    let t = s.trim();
    if t.len() >= 2 {
        let b = t.as_bytes();
        let first = b[0] as char;
        let last = b[b.len() - 1] as char;
        if (first == '"' && last == '"') || (first == '\'' && last == '\'') {
            return t[1..t.len() - 1].to_string();
        }
    }
    t.to_string()
}

fn normalize_cobol_name(raw: &str) -> String {
    raw.trim()
        .trim_end_matches('.')
        .trim()
        .to_ascii_uppercase()
}

pub(crate) fn cobol_prepare_source(parse_stream: &str) -> String {
    // tree-sitter-cobol's external scanner expects fixed-format column semantics:
    // it consumes columns 1-6 as LINE_PREFIX_COMMENT. If we feed "columnless" text,
    // it will drop the first 6 chars of each line. So we synthesize:
    // - cols 1-6: spaces (sequence area)
    // - col 7: space (indicator)
    // - col 8+: the line content
    let mut out = String::new();
    for line in parse_stream.replace("\r\n", "\n").replace('\r', "\n").split('\n') {
        if line.is_empty() {
            out.push('\n');
            continue;
        }
        out.push_str("       "); // 7 spaces
        out.push_str(line);
        out.push('\n');
    }
    out
}

pub fn extract_cobol(parse_stream: &str) -> anyhow::Result<CobolExtract> {
    let prepared = cobol_prepare_source(parse_stream);
    let tree = parse(tree_sitter_cobol::LANGUAGE.into(), &prepared);

    let mut calls: Vec<CobolCallHit> = Vec::new();
    let mut performs: Vec<CobolPerformHit> = Vec::new();
    let mut io: Vec<CobolIoHit> = Vec::new();

    cobol_file().try_visit_matches(&tree, &prepared, false, |m| {
        if let Some((_q, mut it)) = m.named_capture("cobol_call_target") {
            if let Some(n) = it.next() {
                let raw = node_text(&prepared, n);
                let call_type = if raw.trim().starts_with('"') || raw.trim().starts_with('\'') {
                    "literal"
                } else {
                    "dynamic"
                };
                let callee = strip_quotes(&raw);
                if !callee.trim().is_empty() {
                    calls.push(CobolCallHit {
                        callee,
                        call_type,
                        logical_row: m.full_capture().start_position().row,
                    });
                }
            }
        }

        if let Some((_q, mut it)) = m.named_capture("cobol_perform_target") {
            if let Some(n) = it.next() {
                let target = normalize_cobol_name(&strip_quotes(&node_text(&prepared, n)));
                if !target.is_empty() {
                    let thru = m
                        .named_capture("cobol_perform_thru")
                        .and_then(|(_q, mut it)| it.next())
                        .map(|n| normalize_cobol_name(&strip_quotes(&node_text(&prepared, n))))
                        .filter(|s| !s.is_empty());
                    performs.push(CobolPerformHit {
                        target,
                        thru,
                        logical_row: m.full_capture().start_position().row,
                    });
                }
            }
        }

        if let Some((_q, mut it)) = m.named_capture("cobol_read_file") {
            if let Some(n) = it.next() {
                let name = normalize_cobol_name(&node_text(&prepared, n));
                if !name.is_empty() {
                    io.push(CobolIoHit {
                        op: "READ",
                        target: name,
                        logical_row: m.full_capture().start_position().row,
                    });
                }
            }
        }
        if let Some((_q, mut it)) = m.named_capture("cobol_delete_file") {
            if let Some(n) = it.next() {
                let name = normalize_cobol_name(&node_text(&prepared, n));
                if !name.is_empty() {
                    io.push(CobolIoHit {
                        op: "DELETE",
                        target: name,
                        logical_row: m.full_capture().start_position().row,
                    });
                }
            }
        }
        if let Some((_q, mut it)) = m.named_capture("cobol_write_record") {
            if let Some(n) = it.next() {
                let name = normalize_cobol_name(&node_text(&prepared, n));
                if !name.is_empty() {
                    io.push(CobolIoHit {
                        op: "WRITE",
                        target: name,
                        logical_row: m.full_capture().start_position().row,
                    });
                }
            }
        }
        if let Some((_q, mut it)) = m.named_capture("cobol_rewrite_record") {
            if let Some(n) = it.next() {
                let name = normalize_cobol_name(&node_text(&prepared, n));
                if !name.is_empty() {
                    io.push(CobolIoHit {
                        op: "REWRITE",
                        target: name,
                        logical_row: m.full_capture().start_position().row,
                    });
                }
            }
        }

        Ok::<(), anyhow::Error>(())
    })?;

    // Fallback: if the grammar can't form `call_statement` / `perform_statement_call_proc`
    // nodes (e.g. incomplete snippet without PROCEDURE DIVISION), do a conservative
    // line-based extraction so the pipeline remains usable.
    if calls.is_empty() || performs.is_empty() || io.is_empty() {
        let need_calls = calls.is_empty();
        let need_performs = performs.is_empty();
        let need_io = io.is_empty();

        let source = parse_stream.replace("\r\n", "\n").replace('\r', "\n");
        for (row, line) in source.lines().enumerate() {
            let trimmed = line.trim_start();
            if need_calls {
                let mut it = trimmed.splitn(2, char::is_whitespace);
                let head = it.next().unwrap_or("");
                if head.eq_ignore_ascii_case("CALL") {
                    let rest = it.next().unwrap_or("").trim();
                    if !rest.is_empty() {
                        let (raw, call_type) = if rest.starts_with('"') || rest.starts_with('\'') {
                            let q = rest.chars().next().unwrap();
                            let mut end = None;
                            for (i, ch) in rest.char_indices().skip(1) {
                                if ch == q {
                                    end = Some(i);
                                    break;
                                }
                            }
                            let raw = end.map(|i| &rest[..=i]).unwrap_or(rest);
                            (raw.to_string(), "literal")
                        } else {
                            let tok = rest.split_whitespace().next().unwrap_or(rest);
                            (tok.trim_end_matches('.').to_string(), "dynamic")
                        };
                        let callee = strip_quotes(&raw);
                        if !callee.trim().is_empty() {
                            calls.push(CobolCallHit {
                                callee,
                                call_type,
                                logical_row: row,
                            });
                        }
                    }
                }
            }

            if need_performs {
                let mut it = trimmed.splitn(2, char::is_whitespace);
                let head = it.next().unwrap_or("");
                if head.eq_ignore_ascii_case("PERFORM") {
                    let rest = it.next().unwrap_or("").trim();
                    if !rest.is_empty() {
                        let mut toks = rest.split_whitespace();
                        let target = toks
                            .next()
                            .unwrap_or("")
                            .trim_end_matches('.')
                            .to_ascii_uppercase();
                        let mut thru: Option<String> = None;
                        let next = toks.next().unwrap_or("");
                        if next.eq_ignore_ascii_case("THRU") || next.eq_ignore_ascii_case("THROUGH") {
                            thru = toks
                                .next()
                                .map(|s| s.trim_end_matches('.').to_ascii_uppercase())
                                .filter(|s| !s.is_empty());
                        }
                        if !target.trim().is_empty() {
                            performs.push(CobolPerformHit {
                                target,
                                thru,
                                logical_row: row,
                            });
                        }
                    }
                }
            }

            if need_io {
                let mut it = trimmed.splitn(2, char::is_whitespace);
                let head = it.next().unwrap_or("").to_ascii_uppercase();
                let rest = it.next().unwrap_or("").trim();
                if rest.is_empty() {
                    continue;
                }
                match head.as_str() {
                    "READ" => {
                        let file = rest
                            .split_whitespace()
                            .next()
                            .unwrap_or("")
                            .trim_end_matches('.')
                            .to_ascii_uppercase();
                        if !file.is_empty() {
                            io.push(CobolIoHit {
                                op: "READ",
                                target: file,
                                logical_row: row,
                            });
                        }
                    }
                    "WRITE" => {
                        let rec = rest
                            .split_whitespace()
                            .next()
                            .unwrap_or("")
                            .trim_end_matches('.')
                            .to_ascii_uppercase();
                        if !rec.is_empty() {
                            io.push(CobolIoHit {
                                op: "WRITE",
                                target: rec,
                                logical_row: row,
                            });
                        }
                    }
                    "REWRITE" => {
                        let rec = rest
                            .split_whitespace()
                            .next()
                            .unwrap_or("")
                            .trim_end_matches('.')
                            .to_ascii_uppercase();
                        if !rec.is_empty() {
                            io.push(CobolIoHit {
                                op: "REWRITE",
                                target: rec,
                                logical_row: row,
                            });
                        }
                    }
                    "DELETE" => {
                        let file = rest
                            .split_whitespace()
                            .next()
                            .unwrap_or("")
                            .trim_end_matches('.')
                            .to_ascii_uppercase();
                        if !file.is_empty() {
                            io.push(CobolIoHit {
                                op: "DELETE",
                                target: file,
                                logical_row: row,
                            });
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    calls.sort_by(|a, b| a.callee.cmp(&b.callee).then_with(|| a.logical_row.cmp(&b.logical_row)));
    calls.dedup_by(|a, b| a.callee == b.callee && a.call_type == b.call_type && a.logical_row == b.logical_row);
    // Deterministic ordering; prefer the richer THRU form if both patterns match the same PERFORM.
    performs.sort_by(|a, b| {
        a.logical_row
            .cmp(&b.logical_row)
            .then_with(|| a.target.cmp(&b.target))
            .then_with(|| b.thru.is_some().cmp(&a.thru.is_some()))
            .then_with(|| a.thru.cmp(&b.thru))
    });
    performs.dedup_by(|a, b| a.logical_row == b.logical_row && a.target == b.target);

    io.sort_by(|a, b| {
        a.op.cmp(b.op)
            .then_with(|| a.target.cmp(&b.target))
            .then_with(|| a.logical_row.cmp(&b.logical_row))
    });
    io.dedup_by(|a, b| a.op == b.op && a.target == b.target && a.logical_row == b.logical_row);

    Ok(CobolExtract { calls, performs, io })
}


