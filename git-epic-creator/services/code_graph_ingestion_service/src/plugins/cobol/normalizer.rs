use regex::Regex;

#[derive(Debug, Clone)]
pub struct PreprocessResult {
    pub physical_lines: Vec<String>,
    pub logical_lines: Vec<String>,
    pub logical_spans: Vec<(i64, i64)>, // (start_physical_line, end_physical_line)
    pub parse_bytes: Vec<u8>,
}

fn re_free() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r">>\s*SOURCE\s+FORMAT\s+FREE").unwrap())
}
fn re_fixed() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r">>\s*SOURCE\s+FORMAT\s+FIXED").unwrap())
}

pub fn preprocess_cobol_bytes(raw: &[u8]) -> PreprocessResult {
    // Evidence: preserve physical lines verbatim (but normalize newlines for internal processing).
    let mut text = String::from_utf8_lossy(raw).to_string();
    text = text.replace("\r\n", "\n").replace('\r', "\n");
    let mut physical_lines: Vec<String> = text.split('\n').map(|s| s.to_string()).collect();
    if physical_lines.last().is_some_and(|s| s.is_empty()) {
        physical_lines.pop();
    }

    let mut mode = "FIXED".to_string();
    let mut logical_lines: Vec<String> = Vec::new();
    let mut logical_spans: Vec<(i64, i64)> = Vec::new();

    let mut cur_parts: Vec<String> = Vec::new();
    let mut cur_span: Option<(i64, i64)> = None;

    let flush_current = |cur_parts: &mut Vec<String>, cur_span: &mut Option<(i64, i64)>, logical_lines: &mut Vec<String>, logical_spans: &mut Vec<(i64, i64)>| {
        if cur_parts.is_empty() || cur_span.is_none() {
            cur_parts.clear();
            *cur_span = None;
            return;
        }
        logical_lines.push(cur_parts.join(""));
        logical_spans.push(cur_span.unwrap());
        cur_parts.clear();
        *cur_span = None;
    };

    for (i, line) in physical_lines.iter().enumerate() {
        let line_no = (i as i64) + 1;

        let mode_next = if re_free().is_match(line) {
            "FREE".to_string()
        } else if re_fixed().is_match(line) {
            "FIXED".to_string()
        } else {
            mode.clone()
        };

        let (kind, mut content, is_cont) = classify_and_extract(&mode, line);
        if kind == "comment" {
            flush_current(&mut cur_parts, &mut cur_span, &mut logical_lines, &mut logical_spans);
            logical_lines.push(String::new());
            logical_spans.push((line_no, line_no));
            mode = mode_next;
            continue;
        }

        content = strip_inline_comment_to_eol(&content);

        if cur_parts.is_empty() {
            cur_parts.push(content);
            cur_span = Some((line_no, line_no));
        } else if is_cont {
            cur_parts.push(content);
            if let Some((s, _e)) = cur_span {
                cur_span = Some((s, line_no));
            } else {
                cur_span = Some((line_no, line_no));
            }
        } else {
            flush_current(&mut cur_parts, &mut cur_span, &mut logical_lines, &mut logical_spans);
            cur_parts.push(content);
            cur_span = Some((line_no, line_no));
        }

        mode = mode_next;
    }

    flush_current(&mut cur_parts, &mut cur_span, &mut logical_lines, &mut logical_spans);

    let (logical_lines, logical_spans) = collapse_exec_blocks(&logical_lines, &logical_spans);
    let parse_stream = logical_lines.join("\n") + "\n";

    PreprocessResult {
        physical_lines,
        logical_lines,
        logical_spans,
        parse_bytes: parse_stream.into_bytes(),
    }
}

fn strip_inline_comment_to_eol(s: &str) -> String {
    if let Some(idx) = s.find("*>") {
        return s[..idx].to_string();
    }
    s.to_string()
}

fn classify_and_extract(mode: &str, line: &str) -> (&'static str, String, bool) {
    if mode == "FIXED" {
        let mut padded = line.to_string();
        if padded.len() < 80 {
            padded.push_str(&" ".repeat(80 - padded.len()));
        }
        let indicator = padded.as_bytes().get(6).copied().unwrap_or(b' ') as char;
        if indicator == '*' || indicator == '/' || indicator.to_ascii_uppercase() == 'D' {
            return ("comment", String::new(), false);
        }
        let content = if padded.len() >= 72 {
            padded[7..72].to_string()
        } else if padded.len() > 7 {
            padded[7..].to_string()
        } else {
            String::new()
        };
        let is_cont = indicator == '-';
        return ("code", content.trim_end_matches('\n').to_string(), is_cont);
    }

    // FREE
    let stripped = line.trim_start();
    if stripped.starts_with('*') {
        return ("comment", String::new(), false);
    }
    if stripped.starts_with('-') {
        return ("code", stripped[1..].to_string(), true);
    }
    ("code", line.to_string(), false)
}

fn collapse_exec_blocks(
    logical_lines: &[String],
    logical_spans: &[(i64, i64)],
) -> (Vec<String>, Vec<(i64, i64)>) {
    let mut out_lines: Vec<String> = Vec::new();
    let mut out_spans: Vec<(i64, i64)> = Vec::new();

    let mut i = 0usize;
    while i < logical_lines.len() {
        let line = &logical_lines[i];
        let span = logical_spans[i];

        if line.trim_start().to_ascii_uppercase().starts_with("EXEC ") {
            let start_span = span.0;
            let mut end_span = span.1;
            let mut j = i + 1;
            while j < logical_lines.len() {
                end_span = logical_spans[j].1;
                if logical_lines[j].to_ascii_uppercase().contains("END-EXEC") {
                    j += 1;
                    break;
                }
                j += 1;
            }
            out_lines.push("EXEC_BLOCK.".to_string());
            out_spans.push((start_span, end_span));
            i = j;
            continue;
        }

        out_lines.push(line.clone());
        out_spans.push(span);
        i += 1;
    }

    (out_lines, out_spans)
}


