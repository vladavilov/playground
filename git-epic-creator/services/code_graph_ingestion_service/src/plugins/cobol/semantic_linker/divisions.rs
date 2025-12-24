use crate::plugins::cobol::normalizer::PreprocessResult;

use super::regexes;

/// Regex fallback for structure extraction, but operating on `logical_lines` (not physical lines),
/// so FIXED-format sequence/indicator columns do not break recognition.
pub(crate) fn extract_divisions_from_logical(preprocess: &PreprocessResult) -> Vec<(String, String, i64, i64)> {
    let eof = (preprocess.physical_lines.len() as i64).max(1);
    let mut hits: Vec<(String, String, i64)> = Vec::new();
    for (idx, line) in preprocess.logical_lines.iter().enumerate() {
        if let Some(cap) = regexes::re_division().captures(line) {
            let sym = cap.get(1).unwrap().as_str().trim().to_ascii_uppercase();
            let start = preprocess.logical_spans.get(idx).map(|(s, _e)| *s).unwrap_or(1);
            hits.push(("division".to_string(), sym, start));
        }
    }
    hits.sort_by(|a, b| a.2.cmp(&b.2).then_with(|| a.1.cmp(&b.1)));
    hits.dedup();

    let mut out = Vec::new();
    for (i, (kind, sym, start)) in hits.iter().enumerate() {
        let end = if i + 1 < hits.len() {
            (hits[i + 1].2 - 1).max(*start)
        } else {
            eof
        };
        out.push((kind.clone(), sym.clone(), *start, end));
    }
    out
}

fn find_division_for_line(div_spans: &[(String, i64, i64)], line_no: i64) -> Option<(String, i64, i64)> {
    for (sym, s, e) in div_spans {
        if *s <= line_no && line_no <= *e {
            return Some((sym.clone(), *s, *e));
        }
    }
    None
}

pub(crate) fn extract_sections_from_logical(
    preprocess: &PreprocessResult,
    divisions: &[(String, String, i64, i64)],
) -> Vec<(String, String, i64, i64)> {
    let eof = (preprocess.physical_lines.len() as i64).max(1);
    let div_spans: Vec<(String, i64, i64)> = divisions.iter().map(|(_, sym, s, e)| (sym.clone(), *s, *e)).collect();

    let mut hits: Vec<(String, String, i64)> = Vec::new();
    for (idx, line) in preprocess.logical_lines.iter().enumerate() {
        if let Some(cap) = regexes::re_section().captures(line) {
            let name = cap.get(1).unwrap().as_str().trim();
            if name.is_empty() {
                continue;
            }
            let start = preprocess.logical_spans.get(idx).map(|(s, _e)| *s).unwrap_or(1);
            let div = find_division_for_line(&div_spans, start);
            let Some((div_name, _, _)) = div else { continue };
            if !matches!(div_name.as_str(), "DATA" | "PROCEDURE" | "ENVIRONMENT") {
                continue;
            }
            hits.push(("section".to_string(), name.to_ascii_uppercase(), start));
        }
    }
    hits.sort_by(|a, b| a.2.cmp(&b.2).then_with(|| a.1.cmp(&b.1)));
    hits.dedup();

    let mut out = Vec::new();
    for (i, (kind, sym, start)) in hits.iter().enumerate() {
        let end = if i + 1 < hits.len() {
            (hits[i + 1].2 - 1).max(*start)
        } else {
            eof
        };
        out.push((kind.clone(), sym.clone(), *start, end));
    }
    out
}

pub(crate) fn extract_paragraphs_from_logical(
    preprocess: &PreprocessResult,
    divisions: &[(String, String, i64, i64)],
) -> Vec<(String, String, i64, i64)> {
    let proc = divisions.iter().find(|(_, sym, _, _)| sym == "PROCEDURE");
    let Some((_k, _sym, proc_start, proc_end)) = proc else { return vec![] };

    let mut hits: Vec<(String, String, i64)> = Vec::new();
    for (idx, line) in preprocess.logical_lines.iter().enumerate() {
        let start = preprocess.logical_spans.get(idx).map(|(s, _e)| *s).unwrap_or(1);
        if start < *proc_start || start > *proc_end {
            continue;
        }

        let stripped = line.trim();
        if !stripped.ends_with('.') {
            continue;
        }
        let token = stripped.trim_end_matches('.').trim();
        if token.is_empty() || token.contains(' ') {
            continue;
        }
        let up = token.to_ascii_uppercase();
        if regexes::reserved_para_names().contains(up.as_str()) {
            continue;
        }
        if up.ends_with("DIVISION") || up.ends_with("SECTION") || up == "PROGRAM-ID" {
            continue;
        }
        let indent = line.len() - line.trim_start_matches(' ').len();
        if indent > 12 {
            continue;
        }

        hits.push(("paragraph".to_string(), up, start));
    }
    hits.sort_by(|a, b| a.2.cmp(&b.2).then_with(|| a.1.cmp(&b.1)));
    hits.dedup();

    let mut out = Vec::new();
    for (i, (kind, sym, start)) in hits.iter().enumerate() {
        let end = if i + 1 < hits.len() {
            (hits[i + 1].2 - 1).max(*start)
        } else {
            *proc_end
        };
        out.push((kind.clone(), sym.clone(), *start, end));
    }
    out
}

#[cfg(test)]
mod tests {
    use crate::plugins::cobol::normalizer::preprocess_cobol_bytes;

    use super::{extract_divisions_from_logical, extract_paragraphs_from_logical, extract_sections_from_logical};

    #[test]
    fn logical_regex_fallback_handles_fixed_sequence_area() {
        // Fixed-format with sequence numbers: physical lines start with digits, which breaks
        // naive regex scanning on physical lines. The fallback here must use logical_lines.
        let src = concat!(
            "000100 IDENTIFICATION DIVISION.\n",
            "000200 PROGRAM-ID. PGM1.\n",
            "000300 DATA DIVISION.\n",
            "000400 WORKING-STORAGE SECTION.\n",
            "000500 01 REC.\n",
            "000600 PROCEDURE DIVISION.\n",
            "000700 1000-INIT.\n",
            "000800     STOP RUN.\n",
            "000900 2000-WORK.\n",
            "001000     EXIT.\n",
        )
        .as_bytes()
        .to_vec();

        let prep = preprocess_cobol_bytes(&src);
        let divs = extract_divisions_from_logical(&prep);
        assert!(divs.iter().any(|(_, sym, _, _)| sym == "IDENTIFICATION"));
        assert!(divs.iter().any(|(_, sym, _, _)| sym == "DATA"));
        assert!(divs.iter().any(|(_, sym, _, _)| sym == "PROCEDURE"));

        let secs = extract_sections_from_logical(&prep, &divs);
        assert!(secs.iter().any(|(_, sym, _, _)| sym == "WORKING-STORAGE"));

        let paras = extract_paragraphs_from_logical(&prep, &divs);
        assert!(paras.iter().any(|(_, sym, _, _)| sym == "1000-INIT"));
        assert!(paras.iter().any(|(_, sym, _, _)| sym == "2000-WORK"));
    }
}


