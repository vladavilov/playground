pub(crate) fn slice_text(lines: &[String], start_line: i64, end_line: i64) -> String {
    let s = start_line.max(1);
    let e = end_line.min(lines.len() as i64);
    if e < s {
        return String::new();
    }
    lines[(s - 1) as usize..=(e - 1) as usize].join("\n") + "\n"
}


