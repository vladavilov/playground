use regex::Regex;

pub(crate) fn re_division() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r"(?i)^\s*(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\.\s*$").unwrap()
    })
}

pub(crate) fn re_section() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r"(?i)^\s*([A-Za-z0-9_-]+)\s+SECTION\.\s*$").unwrap())
}

pub(crate) fn re_data_def() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r"(?i)^\s*(\d{2}|66|77|88)\s+([A-Za-z0-9_-]+)\b").unwrap())
}

pub(crate) fn re_qualified_ref() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r"(?i)\b([A-Za-z0-9_-]+)(?:\s+(?:OF|IN)\s+[A-Za-z0-9_-]+)+\b").unwrap()
    })
}

pub(crate) fn re_exec_header() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r"(?i)\bEXEC\s+(SQL|CICS)\b").unwrap())
}

pub(crate) fn re_sql_from_join() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    // Very conservative: FROM <ident> and JOIN <ident>
    RE.get_or_init(|| {
        Regex::new(r"(?i)\b(?:FROM|JOIN)\s+([A-Z0-9_#$@]+(?:\.[A-Z0-9_#$@]+)*)\b").unwrap()
    })
}

pub(crate) fn re_sql_insert_into() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r"(?i)\bINSERT\s+INTO\s+([A-Z0-9_#$@]+(?:\.[A-Z0-9_#$@]+)*)\b").unwrap()
    })
}

pub(crate) fn re_sql_update() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r"(?i)\bUPDATE\s+([A-Z0-9_#$@]+(?:\.[A-Z0-9_#$@]+)*)\b").unwrap())
}

pub(crate) fn re_sql_delete_from() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r"(?i)\bDELETE\s+FROM\s+([A-Z0-9_#$@]+(?:\.[A-Z0-9_#$@]+)*)\b").unwrap()
    })
}

pub(crate) fn re_sql_merge_into() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r"(?i)\bMERGE\s+INTO\s+([A-Z0-9_#$@]+(?:\.[A-Z0-9_#$@]+)*)\b").unwrap()
    })
}

pub(crate) fn re_sql_hostvar() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    // COBOL host variables in embedded SQL: :WS-NAME, :REC-ID, :X, :IND, etc.
    // We normalize them to SQL-ish identifiers so the SQL grammar can parse.
    RE.get_or_init(|| Regex::new(r":([A-Za-z0-9_-]+)").unwrap())
}

pub(crate) fn re_exec_cics_command() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r"(?i)\bEXEC\s+CICS\s+([A-Z0-9-]+)\b").unwrap())
}

pub(crate) fn re_exec_cics_program_arg() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    // PROGRAM(<literal-or-ident>) — we accept either quoted literal or identifier.
    RE.get_or_init(|| {
        Regex::new(r#"(?i)\bPROGRAM\s*\(\s*(?:'([^']+)'|"([^"]+)"|([A-Z0-9_#$@-]+))\s*\)"#).unwrap()
    })
}

pub(crate) fn reserved_para_names() -> &'static std::collections::HashSet<&'static str> {
    static SET: std::sync::OnceLock<std::collections::HashSet<&'static str>> = std::sync::OnceLock::new();
    SET.get_or_init(|| {
        [
            "ACCEPT", "ADD", "CALL", "CANCEL", "CLOSE", "COMPUTE", "CONTINUE", "DELETE", "DIVIDE",
            "DISPLAY", "ELSE", "END", "END-IF", "END-READ", "END-WRITE", "EVALUATE", "EXEC",
            "EXIT", "GOBACK", "GO", "IF", "INITIALIZE", "INSPECT", "MERGE", "MOVE", "MULTIPLY",
            "OPEN", "PERFORM", "READ", "RETURN", "REWRITE", "SEARCH", "SET", "SORT", "START",
            "STOP", "STRING", "SUBTRACT", "UNSTRING", "WHEN", "WRITE",
        ]
        .into_iter()
        .collect()
    })
}


