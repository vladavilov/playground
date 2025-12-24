use ignore::gitignore::Gitignore;
use ignore::gitignore::GitignoreBuilder;
use std::path::Path;

// Deterministic defaults. Keep stable; expanding is ok if done deliberately.
const DEFAULT_IGNORE_PATTERNS: &[&str] = &[
    // VCS
    ".git/**",
    ".hg/**",
    ".svn/**",
    // Python
    "__pycache__/**",
    "*.pyc",
    "*.pyo",
    ".pytest_cache/**",
    ".mypy_cache/**",
    ".ruff_cache/**",
    ".tox/**",
    ".nox/**",
    ".venv/**",
    "venv/**",
    // Node
    "node_modules/**",
    // Java / build
    "target/**",
    "build/**",
    "dist/**",
    "out/**",
    // IDE / OS junk
    ".idea/**",
    ".vscode/**",
    "*.swp",
    "Thumbs.db",
    "Desktop.ini",
];

#[derive(Debug, Clone)]
pub struct IgnoreRules {
    gitignore: Gitignore,
}

impl IgnoreRules {
    pub fn is_ignored(&self, repo_rel_posix_path: &str) -> bool {
        // The ignore crate expects forward slashes for gitignore semantics.
        // It returns (is_match, maybe_whitelist).
        self.gitignore.matched(repo_rel_posix_path, false).is_ignore()
    }
}

pub fn build_ignore_rules(repo_root: &Path, extra_patterns: Option<&[String]>) -> anyhow::Result<IgnoreRules> {
    let mut b = GitignoreBuilder::new(repo_root);
    for pat in DEFAULT_IGNORE_PATTERNS {
        b.add_line(None, pat)?;
    }
    if let Some(extra) = extra_patterns {
        for pat in extra {
            b.add_line(None, pat)?;
        }
    }

    let gitignore_path = repo_root.join(".gitignore");
    if gitignore_path.is_file() {
        let text = std::fs::read_to_string(&gitignore_path)?;
        for line in text.lines() {
            b.add_line(None, line)?;
        }
    }

    let gi = b.build()?;
    Ok(IgnoreRules {
        gitignore: gi,
    })
}


