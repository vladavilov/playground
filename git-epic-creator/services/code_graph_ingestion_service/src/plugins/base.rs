use crate::core::records::{CodeNodeRecord, EdgeRecord};
use crate::core::types::CodeLanguage;
use serde_json::Value;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct IngestionContext {
    pub project_id: String,
    pub repo_fingerprint: String,
    pub repo_root: PathBuf,
    pub source_language: CodeLanguage,
}

pub trait LanguagePlugin: Send + Sync {
    fn name(&self) -> &str;

    /// Return repo-relative source files this plugin owns (must be deterministic).
    fn iter_files(&self, ctx: &IngestionContext) -> anyhow::Result<Vec<PathBuf>>;

    /// Produce nodes, edges, and plugin facts (deterministic).
    fn ingest(
        &self,
        ctx: &IngestionContext,
        files: &[PathBuf],
    ) -> anyhow::Result<(Vec<CodeNodeRecord>, Vec<EdgeRecord>, Value)>;

    fn owns_path(&self, _path: &Path) -> bool {
        true
    }
}


