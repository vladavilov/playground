use std::{
    collections::HashMap,
    ffi::OsStr,
    path::{Path, PathBuf},
};

use thiserror::Error;

#[derive(Debug, Error)]
pub enum QueryError {
    #[error("queries dir does not exist: {0}")]
    QueriesDirMissing(String),
    #[error("failed to read query file {path}: {source}")]
    ReadFailed {
        path: String,
        source: std::io::Error,
    },
    #[error("query not found: {0}")]
    NotFound(String),
}

#[derive(Clone, Debug)]
pub struct QueryRegistry {
    // Key is a stable API identifier: relative path without extension, using `/` separators.
    // Example: `graphrag/merge_document`
    queries: HashMap<String, String>,
}

impl QueryRegistry {
    pub fn load_from_dir(dir: &str) -> Result<Self, QueryError> {
        let root = PathBuf::from(dir);
        if !root.exists() {
            return Err(QueryError::QueriesDirMissing(dir.to_string()));
        }
        let mut queries = HashMap::new();
        Self::walk(&root, &root, &mut queries)?;
        Ok(Self { queries })
    }

    fn walk(
        root: &Path,
        current: &Path,
        out: &mut HashMap<String, String>,
    ) -> Result<(), QueryError> {
        let entries = std::fs::read_dir(current).map_err(|e| QueryError::ReadFailed {
            path: current.display().to_string(),
            source: e,
        })?;

        for entry in entries {
            let entry = entry.map_err(|e| QueryError::ReadFailed {
                path: current.display().to_string(),
                source: e,
            })?;
            let path = entry.path();

            if path.is_dir() {
                Self::walk(root, &path, out)?;
                continue;
            }

            if path.extension() != Some(OsStr::new("cypher")) {
                continue;
            }

            let key = Self::key_for_path(root, &path);
            let content = std::fs::read_to_string(&path).map_err(|e| QueryError::ReadFailed {
                path: path.display().to_string(),
                source: e,
            })?;
            // Strip leading/trailing whitespace but keep internal formatting intact.
            out.insert(key, content.trim().to_string());
        }

        Ok(())
    }

    fn key_for_path(root: &Path, file: &Path) -> String {
        let rel = file.strip_prefix(root).unwrap_or(file);
        let rel = rel.with_extension("");
        // Normalize to `/` separators for stable keys across OSes.
        rel.to_string_lossy().replace('\\', "/")
    }

    pub fn get(&self, key: &str) -> Result<&str, QueryError> {
        self.queries
            .get(key)
            .map(|s| s.as_str())
            .ok_or_else(|| QueryError::NotFound(key.to_string()))
    }

    pub fn keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = self.queries.keys().cloned().collect();
        keys.sort();
        keys
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mk_temp_dir() -> PathBuf {
        let mut p = std::env::temp_dir();
        p.push(format!("neo4j_repo_service_queries_test_{}", uuid::Uuid::new_v4()));
        std::fs::create_dir_all(&p).unwrap();
        p
    }

    #[test]
    fn load_from_dir_errors_when_missing() {
        let missing = mk_temp_dir().join("does-not-exist");
        let err = QueryRegistry::load_from_dir(missing.to_string_lossy().as_ref()).unwrap_err();
        assert!(matches!(err, QueryError::QueriesDirMissing(_)));
    }

    #[test]
    fn load_from_dir_reads_cypher_files_and_normalizes_keys() {
        let root = mk_temp_dir();
        let nested = root.join("schema").join("graphrag");
        std::fs::create_dir_all(&nested).unwrap();

        std::fs::write(nested.join("constraint_entity_id_unique.cypher"), "  RETURN 1  \n").unwrap();
        std::fs::write(nested.join("ignore.txt"), "nope").unwrap();

        let reg = QueryRegistry::load_from_dir(root.to_string_lossy().as_ref()).unwrap();

        // Must use `/` regardless of platform separators, and must strip `.cypher`.
        assert_eq!(reg.keys(), vec!["schema/graphrag/constraint_entity_id_unique".to_string()]);
        assert_eq!(
            reg.get("schema/graphrag/constraint_entity_id_unique").unwrap(),
            "RETURN 1"
        );

        std::fs::remove_dir_all(&root).ok();
    }

    #[test]
    fn get_returns_not_found_for_unknown_key() {
        let root = mk_temp_dir();
        let reg = QueryRegistry::load_from_dir(root.to_string_lossy().as_ref()).unwrap();
        let err = reg.get("nope").unwrap_err();
        assert!(matches!(err, QueryError::NotFound(_)));
        std::fs::remove_dir_all(&root).ok();
    }
}
