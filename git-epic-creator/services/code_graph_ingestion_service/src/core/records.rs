use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

use crate::core::types::{CodeLanguage, CodeRelType};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeNodeRecord {
    pub project_id: String,
    pub repo_fingerprint: String,
    pub node_id: String,
    pub language: CodeLanguage,
    pub kind: String,
    pub symbol: Option<String>,
    pub file_path: String,
    pub start_line: i64,
    pub end_line: i64,
    pub snippet_hash: String,
    pub text: String,
    #[serde(default)]
    pub extra_labels: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EdgeRecord {
    pub project_id: String,
    pub repo_fingerprint: String,
    pub rel_type: CodeRelType,
    pub src_node_id: String,
    pub dst_node_id: String,
    pub confidence: f64,
    #[serde(default)]
    pub metadata: Map<String, Value>,
}


