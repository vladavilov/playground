use sha2::{Digest, Sha256};
use uuid::Uuid;

pub fn canonical_json_bytes(value: &serde_json::Value) -> Vec<u8> {
    // Canonical JSON bytes (sorted keys, no whitespace).
    // serde_json doesn't provide canonicalization directly, so we re-serialize after
    // recursively sorting object keys.
    fn canonicalize(v: &serde_json::Value) -> serde_json::Value {
        match v {
            serde_json::Value::Object(map) => {
                let mut keys: Vec<_> = map.keys().cloned().collect();
                keys.sort();
                let mut out = serde_json::Map::new();
                for k in keys {
                    out.insert(k.clone(), canonicalize(&map[&k]));
                }
                serde_json::Value::Object(out)
            }
            serde_json::Value::Array(arr) => {
                serde_json::Value::Array(arr.iter().map(canonicalize).collect())
            }
            other => other.clone(),
        }
    }

    let canon = canonicalize(value);
    serde_json::to_vec(&canon).expect("json bytes")
}

pub fn sha256_hex(data: &[u8]) -> String {
    hex::encode(Sha256::digest(data))
}

#[derive(Debug, Clone)]
pub struct RepoIndexUpsertResult {
    pub content_sha256: String,
}

pub fn upsert_repo_index_sync(
    database_url: &str,
    project_id: Uuid,
    repo_fingerprint: &str,
    repo_index_json: &serde_json::Value,
) -> anyhow::Result<RepoIndexUpsertResult> {
    let canonical = canonical_json_bytes(repo_index_json);
    let content_sha = sha256_hex(&canonical);

    let mut client = postgres::Client::connect(database_url, postgres::NoTls)?;
    client.batch_execute("SET statement_timeout TO 60000")?;

    client.execute(
        r#"
        INSERT INTO project_repo_indexes (project_id, repo_fingerprint, repo_index_json, content_sha256)
        VALUES ($1::uuid, $2, $3::jsonb, $4)
        ON CONFLICT (project_id, repo_fingerprint)
        DO UPDATE SET
            repo_index_json = EXCLUDED.repo_index_json,
            content_sha256 = EXCLUDED.content_sha256,
            updated_at = NOW()
        "#,
        &[
            &project_id.to_string(),
            &repo_fingerprint,
            &String::from_utf8_lossy(&canonical).to_string(),
            &content_sha,
        ],
    )?;

    Ok(RepoIndexUpsertResult {
        content_sha256: content_sha,
    })
}


