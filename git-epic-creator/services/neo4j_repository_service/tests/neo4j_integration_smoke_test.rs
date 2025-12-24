use std::time::Duration;

use neo4j_repository_service::{config::Config, neo4j};

/// Integration smoke test.
///
/// Runs only when these env vars are set:
/// - NEO4J_TEST_URI
/// - NEO4J_TEST_USERNAME
/// - NEO4J_TEST_PASSWORD
/// - NEO4J_TEST_DATABASE (optional; defaults to "neo4j")
#[tokio::test]
async fn neo4j_healthcheck_smoke() {
    let uri = match std::env::var("NEO4J_TEST_URI") {
        Ok(v) => v,
        Err(_) => return,
    };
    let username = match std::env::var("NEO4J_TEST_USERNAME") {
        Ok(v) => v,
        Err(_) => return,
    };
    let password = match std::env::var("NEO4J_TEST_PASSWORD") {
        Ok(v) => v,
        Err(_) => return,
    };
    let database = std::env::var("NEO4J_TEST_DATABASE").unwrap_or_else(|_| "neo4j".to_string());

    let cfg = Config {
        bind_addr: "127.0.0.1:0".parse().unwrap(),
        queries_dir: "queries".to_string(),
        neo4j_uri: uri,
        neo4j_username: username,
        neo4j_password: password,
        neo4j_database: database,
        neo4j_max_connections: 4,
        neo4j_connect_timeout: Duration::from_secs(10),
    };

    let graph = neo4j::connect(&cfg).await.unwrap();
    neo4j::healthcheck(&graph).await.unwrap();
}


