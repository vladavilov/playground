use std::{env, net::SocketAddr, time::Duration};

#[derive(Clone, Debug)]
pub struct Config {
    pub bind_addr: SocketAddr,
    pub queries_dir: String,
    pub neo4j_uri: String,
    pub neo4j_username: String,
    pub neo4j_password: String,
    pub neo4j_database: String,
    pub neo4j_max_connections: usize,
    pub neo4j_connect_timeout: Duration,
}

impl Config {
    pub fn from_env() -> anyhow::Result<Self> {
        let api_port: u16 = env::var("API_PORT")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(8080);
        let bind_host = env::var("BIND_HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
        let bind_addr: SocketAddr = format!("{bind_host}:{api_port}").parse()?;

        // Intentionally fixed location inside the container image.
        // We do not support overriding this via environment variables.
        let queries_dir = "/app/queries".to_string();

        let neo4j_uri = env::var("NEO4J_URI").unwrap_or_else(|_| "bolt://neo4j:7687".to_string());
        let neo4j_username = env::var("NEO4J_USERNAME").unwrap_or_else(|_| "neo4j".to_string());
        let neo4j_password = env::var("NEO4J_PASSWORD").unwrap_or_else(|_| "neo4j123".to_string());
        let neo4j_database = env::var("NEO4J_DATABASE").unwrap_or_else(|_| "neo4j".to_string());

        let neo4j_max_connections: usize = env::var("NEO4J_MAX_CONNECTION_POOL_SIZE")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(50);
        let neo4j_connect_timeout = Duration::from_millis(
            env::var("NEO4J_CONNECT_TIMEOUT_MS")
                .ok()
                .and_then(|v| v.parse().ok())
                .unwrap_or(30_000),
        );

        Ok(Self {
            bind_addr,
            queries_dir,
            neo4j_uri,
            neo4j_username,
            neo4j_password,
            neo4j_database,
            neo4j_max_connections,
            neo4j_connect_timeout,
        })
    }
}
