use axum::Router;
use tower_http::{compression::CompressionLayer, request_id::MakeRequestUuid, trace::TraceLayer};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

use std::sync::Arc;

use neo4j_repository_service::{
    AppState, config::Config, executor::Neo4rsExecutor, http, neo4j, queries::QueryRegistry,
};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::registry()
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .with(tracing_subscriber::fmt::layer())
        .init();

    let cfg = Config::from_env()?;
    let queries = QueryRegistry::load_from_dir(&cfg.queries_dir)?;
    let graph = neo4j::connect(&cfg).await?;
    let executor = Arc::new(Neo4rsExecutor::new(graph));

    let state = AppState {
        cfg: cfg.clone(),
        executor,
        queries,
    };

    let app: Router = http::router()
        .layer(TraceLayer::new_for_http())
        .layer(tower_http::request_id::SetRequestIdLayer::x_request_id(
            MakeRequestUuid,
        ))
        .layer(CompressionLayer::new())
        // Safety: avoid accidental huge payloads (vectors, neighborhoods, etc.).
        .layer(tower_http::limit::RequestBodyLimitLayer::new(
            4 * 1024 * 1024,
        ))
        .with_state(state);

    let listener = tokio::net::TcpListener::bind(cfg.bind_addr).await?;
    tracing::info!("neo4j-repository-service listening on {}", cfg.bind_addr);

    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await?;

    Ok(())
}

async fn shutdown_signal() {
    let _ = tokio::signal::ctrl_c().await;
    tracing::info!("shutdown signal received");
}
