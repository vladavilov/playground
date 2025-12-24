pub mod api;
pub mod config;
pub mod domain;
pub mod executor;
pub mod http;
pub mod neo4j;
pub mod queries;

#[derive(Clone)]
pub struct AppState {
    pub cfg: config::Config,
    pub executor: executor::DynExecutor,
    pub queries: queries::QueryRegistry,
}
