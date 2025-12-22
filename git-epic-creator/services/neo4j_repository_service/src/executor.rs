use std::{collections::HashMap, sync::Arc};

use async_trait::async_trait;
use neo4rs::Graph;
use serde_json::Value;

use crate::neo4j;

#[async_trait]
pub trait Neo4jExecutor: Send + Sync {
    async fn healthcheck(&self) -> Result<(), neo4j::Neo4jError>;

    async fn execute_write(
        &self,
        cypher: &str,
        params: HashMap<String, Value>,
    ) -> Result<neo4j::ExecuteSummary, neo4j::Neo4jError>;

    async fn execute_read_rows(
        &self,
        cypher: &str,
        params: HashMap<String, Value>,
        fields: Vec<String>,
        max_rows: usize,
    ) -> Result<Vec<HashMap<String, Value>>, neo4j::Neo4jError>;
}

#[derive(Clone)]
pub struct Neo4rsExecutor {
    graph: Graph,
}

impl Neo4rsExecutor {
    pub fn new(graph: Graph) -> Self {
        Self { graph }
    }
}

#[async_trait]
impl Neo4jExecutor for Neo4rsExecutor {
    async fn healthcheck(&self) -> Result<(), neo4j::Neo4jError> {
        neo4j::healthcheck(&self.graph).await
    }

    async fn execute_write(
        &self,
        cypher: &str,
        params: HashMap<String, Value>,
    ) -> Result<neo4j::ExecuteSummary, neo4j::Neo4jError> {
        neo4j::execute_write(&self.graph, cypher, params).await
    }

    async fn execute_read_rows(
        &self,
        cypher: &str,
        params: HashMap<String, Value>,
        fields: Vec<String>,
        max_rows: usize,
    ) -> Result<Vec<HashMap<String, Value>>, neo4j::Neo4jError> {
        neo4j::execute_read_rows(&self.graph, cypher, params, &fields, max_rows).await
    }
}

pub type DynExecutor = Arc<dyn Neo4jExecutor>;
