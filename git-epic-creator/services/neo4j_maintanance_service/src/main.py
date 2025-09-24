"""
Neo4j Graph RAG Schema Initialization Service

This service provides a one-call initialization of the Neo4j Graph RAG schema
for the Agentic AI Requirements Engineering System.

Enhanced with maintenance capabilities.
"""

from typing import Dict, Any
import uvicorn
from fastapi import APIRouter, Depends, Request
from fastapi.responses import JSONResponse
import structlog

# Import shared library components
from configuration.common_config import get_app_settings
from configuration.logging_config import configure_logging
from utils.neo4j_client import get_neo4j_client, Neo4jClient
from utils.error_handler import ErrorHandler
from utils.app_factory import FastAPIFactory

# Import service classes at module level for testing
from services.schema_query_builder import SchemaQueryBuilder
from services.neo4j_schema_service import Neo4jSchemaService
from services.neo4j_index_maintenance import Neo4jIndexMaintenance

# Configure logging at application startup
configure_logging()
logger = structlog.get_logger(__name__)

# Create FastAPI application using the factory
app = FastAPIFactory.create_app(
    title="Neo4j Graph RAG Schema Initialization Service",
    description="A comprehensive microservice to initialize and maintain Neo4j Graph RAG schema "
               "for the Agentic AI Requirements Engineering System",
    version="1.0.0",
    enable_azure_auth=False,  # Disable Azure auth for this service
    enable_docs_auth=False,   # Disable docs auth for this service
    enable_cors=True,         # Enable CORS for API access
    enable_neo4j=True         # Enable Neo4j integration
)

# Create API router for Neo4j operations
neo4j_router = APIRouter()

# Dependency to get neo4j client from app state
def get_db_client(request: Request) -> Neo4jClient:
    """Get Neo4j client from application state."""
    return request.app.state.neo4j_client

# Override dependencies for service classes
def get_schema_service(db_client: Neo4jClient = Depends(get_db_client)) -> Neo4jSchemaService:
    """Get Neo4j schema service instance."""
    return Neo4jSchemaService(
        db_client,
        SchemaQueryBuilder(),
        db_client.settings
    )

def get_maintenance_service(db_client: Neo4jClient = Depends(get_db_client)) -> Neo4jIndexMaintenance:
    """Get Neo4j index maintenance service instance."""
    return Neo4jIndexMaintenance(db_client, db_client.settings)

@neo4j_router.post("/init-neo4j")
async def init_neo4j(
    schema_service: Neo4jSchemaService = Depends(get_schema_service),
    maintenance_service: Neo4jIndexMaintenance = Depends(get_maintenance_service)
) -> JSONResponse:
    """
    Initialize Neo4j Graph RAG schema.

    This endpoint creates all necessary constraints and indexes for the Graph RAG schema.
    It's designed to be idempotent - can be called multiple times safely.

    Returns:
        JSONResponse: Initialization results
    """
    try:
        # Execute schema initialization
        result = await schema_service.initialize_schema()

        # Ensure vector and community indexes (vector index for embeddings)
        maintenance_result = await maintenance_service.ensure_all_indexes()

        if result["success"]:
            # Extract vector index details
            vector_result = maintenance_result.get("vector", {})
            community_result = maintenance_result.get("community", {})
            constraints_result = maintenance_result.get("constraints", {})
            
            response_data = {
                "status": "Neo4j Graph RAG schema initialized successfully",
                "constraints_created": len(result["constraints"]["executed_queries"]),
                "indexes_created": len(result["indexes"]["executed_queries"]),
                "relationship_types_created": len(result["relationship_types"]["executed_queries"]),
                "failed_queries": result["summary"]["failed"],
                "total_queries": result["summary"]["total_queries"],
                "vector_index_ensured": vector_result.get("success", False),
                "vector_index_name": vector_result.get("name"),
                "vector_dimensions": vector_result.get("dimensions", 1536),
                "similarity_function": vector_result.get("similarity", "cosine"),
                "hnsw_optimization": {
                    "enabled": True,
                    "vector.hnsw.m": 32,
                    "vector.hnsw.ef_construction": 200,
                    "vector.quantization.enabled": True
                },
                "community_indexes_created": len(community_result.get("operations", [])),
                "community_indexes_success": community_result.get("success", False),
                "community_indexes_operations": community_result.get("operations", []),
                "ingestion_constraints_success": constraints_result.get("success", False),
                "ingestion_constraints_applied": len(constraints_result.get("executed", []))
            }

            return JSONResponse(status_code=200, content=response_data)

        # Extract vector index details for error response
        vector_result = maintenance_result.get("vector", {})
        community_result = maintenance_result.get("community", {})
        constraints_result = maintenance_result.get("constraints", {})
        
        response_data = {
            "status": "Neo4j Graph RAG schema initialized with some failures",
            "constraints_created": len(result["constraints"]["executed_queries"]),
            "indexes_created": len(result["indexes"]["executed_queries"]),
            "relationship_types_created": len(result["relationship_types"]["executed_queries"]),
            "failed_queries": result["summary"]["failed"],
            "total_queries": result["summary"]["total_queries"],
            "details": {
                "failed_constraints": result["constraints"]["failed_queries"],
                "failed_indexes": result["indexes"]["failed_queries"],
                "failed_relationship_types": result["relationship_types"]["failed_queries"]
            },
            "vector_index_ensured": vector_result.get("success", False),
            "vector_index_name": vector_result.get("name"),
            "vector_dimensions": vector_result.get("dimensions", 1536),
            "similarity_function": vector_result.get("similarity", "cosine"),
            "hnsw_optimization": {
                "enabled": True,
                "vector.hnsw.m": 32,
                "vector.hnsw.ef_construction": 200,
                "vector.quantization.enabled": True
            },
            "community_indexes_created": len(community_result.get("operations", [])),
            "community_indexes_success": community_result.get("success", False),
            "community_indexes_operations": community_result.get("operations", []),
            "ingestion_constraints_success": constraints_result.get("success", False),
            "ingestion_constraints_applied": len(constraints_result.get("executed", []))
        }

        return JSONResponse(status_code=200, content=response_data)
    except Exception as e:
        error_handler = ErrorHandler()
        return error_handler.format_generic_error(e)

@neo4j_router.get("/schema-info")
async def get_schema_info(
    query_builder: SchemaQueryBuilder = Depends()
) -> Dict[str, Any]:
    """
    Get information about the Neo4j schema.

    Returns:
        Dict[str, Any]: Schema information including constraints, indexes, and relationship types
    """
    return {
        "constraints": query_builder.get_constraint_queries(),
        "indexes": query_builder.get_index_queries(),
        "relationship_type_queries": query_builder.get_relationship_type_queries(),
        "node_types": query_builder.get_node_types(),
        "relationships": query_builder.get_relationship_types(),
        "vector_dimensions": 1536,
        "similarity_function": "cosine",
        "hnsw_optimization": {
            "vector.hnsw.m": 32,
            "vector.hnsw.ef_construction": 200,
            "vector.quantization.enabled": True
        }
    }

# Index Maintenance Endpoints

@neo4j_router.get("/maintenance/health")
async def get_index_health(
    maintenance_service: Neo4jIndexMaintenance = Depends(get_maintenance_service)
) -> Dict[str, Any]:
    """
    Get health status of all vector indexes.

    Returns:
        Dict[str, Any]: Index health status
    """
    health_statuses = await maintenance_service.check_index_health()

    return {
        "status": "success",
        "health_statuses": [
            {
                "index_name": h.index_name,
                "is_healthy": h.is_healthy,
                "last_maintenance": h.last_maintenance.isoformat(),
                "next_maintenance": h.next_maintenance.isoformat(),
                "issues": h.issues,
                "recommendations": h.recommendations
            }
            for h in health_statuses
        ]
    }

app.include_router(neo4j_router)

def reset_neo4j_driver():
    """Reset the Neo4j driver by clearing the cache."""
    get_neo4j_client.cache_clear()
    logger.info("Neo4j driver cache cleared.")

if __name__ == "__main__":
    settings = get_app_settings()
    # This block will only be executed when the script is run directly
    # It's useful for testing the service locally
    uvicorn.run(
        "main:app",
        host="0.0.0.0",
        port=settings.API_PORT,
        reload=True,
        log_level="info",
        reload_dirs=["src"]
    )
