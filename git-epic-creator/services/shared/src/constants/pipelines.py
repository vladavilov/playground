"""Centralized Celery pipeline constants for apps, tasks, queues, and routing.

This module provides a single source of truth for Celery-related identifiers
across services so that publishers and workers remain consistent and traceable.
"""

# Celery app names (per service)
APP_NAME_DOCUMENT_PROCESSING = "document_processing_service"
APP_NAME_NEO4J_INGESTION = "neo4j_ingestion_service"
APP_NAME_PROJECT_MANAGEMENT = "project_management_service"

# Queue names
QUEUE_DOCUMENT_PROCESSING = "document_processing"
QUEUE_NEO4J_INGESTION = "neo4j_ingestion"
QUEUE_NEO4J_INGESTION_DLQ = "neo4j_ingestion_dlq"

# Task routing patterns → queues
ROUTE_DOCUMENT_TASKS_PATTERN = "tasks.document_tasks.*"
ROUTE_NEO4J_TASKS_PATTERN = "tasks.neo4j_ingestion.*"

# Fully-qualified task names
TASK_PROCESS_PROJECT_DOCS = "tasks.document_tasks.process_project_documents_task"
TASK_RUN_GRAPHRAG_JOB = "tasks.neo4j_ingestion.run_graphrag_job"

# Health/validation expectations per service
EXPECTED_TASKS_DOCUMENT = [TASK_PROCESS_PROJECT_DOCS]
EXPECTED_TASKS_INGESTION = [TASK_RUN_GRAPHRAG_JOB]

# Gating/locking namespaces and common TTL defaults
GATE_NS_DOCS = "docproc"
GATE_NS_INGESTION = "graphrag"

# Defaults used by gating helpers (can be overridden at call sites)
GATE_DEFAULT_RUNNING_TTL = 300
GATE_DEFAULT_PENDING_TTL = 60
GATE_DEFAULT_COALESCE_TTL = 5

__all__ = [
    # Apps
    "APP_NAME_DOCUMENT_PROCESSING",
    "APP_NAME_NEO4J_INGESTION",
    "APP_NAME_PROJECT_MANAGEMENT",
    # Queues
    "QUEUE_DOCUMENT_PROCESSING",
    "QUEUE_NEO4J_INGESTION",
    "QUEUE_NEO4J_INGESTION_DLQ",
    # Routes
    "ROUTE_DOCUMENT_TASKS_PATTERN",
    "ROUTE_NEO4J_TASKS_PATTERN",
    # Tasks
    "TASK_PROCESS_PROJECT_DOCS",
    "TASK_RUN_GRAPHRAG_JOB",
    # Expected / health
    "EXPECTED_TASKS_DOCUMENT",
    "EXPECTED_TASKS_INGESTION",
    # Gating
    "GATE_NS_DOCS",
    "GATE_NS_INGESTION",
    "GATE_DEFAULT_RUNNING_TTL",
    "GATE_DEFAULT_PENDING_TTL",
    "GATE_DEFAULT_COALESCE_TTL",
]


