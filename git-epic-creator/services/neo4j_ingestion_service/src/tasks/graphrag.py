"""
Celery tasks for Neo4j ingestion.

Thin wrappers that delegate orchestration to the service layer.
"""

from __future__ import annotations

import os
import time
from typing import Any, Dict

import structlog
logger = structlog.get_logger(__name__)

from worker.celery_app import celery_app
from services.ingestion_service import Neo4jIngestionService
from utils.neo4j_client import get_neo4j_client

@celery_app.task(
    bind=True,
    name="tasks.neo4j_ingestion.run_graphrag_job",
    acks_late=True,
)
def run_graphrag_job(
    self,
    job_id: str,
    project_id: str,
    attempts: int = 0,
) -> Dict[str, Any]:
    logger.info(
        "TASK START - run_graphrag_job",
        job_id=job_id,
        project_id=project_id,
        attempts=attempts,
        task_id=self.request.id,
        worker_pid=os.getpid(),
    )
    start = time.time()

    if not isinstance(job_id, str) or not job_id.strip():
        raise ValueError("job_id must be a non-empty string")
    if not isinstance(project_id, str) or not project_id.strip():
        raise ValueError("project_id must be a non-empty string")

    service = Neo4jIngestionService(client=get_neo4j_client())
    service_result = service.run_graphrag_pipeline(project_id=project_id)

    result = {
        "job_id": job_id,
        "project_id": project_id,
        "attempts": attempts,
        "counts": service_result.get("counts", {}),
        "duration_ms": service_result.get("duration_ms", round((time.time() - start) * 1000, 2)),
    }
    return result
