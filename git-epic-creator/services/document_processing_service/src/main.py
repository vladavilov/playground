"""Entry point that starts FastAPI, Celery worker, and task subscriber with health endpoints."""

import threading
import uvicorn
import structlog
import os
from typing import Dict, Any
from fastapi import Depends, Request, APIRouter
from celery import Celery
from celery_worker_app import celery_app, get_task_validation_status, get_initialization_status, get_processors_health

from configuration.logging_config import configure_logging

# Initialize logging and configuration before all other modules (using logger) are initialized
configure_logging()
from configuration.common_config import get_app_settings
from utils.celery_factory import CeleryHealthChecker
from utils.app_factory import FastAPIFactory

logger = structlog.get_logger(__name__)
settings = get_app_settings()


app = FastAPIFactory.create_app(
    title="Document Processing Service",
    description="A microservice for processing documents with Celery tasks",
    version="1.0.0",
    enable_cors=True
)

# Store Celery app in FastAPI app state for dependency injection
app.state.celery_app = celery_app

def get_celery_app_from_state(request: Request) -> Celery:
    """
    Dependency to get Celery app from FastAPI app state.
    
    Args:
        request: FastAPI request object containing app state
        
    Returns:
        Celery: Celery application instance
    """
    return request.app.state.celery_app

# Create API router for Celery-specific endpoints
celery_router = APIRouter(prefix="/health", tags=["Health"])

@celery_router.get("/celery")
def celery_health_check(
    scoped_celery_app: Celery = Depends(get_celery_app_from_state)
) -> Dict[str, Any]:
    """
    Comprehensive health check endpoint that returns detailed health information.
    
    Checks:
    - Basic service health
    - Celery worker status
    - Docling processor initialization and runtime health (VLM mode, OCR, RapidOCR models)
    - Tika processor initialization and runtime health (server availability, version)
    
    The service is considered unhealthy if any critical component fails.
    Each processor (Docling and Tika) is checked independently and reported separately.
    
    Args:
        scoped_celery_app: Celery application instance from dependency injection
        
    Returns:
        Dict[str, Any]: Comprehensive health check response with separate component statuses:
            - components.docling_initialization: Docling startup status
            - components.docling: Docling runtime health check
            - components.tika_initialization: Tika startup status
            - components.tika: Tika runtime health check
            - components.celery: Celery worker status
    """
    overall_healthy = True
    health_response = {
        "service": "Document Processing Service",
        "healthy": True,
        "components": {}
    }
    
    try:
        # 1. Basic service health
        health_response["components"]["basic"] = {
            "healthy": True,
            "status": "ok"
        }
        
        # 2. Check processor initialization status
        init_status = get_initialization_status()
        
        # Report separate Docling and Tika initialization status
        health_response["components"]["docling_initialization"] = init_status.get("docling", {})
        health_response["components"]["tika_initialization"] = init_status.get("tika", {})
        
        if not init_status.get("healthy", False):
            overall_healthy = False
            health_response["processor_initialization_failed"] = True
        
        # 3. Check Celery worker status
        health_checker = CeleryHealthChecker()
        celery_status = health_checker.check_health_with_details(scoped_celery_app)
        
        health_response["components"]["celery"] = {
            "healthy": celery_status.get("healthy", False),
            "celery_app_name": scoped_celery_app.main,
            "task_validation_status": get_task_validation_status(),
            "registered_tasks_count": len(scoped_celery_app.tasks),
            "broker_url": scoped_celery_app.conf.broker_url,
            "result_backend": scoped_celery_app.conf.result_backend,
            "worker_queues": ["document_processing"],
            "task_serializer": scoped_celery_app.conf.task_serializer,
            "result_serializer": scoped_celery_app.conf.result_serializer
        }
        
        if not celery_status.get("healthy", False):
            overall_healthy = False
        
        # 4. Check Docling and Tika processor runtime health (uses singleton instances)
        processors_health = get_processors_health()
        
        health_response["components"]["docling"] = processors_health["docling"]
        health_response["components"]["tika"] = processors_health["tika"]
        
        # Update overall health based on processor health
        if not processors_health["docling"].get("healthy", False):
            overall_healthy = False
        if not processors_health["tika"].get("healthy", False):
            overall_healthy = False
        
        # Set overall health status
        health_response["healthy"] = overall_healthy
        
        logger.info("Comprehensive health check completed", 
                    overall_healthy=overall_healthy,
                    docling_healthy=processors_health["docling"].get("healthy", False),
                    tika_healthy=processors_health["tika"].get("healthy", False))
        return health_response
        
    except Exception as e:
        error_msg = f"Health check failed: {str(e)}"
        logger.error("Health check error", error=error_msg, exc_info=True)
        return {
            "healthy": False,
            "error": error_msg,
            "service": "Document Processing Service"
        }

app.include_router(celery_router)

def configure_thread_logging(thread_name: str):
    """Deprecated: threads reuse module logger."""
    return logger

def start_worker():
    """
    Start Celery worker for document processing tasks.
    Runs in a separate thread and handles task execution.
    
    Uses platform-aware pool selection:
    - prefork (Linux/Unix): Multiple processes, true parallelism, isolated memory
    - threads (Windows): Multiple threads, shared memory, Windows-compatible
    
    This fixes health check issues where solo pool blocked inspector calls
    during long-running document processing tasks.
    """
    try:
        # Platform-aware pool selection
        # prefork: Best for Linux/Docker - true process isolation
        # threads: Best for Windows dev - handles signals properly
        pool_type = 'threads' if os.name == 'nt' else 'prefork'
        
        worker_args = [
            'worker',
            '--loglevel=info',
            '--concurrency=2',
            f'--pool={pool_type}',
            '--hostname=document-processor@%h',
            '--queues=document_processing',
            '--prefetch-multiplier=1',
            '--max-tasks-per-child=10',  # Restart workers periodically to prevent memory leaks
        ]
        
        logger.info("CELERY_WORKER_STARTING", 
                   pool=pool_type, 
                   concurrency=2,
                   platform=os.name,
                   note="Using prefork/threads pool for concurrent processing and health check responsiveness")
        
        celery_app.worker_main(worker_args)
        logger.debug("Celery worker started successfully")
    except Exception as e:
        error_msg = str(e)
        logger.error("Failed to start Celery worker", error=error_msg, exc_info=True)
        raise

def start_fastapi_server():
    """
    Start FastAPI server for health monitoring and API endpoints.
    Runs in a separate thread and provides HTTP interface.
    
    Raises:
        Exception: If FastAPI server startup fails
    """
    try:
        logger.debug("Starting FastAPI server for health monitoring", port=settings.API_PORT)
        uvicorn.run(
            app, 
            host="0.0.0.0", 
            port=settings.API_PORT, 
            log_config=None,
            access_log=False
        )
        logger.info("FastAPI server started successfully")
    except Exception as e:
        error_msg = str(e)
        logger.error("Failed to start FastAPI server", error=error_msg)
        raise

def start_service():
    """
    Start FastAPI server, Celery worker, and TaskRequestSubscriber concurrently using threading.
    
    This function creates separate threads for:
    1. FastAPI server for health monitoring and API endpoints
    2. Celery worker for document processing tasks
    3. TaskRequestSubscriber for Redis task request handling
    
    All services run concurrently in the same process.
    
    Raises:
        Exception: If any service startup fails
    """
    try:
        logger.info("Starting Document Processing Service with FastAPI and Celery")

        fastapi_thread = threading.Thread(target=start_fastapi_server, name="FastAPI-Server")
        celery_thread = threading.Thread(target=start_worker, name="Celery-Worker")

        # Set as daemon threads so they terminate when main process exits
        fastapi_thread.daemon = True
        celery_thread.daemon = True

        # Start all threads
        fastapi_thread.start()
        celery_thread.start()

        logger.info("All service threads started (FastAPI, Celery, TaskSubscriber)")

        # Wait for all threads to complete
        fastapi_thread.join()
        celery_thread.join()
        # no Redis Streams subscriber; tasks are triggered directly via Celery broker

        logger.info("Document Processing Service shutdown completed")

    except Exception as e:
        error_msg = f"Service startup failed: {str(e)}"
        logger.error("Failed to start Document Processing Service with subscriber", error=error_msg)
        raise Exception(error_msg) from e


if __name__ == "__main__":
    start_service()
