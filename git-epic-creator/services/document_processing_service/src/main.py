"""Entry point that starts FastAPI, Celery worker, and task subscriber with health endpoints."""

import threading
import uvicorn
import structlog
from typing import Dict, Any
from fastapi import Depends, Request, APIRouter
from celery import Celery
from celery_worker_app import celery_app, get_task_validation_status, get_initialization_status

from configuration.logging_config import configure_logging

# Initialize logging and configuration before all other modules (using logger) are initialized
configure_logging()
from configuration.common_config import get_app_settings
from utils.celery_factory import CeleryHealthChecker
from utils.app_factory import FastAPIFactory
from services.tika_processor import TikaProcessor

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
    Celery health check endpoint that returns detailed health information.
    
    Checks both Celery worker status AND processor initialization status.
    The service is considered unhealthy if processor initialization failed.
    
    Args:
        scoped_celery_app: Celery application instance from dependency injection
        
    Returns:
        Dict[str, Any]: Health check response with Celery and processor status
    """
    try:
        # Check processor initialization status first
        init_status = get_initialization_status()
        
        health_checker = CeleryHealthChecker()
        health_status = health_checker.check_health_with_details(scoped_celery_app)
        
        # Override healthy status if processors failed to initialize
        if not init_status["healthy"]:
            health_status["healthy"] = False
            health_status["processor_initialization_failed"] = True
        
        health_status.update({
            "service": "Document Processing Service",
            "celery_app_name": scoped_celery_app.main,
            "processor_initialization": init_status,
            "task_validation_status": get_task_validation_status(),
            "active_tasks": list(scoped_celery_app.tasks.keys()),
            "registered_tasks_count": len(scoped_celery_app.tasks),
            "broker_url": scoped_celery_app.conf.broker_url,
            "result_backend": scoped_celery_app.conf.result_backend,
            "task_routes": scoped_celery_app.conf.task_routes,
            "worker_queues": ["document_processing"],  # Expected worker queues
            "task_serializer": scoped_celery_app.conf.task_serializer,
            "result_serializer": scoped_celery_app.conf.result_serializer
        })
        
        logger.debug("Celery health check completed", 
                    status=health_status.get("healthy"),
                    processor_healthy=init_status["healthy"])
        return health_status
        
    except Exception as e:
        error_msg = f"Celery health check failed: {str(e)}"
        logger.error("Celery health check error", error=error_msg, exc_info=True)
        return {
            "healthy": False,
            "error": error_msg,
            "service": "Document Processing Service"
        }

@celery_router.get("/tika")
def tika_health_check() -> Dict[str, Any]:
    """
    Tika health check endpoint that returns Tika processor status.
    
    Also checks processor initialization status.
    
    Returns:
        Dict[str, Any]: Health check response with Tika status
    """
    try:
        # Check processor initialization status first
        init_status = get_initialization_status()
        
        # If initialization failed, return unhealthy immediately
        if not init_status["healthy"]:
            return {
                "healthy": False,
                "service": "Document Processing Service",
                "component": "Tika Processor",
                "processor_initialization_failed": True,
                "initialization_error": init_status.get("error"),
                "error_type": init_status.get("error_type")
            }
        
        tika_processor = TikaProcessor()
        
        health_status = tika_processor.check_health()
        
        health_status.update({
            "service": "Document Processing Service",
            "component": "Tika Processor",
            "processor_initialization": init_status
        })
        
        logger.debug("Tika health check completed", status=health_status.get("healthy"))
        return health_status
        
    except Exception as e:
        error_msg = f"Tika health check failed: {str(e)}"
        logger.error("Tika health check error", error=error_msg, exc_info=True)
        return {
            "healthy": False,
            "error": error_msg,
            "service": "Document Processing Service",
            "component": "Tika Processor"
        }

app.include_router(celery_router)

def configure_thread_logging(thread_name: str):
    """Deprecated: threads reuse module logger."""
    return logger

def start_worker():
    """
    Start Celery worker for document processing tasks.
    Runs in a separate thread and handles task execution.
    """
    try:
        celery_app.worker_main([
            'worker',
            '--loglevel=info',
            '--concurrency=2',
            '--pool=solo',
            '--hostname=document-processor@%h',
            '--queues=document_processing',
            '--prefetch-multiplier=1'
        ])
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
