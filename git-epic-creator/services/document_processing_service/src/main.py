"""
Celery worker and FastAPI server entry point for Document Processing Service.

This module configures and starts both:
1. Celery workers for processing document tasks
2. FastAPI server for health monitoring and API endpoints

It uses the enhanced CeleryFactory to create a worker application with automatic
task module discovery and proper error handling, alongside a FastAPI application
for health checks and monitoring.

Enhanced with comprehensive logging and monitoring for:
- Worker startup and shutdown events
- Task execution lifecycle (start, success, failure, retry)
- Structured logging with consistent format
- Error handling and monitoring
- Health check endpoints for system monitoring
"""

import threading
import uvicorn
import asyncio
import structlog
from typing import Dict, Any
from fastapi import Depends, Request, APIRouter
from celery import Celery

# Import configuration and utilities
from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings
from utils.celery_factory import get_celery_app, CeleryHealthChecker
from utils.app_factory import FastAPIFactory
from utils.unified_redis_messages import create_redis_message_factory
from utils.redis_client import get_redis_client

# Import signal handlers from celery_factory to make them available in main module
from utils.celery_factory import (
    _log_task_prerun,
    _log_task_postrun, 
    _log_task_failure,
    _log_task_retry,
    _log_worker_ready
)

# Initialize logging and configuration
configure_logging()
logger = structlog.get_logger(__name__)
settings = get_app_settings()

# Create celery app with proper error handling
try:
    celery_app = get_celery_app(name="document_processing_service")
except Exception as e:
    logger.error("Failed to initialize Celery app", error=str(e))
    raise

# Create FastAPI app using the factory pattern
app = FastAPIFactory.create_app(
    title="Document Processing Service",
    description="A microservice for processing documents with Celery tasks",
    version="1.0.0",
    enable_azure_auth=False,
    enable_docs_auth=False,
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
    celery_app: Celery = Depends(get_celery_app_from_state)
) -> Dict[str, Any]:
    """
    Celery health check endpoint that returns detailed health information.
    
    Args:
        celery_app: Celery application instance from dependency injection
        
    Returns:
        Dict[str, Any]: Comprehensive health check results including:
            - Connection status to broker and backend
            - Active workers count and details
            - Active, scheduled, and registered tasks
            - Worker statistics and performance metrics
    """
    try:
        health_data = CeleryHealthChecker.check_health_with_details(celery_app)
        logger.info("Celery health check completed", healthy=health_data.get("healthy", False))
        return health_data
    except Exception as e:
        # Safely access configuration with fallback values
        # Handle cases where conf object itself might not exist or raise AttributeError
        broker_url = 'unavailable'
        backend_url = 'unavailable'

        try:
            # Try to access conf and its attributes with comprehensive error handling
            conf = celery_app.conf
            if conf is not None:
                broker_url = getattr(conf, 'broker_url', 'unavailable')
        except (AttributeError, TypeError):
            # celery_app.conf doesn't exist, can't be accessed, or getattr fails
            broker_url = 'unavailable'

        try:
            # Try to access conf and its attributes with comprehensive error handling
            conf = celery_app.conf
            if conf is not None:
                backend_url = getattr(conf, 'result_backend', 'unavailable')
        except (AttributeError, TypeError):
            # celery_app.conf doesn't exist, can't be accessed, or getattr fails
            backend_url = 'unavailable'
 
        error_result = {
            "healthy": False,
            "error": str(e),
            "broker_url": broker_url,
            "backend_url": backend_url
        }
        logger.error("Celery health check failed", error=str(e))
        return error_result

# Include the Celery router in the FastAPI app
app.include_router(celery_router)

def start_worker():
    """
    Start the Celery worker with enhanced monitoring and error handling.
    
    Raises:
        Exception: If worker startup fails
    """
    try:
        logger.info("Starting Celery worker with enhanced monitoring")
        worker = celery_app.Worker()
        worker.start()
        logger.info("Celery worker started successfully")
    except Exception as e:
        error_msg = str(e)
        logger.error("Failed to start Celery worker", error=error_msg)
        raise

def start_fastapi_server():
    """
    Start the FastAPI server for health monitoring and API endpoints.
    
    Raises:
        Exception: If FastAPI server startup fails
    """
    try:
        logger.info("Starting FastAPI server for health monitoring", port=settings.API_PORT)
        uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT, log_config=None)
        logger.info("FastAPI server started successfully")
    except Exception as e:
        error_msg = str(e)
        logger.error("Failed to start FastAPI server", error=error_msg)
        raise

def start_service():
    """
    Start both FastAPI server and Celery worker concurrently using threading.
    
    This function creates separate threads for:
    1. FastAPI server for health monitoring and API endpoints
    2. Celery worker for document processing tasks
    
    Both services run concurrently in the same process.
    
    Raises:
        Exception: If either service startup fails
    """
    try:
        logger.info("Starting Document Processing Service with both FastAPI and Celery")
        
        # Create threads for concurrent execution
        fastapi_thread = threading.Thread(target=start_fastapi_server, name="FastAPI-Server")
        celery_thread = threading.Thread(target=start_worker, name="Celery-Worker")
        
        # Set as daemon threads so they terminate when main process exits
        fastapi_thread.daemon = True
        celery_thread.daemon = True
        
        # Start both threads
        fastapi_thread.start()
        celery_thread.start()
        
        logger.info("Both FastAPI server and Celery worker threads started")
        
        # Wait for both threads to complete
        fastapi_thread.join()
        celery_thread.join()
        
        logger.info("Document Processing Service shutdown completed")
        
    except Exception as e:
        error_msg = f"Service startup failed: {str(e)}"
        logger.error("Failed to start Document Processing Service", error=error_msg)
        raise Exception(error_msg)


def start_task_subscriber():
    """
    Start TaskRequestSubscriber to listen for Redis task requests.
    Runs in a separate thread and handles Redis pub/sub messages.
    """
    try:
        logger.info("Starting TaskRequestSubscriber")
        
        # Create subscriber using factory method
        redis_client = get_redis_client()
        _, _, subscriber = create_redis_message_factory(redis_client)
        
        # Run the async listener in the thread
        asyncio.run(subscriber.start_listening())
        
    except Exception as e:
        logger.error("TaskRequestSubscriber failed", error=str(e), exc_info=True)


def start_service_with_subscriber():
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
        logger.info("Starting Document Processing Service with FastAPI, Celery, and TaskRequestSubscriber")
        
        # Create threads for concurrent execution
        fastapi_thread = threading.Thread(target=start_fastapi_server, name="FastAPI-Server")
        celery_thread = threading.Thread(target=start_worker, name="Celery-Worker")
        subscriber_thread = threading.Thread(target=start_task_subscriber, name="TaskSubscriber")
        
        # Set as daemon threads so they terminate when main process exits
        fastapi_thread.daemon = True
        celery_thread.daemon = True
        subscriber_thread.daemon = True
        
        # Start all threads
        fastapi_thread.start()
        celery_thread.start()
        subscriber_thread.start()
        
        logger.info("All service threads started (FastAPI, Celery, TaskSubscriber)")
        
        # Wait for all threads to complete
        fastapi_thread.join()
        celery_thread.join()
        subscriber_thread.join()
        
        logger.info("Document Processing Service shutdown completed")
        
    except Exception as e:
        error_msg = f"Service startup failed: {str(e)}"
        logger.error("Failed to start Document Processing Service with subscriber", error=error_msg)
        raise Exception(error_msg)


if __name__ == "__main__":
    start_service_with_subscriber()
