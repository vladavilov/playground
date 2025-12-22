import threading
import multiprocessing as mp
import uvicorn
import structlog
from fastapi import APIRouter

from configuration.logging_config import configure_logging
from utils.app_factory import FastAPIFactory
from worker.celery_app import celery_app, get_task_validation_status
from shared.src.utils.celery_factory import CeleryHealthChecker  # reuse shared checker
from configuration.common_config import get_app_settings

# Force spawn to avoid fork with LanceDB
try:
    mp.set_start_method("spawn", force=True)
except Exception:
    pass

# Configure logging at application startup
configure_logging()
logger = structlog.get_logger(__name__)

app = FastAPIFactory.create_app(
    title="Neo4j Graph Ingestion Service",
    description="A microservice to ingest the graph via neo4j-repository-service.",
    version="1.0.0",
    enable_cors=True,
    enable_neo4j=False,
    enable_redis=True,
)

router = APIRouter(prefix="/health", tags=["Health"])

@router.get("/celery")
def celery_health():
    try:
        details = CeleryHealthChecker.check_health_with_details(celery_app)
        status = get_task_validation_status()
        details.update({
            "service": "neo4j_ingestion_service",
            "task_validation_status": status,
        })
        return details
    except Exception as e:
        logger.error("Celery health check failed", error=str(e))
        return {
            "healthy": False,
            "service": "neo4j_ingestion_service",
            "error": str(e),
        }

app.include_router(router)


def start_worker() -> None:
    try:
        # Give local worker a unique hostname to avoid DuplicateNodenameWarning
        # Include process id to ensure uniqueness across multiple runs
        from os import getpid, name as os_name, getenv
        suffix = getenv("WORKER_SUFFIX") or str(getpid())
        unique_host = f"neo4j-ingestor-local@{suffix}"
        args = [
            "worker",
            "--loglevel=info",
            "--concurrency=1",
            f"--hostname={unique_host}",
            "--queues=neo4j_ingestion,neo4j_ingestion_dlq",
            "--prefetch-multiplier=1",
        ]
        # On Windows, prefer 'solo' pool to avoid unsupported features and improve stability
        if os_name == "nt":
            args.append("--pool=solo")
        else:
            args.append("--pool=threads")
        # Avoid cluster mingle/gossip to prevent DuplicateNodenameWarning noise when other workers exist
        args.append("--without-mingle")
        args.append("--without-gossip")
        celery_app.worker_main(args)
    except Exception as e:
        logger.error("Failed to start Celery worker", error=str(e))
        raise


def start_fastapi_server() -> None:
    try:
        
        settings = get_app_settings()
        uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT, log_config=None, access_log=False)
    except Exception as e:
        logger.error("Failed to start FastAPI server", error=str(e))
        raise

def start_service() -> None:
    try:
        logger.info("Starting Neo4j Ingestion Service with FastAPI and Celery")
        fastapi_thread = threading.Thread(target=start_fastapi_server, name="FastAPI-Server", daemon=True)
        celery_thread = threading.Thread(target=start_worker, name="Celery-Worker", daemon=True)
        fastapi_thread.start()
        celery_thread.start()
        fastapi_thread.join()
        celery_thread.join()
    except Exception as e:
        logger.error("Service startup failed", error=str(e))
        raise

if __name__ == "__main__":
    start_service()
