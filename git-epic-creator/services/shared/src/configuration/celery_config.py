"""
Celery configuration settings.
"""

import os
from functools import lru_cache
from typing import Dict, List
from pydantic_settings import BaseSettings
from constants import (
    ROUTE_DOCUMENT_TASKS_PATTERN,
    ROUTE_NEO4J_TASKS_PATTERN,
    QUEUE_DOCUMENT_PROCESSING,
    QUEUE_NEO4J_INGESTION,
)


class CelerySettings(BaseSettings):
    """Celery configuration settings."""

    # Broker and Backend
    CELERY_BROKER_URL: str = os.getenv("CELERY_BROKER_URL", os.getenv("REDIS_URL", "redis://localhost:6379") + "/0")
    CELERY_RESULT_BACKEND: str = os.getenv("CELERY_RESULT_BACKEND", os.getenv("REDIS_URL", "redis://localhost:6379") + "/0")

    # Serialization
    CELERY_TASK_SERIALIZER: str = "json"
    CELERY_RESULT_SERIALIZER: str = "json"
    CELERY_ACCEPT_CONTENT: List[str] = ["json"]

    # Timezone
    CELERY_TIMEZONE: str = "UTC"
    CELERY_ENABLE_UTC: bool = True

    # Task Configuration
    CELERY_TASK_TRACK_STARTED: bool = True
    CELERY_TASK_TIME_LIMIT: int = 300  # 5 minutes
    CELERY_TASK_SOFT_TIME_LIMIT: int = 240  # 4 minutes
    
    # Worker Configuration
    CELERY_WORKER_PREFETCH_MULTIPLIER: int = 1
    CELERY_WORKER_MAX_TASKS_PER_CHILD: int = 1000
    CELERY_WORKER_CONCURRENCY: int = 4

    # Broker transport options
    # Visibility timeout for in-flight tasks (seconds). Keep modest to enable timely redelivery on worker loss.
    CELERY_BROKER_VISIBILITY_TIMEOUT: int = 60
    
    # Health Check Configuration
    CELERY_HEALTH_CHECK_PING_TIMEOUT: int = 5  # Seconds to wait for worker ping response
    CELERY_HEALTH_CHECK_INSPECTOR_TIMEOUT: int = 5  # Seconds to wait for inspector calls (increased from 1s for busy workers)
    CELERY_HEALTH_CHECK_LONG_RUNNING_THRESHOLD: int = 60  # Seconds after which a task is considered long-running

    # Task Routing (centralized)
    CELERY_TASK_ROUTES: Dict[str, Dict[str, str]] = {
        ROUTE_DOCUMENT_TASKS_PATTERN: {'queue': QUEUE_DOCUMENT_PROCESSING},
        ROUTE_NEO4J_TASKS_PATTERN: {'queue': QUEUE_NEO4J_INGESTION},
    }

    model_config = {
        "env_file": ".env",
        "case_sensitive": True
    }


@lru_cache()
def get_celery_settings() -> CelerySettings:
    """Get Celery settings instance."""
    return CelerySettings()
