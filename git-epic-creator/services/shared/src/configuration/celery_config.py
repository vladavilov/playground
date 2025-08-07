"""
Celery configuration settings.
"""

import os
from functools import lru_cache
from typing import Dict, List
from pydantic_settings import BaseSettings


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

    # Task Routing
    CELERY_TASK_ROUTES: Dict[str, Dict[str, str]] = {
        'tasks.document_tasks.*': {'queue': 'document_processing'},
        'tasks.project_tasks.*': {'queue': 'project_management'},
    }

    model_config = {
        "env_file": ".env",
        "case_sensitive": True
    }


@lru_cache()
def get_celery_settings() -> CelerySettings:
    """Get Celery settings instance."""
    return CelerySettings()
