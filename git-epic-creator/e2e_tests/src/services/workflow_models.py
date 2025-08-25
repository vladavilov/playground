from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict

from services.redis_test_monitor import RedisTestMonitor


@dataclass
class WorkflowTestFixtures:
    """Container for test fixtures to reduce method argument count."""
    service_urls: Dict[str, str]
    auth_headers: Dict[str, str]
    postgres_connection: Any
    redis_config: Dict[str, Any]
    test_project_data: Dict[str, Any]
    test_pdf_content: bytes
    unique_test_filename: str
    project_manager: Any
    redis_monitor: RedisTestMonitor
    neo4j_driver: Any = None
