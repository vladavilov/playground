"""
Project status monitoring for e2e tests.

This module provides status polling operations following
Single Responsibility Principle.
"""

from __future__ import annotations

import time
from typing import Dict, Any

import requests

from config import TestConstants
from services.workflow_models import WorkflowTestFixtures
from shared_utils import ProjectTestUtils


class StatusMonitor:
    """
    Project status monitoring operations.
    
    Provides operations for:
    - API status polling
    - Expected UI sequence generation
    """
    
    @staticmethod
    def wait_for_api_status(
        project_id: str,
        fixtures: WorkflowTestFixtures,
        expected_status: str,
        *,
        timeout: int = TestConstants.DOCUMENT_PROCESSING_TIMEOUT,
        interval: float = 1.0,
    ) -> Dict[str, Any]:
        """
        Poll the Project Management API until project reaches expected status.
        
        Args:
            project_id: Project UUID to monitor
            fixtures: Test fixtures with service URLs and auth
            expected_status: Target status to wait for
            timeout: Maximum time to wait in seconds
            interval: Polling interval in seconds
            
        Returns:
            Final project response JSON
            
        Raises:
            AssertionError: If timeout reached without achieving expected status
        """
        deadline = time.time() + timeout
        last: Dict[str, Any] = {}
        
        while time.time() < deadline:
            try:
                project_url = ProjectTestUtils.build_project_url(
                    fixtures.service_urls['project_management'], project_id
                )
                resp = requests.get(
                    project_url,
                    headers=fixtures.auth_headers,
                    verify=False,
                    timeout=TestConstants.DEFAULT_TIMEOUT,
                )
                if resp.status_code == TestConstants.HTTP_OK:
                    last = resp.json()
                    if last.get("status") == expected_status:
                        return last
            except requests.RequestException:
                pass
            time.sleep(interval)
        
        assert last.get("status") == expected_status, (
            f"API status did not reach '{expected_status}', last='{last.get('status')}'"
        )
        return last
    
    @staticmethod
    def expected_ui_sequence() -> list:
        """
        Standard expected UI status/progress messages for a full document processing run.
        
        Returns:
            List of expected status transitions including process_step tuples
        """
        return [
            TestConstants.PROJECT_STATUS_PROCESSING,
            TestConstants.PROJECT_STATUS_ACTIVE,
            TestConstants.PROJECT_STATUS_RAG_PROCESSING,
            (TestConstants.PROJECT_STATUS_RAG_PROCESSING, "Pipeline started"),
            (TestConstants.PROJECT_STATUS_RAG_PROCESSING, "Pipeline finished"),
            TestConstants.PROJECT_STATUS_RAG_READY,
        ]

