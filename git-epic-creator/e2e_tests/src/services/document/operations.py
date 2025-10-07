"""
Document operations for e2e tests.

This module provides document upload and processing operations following
Single Responsibility Principle.
"""

from __future__ import annotations

import io
import time
from typing import Dict, Any, Tuple

import requests

from config import TestConstants
from services.redis_test_monitor import RedisTestMonitor
from services.workflow_models import WorkflowTestFixtures
from shared_utils import ProjectTestUtils


class DocumentOperations:
    """
    Document upload and processing operations.
    
    Provides operations for:
    - Single document upload
    - Multiple document upload preparation
    - Task monitoring during upload
    - Upload response validation
    """
    
    @staticmethod
    def upload_document(
        project_id: str,
        fixtures: WorkflowTestFixtures,
        task_monitor: RedisTestMonitor
    ) -> Dict[str, Any]:
        """
        Upload a document to a project and verify task publishing.
        
        Args:
            project_id: Target project UUID
            fixtures: Test fixtures with PDF content and credentials
            task_monitor: Redis monitor for task queue verification
            
        Returns:
            Upload response JSON
            
        Raises:
            AssertionError: If upload fails or task publishing verification fails
        """
        initial_task_count = task_monitor.get_current_task_count()
        initial_queue_count = task_monitor.get_current_task_count()
        
        files = {
            'files': (
                fixtures.unique_test_filename,
                io.BytesIO(fixtures.test_pdf_content),
                'application/pdf'
            )
        }
        
        upload_url = ProjectTestUtils.build_upload_url(
            fixtures.service_urls['project_management'], project_id
        )
        
        upload_response = requests.post(
            upload_url,
            files=files,
            headers=fixtures.auth_headers,
            timeout=30,
            verify=False
        )
        
        assert upload_response.status_code == TestConstants.HTTP_OK, (
            f"Failed to upload document: {upload_response.text}"
        )
        
        time.sleep(2)
        
        verification_results = task_monitor.verify_task_published_comprehensive(
            project_id=project_id,
            initial_stream_count=initial_queue_count,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        
        verification_passed = (
            verification_results['verification_successful'] or
            verification_results.get('queue_increased') or
            task_monitor.get_current_task_count() > initial_task_count
        )
        
        assert verification_passed, (
            f"Expected task publishing verification to pass after document upload. "
            f"Verification results: {verification_results}. "
            f"Queue length: {initial_task_count} -> {task_monitor.get_current_task_count()}"
        )
        
        return upload_response.json()
    
    @staticmethod
    def prepare_multiple_files(
        project_id: str,
        test_pdf_content: bytes
    ) -> Tuple[list, list]:
        """
        Prepare multiple files for batch upload testing.
        
        Args:
            project_id: Project UUID (used for unique filenames)
            test_pdf_content: PDF content bytes
            
        Returns:
            Tuple of (files list, expected_filenames list)
        """
        files = []
        expected_filenames = []
        for i in range(3):
            filename = f"test_document_{i}_{project_id[:8]}.pdf"
            expected_filenames.append(filename)
            files.append(('files', (filename, io.BytesIO(test_pdf_content), 'application/pdf')))
        return files, expected_filenames
    
    @staticmethod
    def verify_upload_response(
        project_id: str,
        upload_result: Dict[str, Any],
        fixtures: WorkflowTestFixtures
    ) -> None:
        """
        Verify single document upload response structure and content.
        
        Args:
            project_id: Expected project UUID
            upload_result: Upload response JSON
            fixtures: Test fixtures with expected filename
            
        Raises:
            AssertionError: If response doesn't match expected structure
        """
        assert upload_result["project_id"] == project_id
        assert upload_result["total_files"] == 1
        assert upload_result["successful_uploads"] == 1
        assert upload_result["failed_uploads"] == 0
        assert upload_result["processing_initiated"] is True
        assert fixtures.unique_test_filename in upload_result["uploaded_files"]
    
    @staticmethod
    def verify_multiple_upload_response(
        upload_result: Dict[str, Any],
        project_id: str,
        expected_filenames: list
    ) -> None:
        """
        Verify multiple document upload response structure and content.
        
        Args:
            upload_result: Upload response JSON
            project_id: Expected project UUID
            expected_filenames: List of expected filenames
            
        Raises:
            AssertionError: If response doesn't match expected structure
        """
        assert upload_result["project_id"] == project_id
        assert upload_result["total_files"] == 3
        assert upload_result["successful_uploads"] == 3
        assert upload_result["failed_uploads"] == 0
        assert upload_result["processing_initiated"] is True
        for filename in expected_filenames:
            assert filename in upload_result["uploaded_files"]
    
    @staticmethod
    def assert_upload_without_files(
        project_id: str,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str]
    ) -> None:
        """
        Verify error handling for upload without files.
        
        Args:
            project_id: Target project UUID
            service_urls: Service URL configuration
            auth_headers: Authentication headers
            
        Raises:
            AssertionError: If error handling is incorrect
        """
        upload_url = ProjectTestUtils.build_upload_url(
            service_urls['project_management'], project_id
        )
        upload_response = requests.post(
            upload_url,
            headers=auth_headers,
            verify=False,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        assert upload_response.status_code in [400, 422], (
            "Should return error for upload without files"
        )
    
    @staticmethod
    def assert_upload_to_nonexistent_project(
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str]
    ) -> None:
        """
        Verify error handling for upload to non-existent project.
        
        Args:
            service_urls: Service URL configuration
            auth_headers: Authentication headers
            
        Raises:
            AssertionError: If error handling is incorrect
        """
        fake_project_id = "ffffffff-ffff-ffff-ffff-ffffffffffff"
        files = {
            'files': ("test.pdf", io.BytesIO(b"fake pdf content"), 'application/pdf')
        }
        upload_url = ProjectTestUtils.build_upload_url(
            service_urls['project_management'], fake_project_id
        )
        upload_response = requests.post(
            upload_url,
            files=files,
            headers=auth_headers,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        assert upload_response.status_code == TestConstants.HTTP_NOT_FOUND, (
            f"Should return 404 for non-existent project, got {upload_response.status_code}. "
            f"Response: {upload_response.text}"
        )

