"""
End-to-end document processing workflow tests.

This module contains the main integration test for the complete document processing
workflow, from project creation to document upload and processing completion.
"""

from typing import Dict, Any

from config import TestConstants
from shared_utils import ProjectTestUtils
import requests
import io
from conftest import ProjectManager
from services.redis_test_monitor import RedisTestMonitor
from services.workflow_assertions import WorkflowAssertions
from services.workflow_models import WorkflowTestFixtures
from neo4j import GraphDatabase

class TestDocumentWorkflow:
    """Test suite for the complete end-to-end document processing workflow."""

    def test_complete_document_processing_workflow(
        self,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str],
        postgres_connection,
        redis_config: Dict[str, Any],
        test_project_data: Dict[str, Any],
        test_pdf_content: bytes,
        unique_test_filename: str,
        project_manager: ProjectManager,
        redis_monitor: RedisTestMonitor,
        neo4j_driver: GraphDatabase.driver,
        wa: WorkflowAssertions
    ) -> None:
        """
        Test the complete end-to-end document processing workflow.
        """
        fixtures = WorkflowTestFixtures(
            service_urls=service_urls,
            auth_headers=auth_headers,
            postgres_connection=postgres_connection,
            redis_config=redis_config,
            test_project_data=test_project_data,
            test_pdf_content=test_pdf_content,
            unique_test_filename=unique_test_filename,
            project_manager=project_manager,
            redis_monitor=redis_monitor,
            neo4j_driver=neo4j_driver
        )
        
        # Step 1: Create a project
        project_id = fixtures.project_manager.create_project(fixtures.test_project_data)
        
        # Verify project creation response
        assert project_id is not None
        assert isinstance(project_id, str)
        
        # Step 2: Verify project exists in database
        wa.verify_project_in_database(project_id, fixtures)

        # Start UI monitoring for the entire critical section
        with wa.ui_monitoring(project_id, fixtures.redis_monitor):
            # Step 3: Upload PDF file to the project
            upload_result = wa.upload_document(project_id, fixtures, fixtures.redis_monitor)
            wa.verify_upload_response(project_id, upload_result, fixtures)

            # Step 4-7: Iterate UI status sequence with yields between steps
            seq = fixtures.redis_monitor.iter_ui_sequence(
                project_id,
                [
                    TestConstants.PROJECT_STATUS_PROCESSING,
                    TestConstants.PROJECT_STATUS_ACTIVE,
                    (TestConstants.PROJECT_STATUS_RAG_PROCESSING, "pipeline_start"),
                    # assert intermediate messages without relying on exact names
                    TestConstants.PROJECT_STATUS_RAG_PROCESSING,
                    TestConstants.PROJECT_STATUS_RAG_PROCESSING,
                    (TestConstants.PROJECT_STATUS_RAG_PROCESSING, "pipeline_end"),
                    TestConstants.PROJECT_STATUS_RAG_READY,
                ],
                timeout_per_step=300,
            )
            # processing
            _ = next(seq)
            # active -> after this, blobs should be present
            _ = next(seq)
            # rag_processing: pipeline_start
            _ = next(seq)
            # intermediate: expect a workflow_start message
            _msg = next(seq)
            assert isinstance(_msg, dict) and _msg.get("status") == TestConstants.PROJECT_STATUS_RAG_PROCESSING
            _ps = _msg.get("process_step") or ""
            assert isinstance(_ps, str) and _ps.startswith("workflow_start"), f"Unexpected process_step: {_ps}"
            # intermediate: expect a workflow_end message
            _msg2 = next(seq)
            assert isinstance(_msg2, dict) and _msg2.get("status") == TestConstants.PROJECT_STATUS_RAG_PROCESSING
            _ps2 = _msg2.get("process_step") or ""
            assert isinstance(_ps2, str) and _ps2.startswith("workflow_end"), f"Unexpected process_step: {_ps2}"
            # rag_processing: pipeline_end (all workflows done)
            _ = next(seq)
            # rag_ready
            _ = next(seq)
            wa.wait_for_api_status(project_id, fixtures, TestConstants.PROJECT_STATUS_RAG_READY, timeout=300)

        # Step 8: Verify DB status and pct
        wa.verify_db_status(project_id, fixtures, TestConstants.PROJECT_STATUS_RAG_READY, min_pct=100.0)

        # Step 9: Verify Neo4j data (project + document links/records)
        wa.verify_neo4j_project_and_documents(project_id, fixtures, min_docs=1)
        wa.verify_neo4j_constraints_minimal(fixtures)
        wa.verify_neo4j_vector_index(fixtures)

    def test_multiple_document_upload_workflow(
        self,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str],
        postgres_connection,
        redis_config: Dict[str, Any],
        test_project_data: Dict[str, Any],
        test_pdf_content: bytes,
        unique_test_filename: str,
        project_manager: ProjectManager,
        redis_monitor: RedisTestMonitor,
        neo4j_driver: GraphDatabase.driver,
        wa: WorkflowAssertions
    ) -> None:
        """
        Test uploading multiple documents to a single project.
        
        This test verifies that the system can handle multiple document uploads
        and process them correctly.
        """
        # Create a project
        project_id = project_manager.create_project(test_project_data)

        # Build fixtures similar to the primary workflow test
        fixtures = WorkflowTestFixtures(
            service_urls=service_urls,
            auth_headers=auth_headers,
            postgres_connection=postgres_connection,
            redis_config=redis_config,
            test_project_data=test_project_data,
            test_pdf_content=test_pdf_content,
            unique_test_filename=unique_test_filename,
            project_manager=project_manager,
            redis_monitor=redis_monitor,
            neo4j_driver=neo4j_driver
        )

        # Verify project exists in DB
        wa.verify_project_in_database(project_id, fixtures)

        # Start UI monitoring for the critical section
        with wa.ui_monitoring(project_id, redis_monitor):
            # Prepare multiple files for upload
            files, expected_filenames = wa.prepare_multiple_files(project_id, test_pdf_content)

            # Upload multiple documents
            upload_url = ProjectTestUtils.build_upload_url(service_urls['project_management'], project_id)
            upload_response = requests.post(
                upload_url,
                files=files,
                headers=auth_headers,
                timeout=60
            )

            assert upload_response.status_code == TestConstants.HTTP_OK, (
                f"Failed to upload documents: {upload_response.text}"
            )
            upload_result = upload_response.json()

            # Verify multiple upload response
            wa.verify_multiple_upload_response(upload_result, project_id, expected_filenames)

            # Validate UI message sequence
            seq = redis_monitor.iter_ui_sequence(
                project_id,
                [
                    TestConstants.PROJECT_STATUS_PROCESSING,
                    TestConstants.PROJECT_STATUS_ACTIVE,
                    (TestConstants.PROJECT_STATUS_RAG_PROCESSING, "pipeline_start"),
                    # assert intermediate messages without relying on exact names
                    TestConstants.PROJECT_STATUS_RAG_PROCESSING,
                    TestConstants.PROJECT_STATUS_RAG_PROCESSING,
                    (TestConstants.PROJECT_STATUS_RAG_PROCESSING, "pipeline_end"),
                    TestConstants.PROJECT_STATUS_RAG_READY,
                ],
                timeout_per_step=300,
            )
            _ = next(seq)
            _ = next(seq)
            _ = next(seq)
            # intermediate: expect a workflow_start message
            _msg = next(seq)
            assert isinstance(_msg, dict) and _msg.get("status") == TestConstants.PROJECT_STATUS_RAG_PROCESSING
            _ps = _msg.get("process_step") or ""
            assert isinstance(_ps, str) and _ps.startswith("workflow_start"), f"Unexpected process_step: {_ps}"
            # intermediate: expect a workflow_end message
            _msg2 = next(seq)
            assert isinstance(_msg2, dict) and _msg2.get("status") == TestConstants.PROJECT_STATUS_RAG_PROCESSING
            _ps2 = _msg2.get("process_step") or ""
            assert isinstance(_ps2, str) and _ps2.startswith("workflow_end"), f"Unexpected process_step: {_ps2}"
            # pipeline_end
            _ = next(seq)
            # rag_ready
            _ = next(seq)
            wa.wait_for_api_status(project_id, fixtures, TestConstants.PROJECT_STATUS_RAG_READY, timeout=300)

        # Verify DB status
        wa.verify_db_status(project_id, fixtures, TestConstants.PROJECT_STATUS_RAG_READY, min_pct=100.0)

        # Minimal Neo4j check: only assert project node existence
        wa.verify_neo4j_project_and_documents(project_id, fixtures, min_docs=0)

    def test_document_upload_error_handling(
        self,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str],
        test_project_data: Dict[str, Any],
        project_manager: ProjectManager,
        wa: WorkflowAssertions
    ) -> None:
        """
        Test error handling during document upload.
        
        This test verifies proper error responses for invalid uploads.
        """
        project_id = project_manager.create_project(test_project_data)

        # Test 1: Upload without files
        wa.assert_upload_without_files(project_id, service_urls, auth_headers)

        # Test 2: Upload to non-existent project
        wa.assert_upload_to_nonexistent_project(service_urls, auth_headers)

class TestDocumentWorkflowEdgeCases:
    """Test suite for edge cases and error scenarios in document processing."""

    def test_concurrent_document_uploads(
        self,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str],
        test_project_data: Dict[str, Any],
        test_pdf_content: bytes,
        project_manager: ProjectManager
    ) -> None:
        """
        Test handling of concurrent document uploads to the same project.
        
        Note: This is a simplified concurrency test - more sophisticated
        testing would require threading or async frameworks.
        """
        # Create a project
        project_id = project_manager.create_project(test_project_data)

        # Simulate quick successive uploads
        upload_responses = []
        for i in range(2):  # Quick successive uploads
            files = {
                'files': (f"concurrent_test_{i}.pdf", io.BytesIO(test_pdf_content), 'application/pdf')
            }
            
            upload_url = ProjectTestUtils.build_upload_url(service_urls['project_management'], project_id)
            response = requests.post(
                upload_url,
                files=files,
                headers=auth_headers,
                timeout=30
            )
            upload_responses.append(response)

        # Both uploads should succeed
        for i, response in enumerate(upload_responses):
            assert response.status_code == TestConstants.HTTP_OK, (
                f"Concurrent upload {i} failed: {response.text}"
            )
            result = response.json()
            assert result["successful_uploads"] == 1
