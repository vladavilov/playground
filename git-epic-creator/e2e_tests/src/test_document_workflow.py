"""
End-to-end document processing workflow tests.

This module contains the main integration test for the complete document processing
workflow, from project creation to document upload and processing completion.
"""

import io
import time
import redis
from typing import Dict, Any
from dataclasses import dataclass

import requests

from config import TestConstants
from conftest import ProjectManager
from services.redis_test_monitor import RedisTestMonitor
from shared_utils import ProjectTestUtils
from neo4j import GraphDatabase


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
    project_manager: ProjectManager
    redis_monitor: RedisTestMonitor
    neo4j_driver: Any = None


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
        neo4j_driver: GraphDatabase.driver
    ) -> None:
        """
        Test the complete end-to-end document processing workflow.
        
        This test verifies:
        1. Project creation via project_management_service
        2. Document upload to the project
        3. Document processing initiation
        4. Processing completion verification
        5. Project status updates during processing
        6. Neo4j data ingestion
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
        
        # Execute the workflow
        project_id = self._execute_main_workflow(fixtures)
        
        # Verify completion
        self._verify_processing_completion(
            project_id=project_id,
            fixtures=fixtures
        )

    def _execute_main_workflow(self, fixtures: WorkflowTestFixtures) -> str:
        """Execute the main document processing workflow."""
        # Step 1: Create a project
        project_id = fixtures.project_manager.create_project(fixtures.test_project_data)
        
        # Verify project creation response
        assert project_id is not None
        assert isinstance(project_id, str)
        
        # Step 2: Verify project exists in database
        self._verify_project_in_database(project_id, fixtures)

        # Step 3: Start UI progress monitoring BEFORE document processing
        fixtures.redis_monitor.start_ui_progress_monitoring(project_id)
        
        try:
            # Step 4: Upload PDF file to the project
            upload_result = self._upload_document(project_id, fixtures, fixtures.redis_monitor)
            
            # Verify upload response structure and content
            self._verify_upload_response(project_id, upload_result, fixtures)

            # Step 5: Wait for document processing while monitoring UI messages
            self._wait_for_processing_with_ui_monitoring(
                project_id=project_id,
                fixtures=fixtures
            )
            
            # Step 6: Verify that Redis messages were properly consumed
            self._verify_redis_message_consumed(fixtures)

            # Step 7: Verify project progress messages were captured during processing
            self._verify_ui_progress_messages_received(project_id, fixtures.redis_monitor)
            
        finally:
            fixtures.redis_monitor.stop_ui_progress_monitoring()

        # Step 8: Verify project status was updated during processing
        self._verify_project_processing_status(
            project_id=project_id,
            fixtures=fixtures
        )
        
        return project_id

    def _wait_for_processing_with_ui_monitoring(
        self,
        project_id: str,
        fixtures: WorkflowTestFixtures
    ) -> None:
        """
        Wait for document processing while continuously monitoring UI progress messages.
        
        This method uses real-time pub/sub monitoring combined with processing status
        checks to ensure comprehensive validation of the workflow.
        """
        
        def check_processing_complete() -> bool:
            """Check if document processing is complete."""
            try:
                project_url = ProjectTestUtils.build_project_url(
                    fixtures.service_urls['project_management'], project_id
                )
                response = requests.get(
                    project_url,
                    headers=fixtures.auth_headers,
                    timeout=TestConstants.DEFAULT_TIMEOUT
                )
                
                if response.status_code == TestConstants.HTTP_OK:
                    project = response.json()
                    return project["status"] == TestConstants.PROJECT_STATUS_ACTIVE
                    
            except requests.RequestException as e:
                # Ignore individual status check errors during monitoring
                pass
                
            return False
        
        success = fixtures.redis_monitor.monitor_ui_during_processing(
            processing_check_func=check_processing_complete,
            timeout=TestConstants.DOCUMENT_PROCESSING_TIMEOUT,
            check_interval=1.0
        )
        
        assert success, "Processing monitoring failed"

    def _verify_ui_progress_messages_received(
        self,
        project_id: str,
        redis_monitor: RedisTestMonitor
    ) -> None:
        """
        Verify that UI progress messages were actually received during processing.
        
        This validation confirms that the pub/sub mechanism worked correctly
        and that messages were published and captured at the right time.
        """
        messages_count = redis_monitor.get_ui_messages_count()
        messages = redis_monitor.get_ui_messages()
        
        assert messages_count > 0, (
            f"No UI progress messages received for project {project_id}. "
            f"This indicates the pub/sub mechanism may not be working. "
            f"Expected at least 1 progress message during document processing. "
            f"Monitored channel: {redis_monitor.ui_progress_channel}"
        )
        
        # Verify message structure and content
        valid_messages = 0
        for message in messages:
            # Validate message structure
            if (message.get('project_id') == project_id and
                message.get('message_type') == 'project_progress' and
                'status' in message):
                valid_messages += 1
        
        assert valid_messages > 0, f"No valid UI progress messages found for project {project_id}"
        assert valid_messages == messages_count, (
            f"Some UI progress messages had invalid structure. "
            f"Valid: {valid_messages}/{messages_count}"
        )

    def _verify_project_in_database(self, project_id: str, fixtures: WorkflowTestFixtures) -> None:
        """Verify project exists in database with correct data."""
        cursor = fixtures.postgres_connection.cursor()
        cursor.execute("SELECT id, name, status FROM projects WHERE id = %s", (project_id,))
        db_project = cursor.fetchone()
        cursor.close()

        assert db_project is not None, "Project not found in database"
        assert str(db_project[0]) == project_id
        assert db_project[1] == fixtures.test_project_data["name"]
        assert db_project[2] == fixtures.test_project_data["status"]

    def _upload_document(
        self, 
        project_id: str, 
        fixtures: WorkflowTestFixtures,
        task_monitor: RedisTestMonitor
    ) -> Dict[str, Any]:
        """Upload document to project and return response with comprehensive task verification."""
        # Get initial state for multiple monitoring strategies
        initial_task_count = task_monitor.get_current_task_count()
        initial_stream_count = task_monitor.get_stream_message_count()
        
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
            timeout=30  # Longer timeout for file upload
        )

        assert upload_response.status_code == TestConstants.HTTP_OK, (
            f"Failed to upload document: {upload_response.text}"
        )
        
        # Wait a moment for async task queuing and Redis message publishing
        time.sleep(2)
        
        # Use comprehensive verification strategy
        verification_results = task_monitor.verify_task_published_comprehensive(
            project_id=project_id,
            initial_stream_count=initial_stream_count,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        
        # Accept verification if any strong indicator is present
        verification_passed = (
            verification_results['verification_successful'] or
            verification_results['stream_message_found'] or 
            verification_results['stream_count_increased'] or
            task_monitor.get_current_task_count() > initial_task_count
        )
        
        assert verification_passed, (
            f"Expected task publishing verification to pass after document upload. "
            f"Verification results: {verification_results}. "
            f"Traditional queue: {initial_task_count} -> {task_monitor.get_current_task_count()}. "
            f"Stream messages: {initial_stream_count} -> {task_monitor.get_stream_message_count()}"
        )
        
        return upload_response.json()

    def _verify_redis_message_published(self, project_id: str, fixtures: WorkflowTestFixtures) -> None:
        """Verify that a task request message was published to Redis streams."""
        try:
            redis_client = redis.Redis(
                host=fixtures.redis_config.get('host', 'localhost'),
                port=fixtures.redis_config.get('port', 6379),
                db=fixtures.redis_config.get('db', 0),
                decode_responses=True
            )
            
            stream_name = "task_streams:document_processing"
            
            try:
                stream_info = redis_client.xinfo_stream(stream_name)
                stream_length = stream_info.get('length', 0)
                
                assert stream_length > 0, (
                    f"Expected messages in Redis stream '{stream_name}', but found {stream_length} messages"
                )
                
                # Get the latest message to verify it's for our project
                latest_messages = redis_client.xrevrange(stream_name, count=1)
                if latest_messages:
                    message_id, message_data = latest_messages[0]
                    message_project_id = message_data.get('project_id')
                    
                    assert message_project_id == project_id, (
                        f"Expected message for project {project_id}, but found message for project {message_project_id}"
                    )
                    
                    message_type = message_data.get('message_type')
                    assert message_type == 'task_request', (
                        f"Expected message_type 'task_request', but found '{message_type}'"
                    )
                    
                    # Verify task type
                    task_type = message_data.get('task_type')
                    assert task_type == 'process_project_documents', (
                        f"Expected task_type 'process_project_documents', but found '{task_type}'"
                    )
                else:
                    assert False, f"No messages found in Redis stream '{stream_name}'"
                    
            except redis.ResponseError as e:
                if "no such key" in str(e).lower():
                    assert False, f"Redis stream '{stream_name}' does not exist - message was not published"
                else:
                    raise
                    
        except Exception as e:
            raise Exception(f"Redis message verification failed: {e}")

    def _verify_redis_message_consumed(self, fixtures: WorkflowTestFixtures) -> None:
        """Verify that Redis messages were properly consumed and acknowledged."""
        
        redis_client = redis.Redis(
            host=fixtures.redis_config.get('host', 'localhost'),
            port=fixtures.redis_config.get('port', 6379),
            db=fixtures.redis_config.get('db', 0),
            decode_responses=True
        )
        
        stream_name = "task_streams:document_processing"
        consumer_group = "document_processors"
        
        try:
            # Check pending messages in consumer group
            pending_info = redis_client.xpending(stream_name, consumer_group)
            
            # Extract pending count from the response
            if isinstance(pending_info, dict):
                pending_count = pending_info.get('pending', 0)
            elif isinstance(pending_info, list) and len(pending_info) > 0:
                pending_count = pending_info[0]  # First element is usually the count
            else:
                pending_count = 0
            
            if pending_count > 0:
                raise Exception(f"Found {pending_count} pending messages - may indicate incomplete processing")
                
            # Check consumer group info
            groups_info = redis_client.xinfo_groups(stream_name)
            for group_info in groups_info:
                if group_info.get('name') == consumer_group:
                    consumers_count = group_info.get('consumers', 0)
                    pending_group = group_info.get('pending', 0)
                    
                    if (consumers_count < 1):
                        raise Exception(f"Found {consumers_count} consumers in consumer group '{consumer_group}' - may indicate incomplete processing")
                    
                    if (pending_group > 0):
                        raise Exception(f"Found {pending_group} pending messages in consumer group '{consumer_group}' - may indicate incomplete processing")

                    break
                    
        except redis.ResponseError as e:
            if "nogroup" in str(e).lower():
                raise Exception(f"Consumer group '{consumer_group}' does not exist")
            else:
                raise Exception(f"Redis error checking message consumption: {e}")
        except Exception as e:
            raise Exception(f"Error verifying message consumption: {e}")

    def _verify_ui_project_progress_published(self, project_id: str, fixtures: WorkflowTestFixtures) -> None:
        """
        Verify that project progress messages were published to the UI channel.
        
        This validation ensures the pub/sub mechanism is working correctly
        for real-time UI updates during document processing workflow.
        """
        progress_found = fixtures.redis_monitor.wait_for_ui_project_progress_message(
            project_id=project_id,
            expected_status=None,  # Accept any status update
            timeout=15 
        )
        
        assert progress_found, (
            f"Expected UI project progress message for project {project_id}, but no message found"
        )

    def _verify_upload_response(
        self, 
        project_id: str, 
        upload_result: Dict[str, Any], 
        fixtures: WorkflowTestFixtures
    ) -> None:
        """Verify upload response structure and content."""
        assert upload_result["project_id"] == project_id
        assert upload_result["total_files"] == 1
        assert upload_result["successful_uploads"] == 1
        assert upload_result["failed_uploads"] == 0
        assert upload_result["processing_initiated"] is True
        assert fixtures.unique_test_filename in upload_result["uploaded_files"]

    def test_multiple_document_upload_workflow(
        self,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str],
        test_project_data: Dict[str, Any],
        test_pdf_content: bytes,
        project_manager: ProjectManager
    ) -> None:
        """
        Test uploading multiple documents to a single project.
        
        This test verifies that the system can handle multiple document uploads
        and process them correctly.
        """
        # Create a project
        project_id = project_manager.create_project(test_project_data)

        # Prepare multiple files for upload
        files, expected_filenames = self._prepare_multiple_files(project_id, test_pdf_content)

        # Upload multiple documents
        upload_url = ProjectTestUtils.build_upload_url(service_urls['project_management'], project_id)
        upload_response = requests.post(
            upload_url,
            files=files,
            headers=auth_headers,
            timeout=60  # Longer timeout for multiple files
        )

        assert upload_response.status_code == TestConstants.HTTP_OK, (
            f"Failed to upload documents: {upload_response.text}"
        )
        upload_result = upload_response.json()

        # Verify multiple upload response
        self._verify_multiple_upload_response(upload_result, project_id, expected_filenames)

    def _prepare_multiple_files(self, project_id: str, test_pdf_content: bytes):
        """Prepare multiple files for upload testing."""
        files = []
        expected_filenames = []
        for i in range(3):  # Upload 3 test documents
            filename = f"test_document_{i}_{project_id[:8]}.pdf"
            expected_filenames.append(filename)
            files.append(('files', (filename, io.BytesIO(test_pdf_content), 'application/pdf')))
        
        return files, expected_filenames

    def _verify_multiple_upload_response(
        self, 
        upload_result: Dict[str, Any], 
        project_id: str, 
        expected_filenames: list
    ) -> None:
        """Verify the response from multiple document upload."""
        assert upload_result["project_id"] == project_id
        assert upload_result["total_files"] == 3
        assert upload_result["successful_uploads"] == 3
        assert upload_result["failed_uploads"] == 0
        assert upload_result["processing_initiated"] is True
        
        for filename in expected_filenames:
            assert filename in upload_result["uploaded_files"]

    def test_document_upload_error_handling(
        self,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str],
        test_project_data: Dict[str, Any],
        project_manager: ProjectManager
    ) -> None:
        """
        Test error handling during document upload.
        
        This test verifies proper error responses for invalid uploads.
        """
        project_id = project_manager.create_project(test_project_data)

        # Test 1: Upload without files
        self._test_upload_without_files(project_id, service_urls, auth_headers)

        # Test 2: Upload to non-existent project
        self._test_upload_to_nonexistent_project(service_urls, auth_headers)

    def _test_upload_without_files(
        self, 
        project_id: str, 
        service_urls: Dict[str, str], 
        auth_headers: Dict[str, str]
    ) -> None:
        """Test uploading without any files."""
        upload_url = ProjectTestUtils.build_upload_url(service_urls['project_management'], project_id)
        upload_response = requests.post(
            upload_url,
            headers=auth_headers,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        
        # Should handle missing files gracefully
        assert upload_response.status_code in [400, 422], (
            "Should return error for upload without files"
        )

    def _test_upload_to_nonexistent_project(
        self, 
        service_urls: Dict[str, str], 
        auth_headers: Dict[str, str]
    ) -> None:
        """Test uploading to a non-existent project."""
        # Use valid UUID format but definitely non-existent project
        fake_project_id = "ffffffff-ffff-ffff-ffff-ffffffffffff"
        files = {
            'files': ("test.pdf", io.BytesIO(b"fake pdf content"), 'application/pdf')
        }
        
        upload_url = ProjectTestUtils.build_upload_url(service_urls['project_management'], fake_project_id)
        upload_response = requests.post(
            upload_url,
            files=files,
            headers=auth_headers,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        
        # Should return not found for non-existent project
        assert upload_response.status_code == TestConstants.HTTP_NOT_FOUND, (
            f"Should return 404 for non-existent project, got {upload_response.status_code}. "
            f"Response: {upload_response.text}"
        )

    def test_project_status_transitions(
        self,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str],
        postgres_connection,
        test_project_data: Dict[str, Any],
        test_pdf_content: bytes,
        unique_test_filename: str,
        project_manager: ProjectManager,
        redis_monitor: RedisTestMonitor
    ) -> None:
        """
        Test that project status transitions correctly during processing.
        
        This test monitors project status changes throughout the workflow.
        """
        # Create a project
        project_id = project_manager.create_project(test_project_data)

        # Verify initial project status
        self._verify_initial_project_status(project_id, service_urls, auth_headers)

        # Upload document and verify status transitions
        self._test_status_transition_after_upload(
            project_id, service_urls, auth_headers, test_pdf_content, 
            unique_test_filename, postgres_connection, redis_monitor
        )

    def _verify_initial_project_status(
        self, 
        project_id: str, 
        service_urls: Dict[str, str], 
        auth_headers: Dict[str, str]
    ) -> None:
        """Verify the initial project status."""
        project_url = ProjectTestUtils.build_project_url(service_urls['project_management'], project_id)
        response = requests.get(
            project_url,
            headers=auth_headers,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        assert response.status_code == TestConstants.HTTP_OK
        project = response.json()
        assert project["status"] == TestConstants.PROJECT_STATUS_ACTIVE

    def _test_status_transition_after_upload(
        self,
        project_id: str,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str],
        test_pdf_content: bytes,
        unique_test_filename: str,
        postgres_connection,
        redis_monitor: RedisTestMonitor
    ) -> None:
        """Test status transitions after document upload."""
        files = {
            'files': (unique_test_filename, io.BytesIO(test_pdf_content), 'application/pdf')
        }

        upload_url = ProjectTestUtils.build_upload_url(service_urls['project_management'], project_id)
        upload_response = requests.post(
            upload_url,
            files=files,
            headers=auth_headers,
            timeout=30
        )
        
        assert upload_response.status_code == TestConstants.HTTP_OK

        # Check if project status changed to processing (may be immediate or after delay)
        # Note: Status might transition quickly, so we check both database and API
        cursor = postgres_connection.cursor()
        cursor.execute("SELECT status FROM projects WHERE id = %s", (project_id,))
        db_status = cursor.fetchone()[0]
        cursor.close()

        # Status should be either 'processing' or back to 'active' (if processing was very fast)
        expected_statuses = [TestConstants.PROJECT_STATUS_PROCESSING, TestConstants.PROJECT_STATUS_ACTIVE]
        assert db_status in expected_statuses

        # Verify that project status changes triggered pub/sub messages
        self._verify_status_change_pubsub_messages(project_id, redis_monitor)

    def _verify_status_change_pubsub_messages(
        self, 
        project_id: str, 
        redis_monitor: RedisTestMonitor
    ) -> None:
        """
        Verify that project status changes triggered pub/sub messages.
        
        This validation ensures that status transitions publish appropriate
        messages to the UI progress channel for real-time updates.
        """
        
        # Check for any project progress messages published during status transitions
        progress_found = redis_monitor.wait_for_ui_project_progress_message(
            project_id=project_id,
            expected_status=None,  # Accept any status change
            timeout=10  # Short timeout since status changes should be immediate
        )
        
        assert progress_found, f"No status change pub/sub messages found for project {project_id}"

    def _verify_project_processing_status(
        self,
        project_id: str,
        fixtures: WorkflowTestFixtures
    ) -> None:
        """Verify that project status reflects processing state correctly."""
        project_url = ProjectTestUtils.build_project_url(
            fixtures.service_urls['project_management'], project_id
        )
        response = requests.get(
            project_url,
            headers=fixtures.auth_headers,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        assert response.status_code == TestConstants.HTTP_OK

        project = response.json()
        # Project should either be back to 'active' or still 'processing'
        expected_statuses = [
            TestConstants.PROJECT_STATUS_ACTIVE, 
            TestConstants.PROJECT_STATUS_PROCESSING
        ]
        assert project["status"] in expected_statuses

    def _verify_processing_completion(
        self,
        project_id: str,
        fixtures: WorkflowTestFixtures
    ) -> None:
        """Verify that document processing completed successfully."""
        # Check final project status via API
        project_url = ProjectTestUtils.build_project_url(
            fixtures.service_urls['project_management'], project_id
        )
        response = requests.get(
            project_url,
            headers=fixtures.auth_headers,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        assert response.status_code == TestConstants.HTTP_OK

        project = response.json()
        # After processing, project should be back to active status
        assert project["status"] == TestConstants.PROJECT_STATUS_ACTIVE

        # Verify in database that processing completed
        cursor = fixtures.postgres_connection.cursor()
        cursor.execute(
            "SELECT status, processed_pct FROM projects WHERE id = %s",
            (project_id,)
        )
        db_project = cursor.fetchone()
        cursor.close()

        assert db_project is not None
        assert db_project[0] == TestConstants.PROJECT_STATUS_ACTIVE

        time.sleep(10)  # Give time for async Neo4j processing
        
        with fixtures.neo4j_driver.session() as session:
            # Look for project node
            project_query = (
                "MATCH (p:Project) WHERE p.project_id = $project_id OR p.id = $project_id "
                "RETURN p"
            )
            result = session.run(project_query, project_id=project_id)
            project_nodes = list(result)
            
            assert project_nodes, f"No project nodes found in Neo4j for project {project_id}"
            
            # Verify Neo4j project and document structure
            document_query = """
                MATCH (p:Project) 
                WHERE p.project_id = $project_id OR p.id = $project_id
                OPTIONAL MATCH (p)-[:HAS_DOCUMENT]->(d:Document)
                RETURN p, collect(d) as documents
            """
            result = session.run(document_query, project_id=project_id)
            
            record = result.single()
            assert record, f"No record found in Neo4j for project {project_id}"
            
            project_node = record["p"]
            documents = record["documents"]
            
            assert project_node is not None
            
            assert documents and documents != [None]
            valid_docs = [d for d in documents if d is not None]
            assert len(valid_docs) >= 1
            
            for doc in valid_docs:
                assert 'filename' in doc or 'content' in doc


class TestDocumentWorkflowEdgeCases:
    """Test suite for edge cases and error scenarios in document processing."""

    def test_large_document_processing(
        self,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str],
        test_project_data: Dict[str, Any],
        project_manager: ProjectManager,
        services_ready: None  # pylint: disable=unused-argument
    ) -> None:
        """Test processing of larger documents (within reasonable limits)."""
        # Create a project
        project_id = project_manager.create_project(test_project_data)

        # Create a larger dummy PDF content (simulate a bigger document)
        # Note: This is still a mock - in real tests you might use actual large PDFs
        large_pdf_content = b"Large PDF content placeholder " * 1000  # Simulate larger content
        
        files = {
            'files': ("large_test.pdf", io.BytesIO(large_pdf_content), 'application/pdf')
        }

        upload_url = ProjectTestUtils.build_upload_url(service_urls['project_management'], project_id)
        upload_response = requests.post(
            upload_url,
            files=files,
            headers=auth_headers,
            timeout=60  # Longer timeout for larger file
        )

        # Should still process successfully
        assert upload_response.status_code == TestConstants.HTTP_OK
        upload_result = upload_response.json()
        assert upload_result["successful_uploads"] == 1

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
