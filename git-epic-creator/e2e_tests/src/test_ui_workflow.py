"""
End-to-end tests via UI service proxy endpoints.

Flow:
- Create project using UI service `/project/projects`
- Upload dummy.pdf using `/project/projects/{id}/documents/upload`
- Wait for UI pub/sub statuses up to 'rag_ready'
- Trigger AI workflow via `/workflow/requirements`
- Track `ai_requirements_progress` pub/sub messages and assert final response
"""

from typing import Dict, Any

import io
import requests

from config import TestConstants
from services.redis_test_monitor import RedisTestMonitor
from services.workflow_assertions import WorkflowAssertions
from services.workflow import UIHelpers
from services.validators import ResponseValidators, ContentValidators
from shared_utils import HTTPUtils


def validate_workflow_progress_message(
    msg: Dict[str, Any],
    expected_status: str,
    expected_message_type: str = "ai_requirements_progress",
    expect_score: bool = False
) -> None:
    """Delegate to ResponseValidators for consistency."""
    return ResponseValidators.validate_workflow_progress_message(
        msg, expected_status, expected_message_type, expect_score
    )


def validate_retrieving_context_message(
    msg: Dict[str, Any],
    expected_message_type: str = "ai_requirements_progress"
) -> None:
    """Delegate to ResponseValidators for consistency."""
    return ResponseValidators.validate_retrieving_context_message(msg, expected_message_type)


def validate_backlog_bundle_comprehensive(bundle: Dict[str, Any], project_id: str) -> None:
    """
    Delegate to ResponseValidators for consistency.
    
    Also validates mock content using ContentValidators.
    """
    ResponseValidators.validate_backlog_bundle_comprehensive(bundle, project_id)
    ContentValidators.validate_mock_content_in_backlog(bundle)


class TestUIRequirementsWorkflow:
    def _simulate_gitlab_oauth_connection(self, session: requests.Session, ui_base: str) -> None:
        """Delegate to UIHelpers for GitLab OAuth connection."""
        return UIHelpers.simulate_gitlab_oauth_connection(session, ui_base)
    
    def _simulate_sso_login(self, ui_base: str) -> requests.Session:
        """Delegate to UIHelpers for SSO login."""
        return UIHelpers.simulate_sso_login(ui_base)
    
    def test_ui_end_to_end_requirements_workflow(
        self,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str],
        test_project_data: Dict[str, Any],
        test_pdf_content: bytes,
        unique_test_filename: str,
        redis_monitor: RedisTestMonitor,
        wa: WorkflowAssertions,
        neo4j_driver,
        target_db_name,
        cyphers_path,
    ) -> None:
        """
        Full user flow driven through UI service proxies.
        """
        ui_base = service_urls["ui_service"]  # requires config addition

        # Seed Neo4j graph for deterministic retrieval behavior (same as retrieval test)
        wa.load_cypher_script(neo4j_driver, target_db_name, cyphers_path)

        # Ensure retrieval service is healthy
        assert HTTPUtils.wait_for_service_health(service_urls['neo4j_retrieval']), (
            "neo4j_retrieval service is not healthy"
        )

        # Simulate browser SSO login to get authenticated session
        authenticated_session = self._simulate_sso_login(ui_base)

        # 1) Create project via UI proxy using authenticated session
        create_resp = authenticated_session.post(
            f"{ui_base}/project/projects",
            json=test_project_data,
            timeout=TestConstants.DEFAULT_TIMEOUT,
        )
        assert create_resp.status_code == TestConstants.HTTP_CREATED, (
            f"Failed to create project via UI: {create_resp.status_code} {create_resp.text}"
        )
        project = create_resp.json()
        project_id = project["id"]

        # 2) Start UI monitoring and upload PDF
        with wa.ui_monitoring(project_id, redis_monitor):
            files = {
                "files": (
                    unique_test_filename,
                    io.BytesIO(test_pdf_content),
                    "application/pdf",
                )
            }
            upload_resp = authenticated_session.post(
                f"{ui_base}/project/projects/{project_id}/documents/upload",
                files=files,
                timeout=60,
            )
            assert upload_resp.status_code == TestConstants.HTTP_OK, (
                f"Upload failed via UI: {upload_resp.status_code} {upload_resp.text}"
            )

            # Wait for UI status sequence to RAG_READY
            seq = redis_monitor.iter_ui_sequence(
                project_id,
                [
                    TestConstants.PROJECT_STATUS_PROCESSING,
                    TestConstants.PROJECT_STATUS_ACTIVE,
                    TestConstants.PROJECT_STATUS_RAG_PROCESSING,
                    TestConstants.PROJECT_STATUS_RAG_READY,
                ],
                timeout_per_step=TestConstants.DOCUMENT_PROCESSING_TIMEOUT,
            )
            _ = next(seq)
            _ = next(seq)
            _ = next(seq)
            _ = next(seq)

        # Pre-fetch retrieval result for the target question to compare later
        question = "what are the main components of the bridge?"
        
        # 3) Trigger AI requirements via UI proxy and track ai_requirements_progress messages
        # Begin AI requirements monitoring for the same project
        # (requires RedisTestMonitor to support ai_requirements_progress)
        redis_monitor.start_ai_requirements_monitoring(project_id)  # type: ignore[attr-defined]
        try:
            req_payload = {
                "project_id": project_id,
                "prompt": question,
            }
            wf_resp = authenticated_session.post(
                f"{ui_base}/workflow/requirements",
                json=req_payload,
                timeout=120,
            )
            assert wf_resp.status_code == TestConstants.HTTP_OK, (
                f"Workflow request failed via UI: {wf_resp.status_code} {wf_resp.text}"
            )

            ai_seq = redis_monitor.iter_ai_sequence(
                project_id,
                [
                    "analyzing_prompt",
                    "retrieving_context",
                    "drafting_requirements",
                ],
                timeout_per_step=TestConstants.DOCUMENT_PROCESSING_TIMEOUT,
            )  # type: ignore[attr-defined]
            analyzing_msg = next(ai_seq)  # analyzing_prompt
            validate_workflow_progress_message(analyzing_msg, "analyzing_prompt")
            
            retrieving_msg = next(ai_seq)  # retrieving_context
            validate_retrieving_context_message(retrieving_msg)
            
            drafting_msg = next(ai_seq)  # drafting_requirements
            validate_workflow_progress_message(drafting_msg, "drafting_requirements")

            # Validate final requirements bundle response
            bundle = wf_resp.json()
            assert bundle.get("project_id") == project_id, \
                f"project_id mismatch: expected {project_id}, got {bundle.get('project_id')}"
            
            # Required fields
            assert "prompt_id" in bundle, "Missing prompt_id in response"
            assert "business_requirements" in bundle, "Missing business_requirements"
            assert "functional_requirements" in bundle, "Missing functional_requirements"
            
            # Validate structure and content quality
            assert isinstance(bundle.get("business_requirements", []), list), \
                "business_requirements should be a list"
            assert isinstance(bundle.get("functional_requirements", []), list), \
                "functional_requirements should be a list"
            
            # Validate at least some requirements were generated
            total_reqs = (len(bundle.get("business_requirements", [])) + 
                         len(bundle.get("functional_requirements", [])))
            assert total_reqs > 0, \
                "Should generate at least one requirement (business or functional)"
            
            # Validate mock response content
            all_reqs = bundle.get("business_requirements", []) + bundle.get("functional_requirements", [])
            ContentValidators.validate_mock_content_in_requirements(all_reqs)
        finally:
            try:
                redis_monitor.stop_ai_requirements_monitoring()  # type: ignore[attr-defined]
            except Exception:
                pass
    
    def test_ui_tasks_generation_workflow(
        self,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str],
        test_project_data: Dict[str, Any],
        test_pdf_content: bytes,
        unique_test_filename: str,
        redis_monitor: RedisTestMonitor,
        wa: WorkflowAssertions,
        neo4j_driver,
        target_db_name,
        cyphers_path,
    ) -> None:
        """
        Full AI tasks/backlog generation flow via UI service.
        
        Flow:
        1. SSO login to get authenticated session
        2. GitLab OAuth connection to enable GitLab backlog fetching
        3. Create project and upload document
        4. Wait for RAG_READY status
        5. Trigger tasks generation via /tasks/generate
        6. Monitor ai_tasks_progress pub/sub messages
        7. Assert final backlog bundle response
        """
        ui_base = service_urls["ui_service"]

        # Seed Neo4j graph for retrieval context
        wa.load_cypher_script(neo4j_driver, target_db_name, cyphers_path)

        # Ensure required services are healthy
        assert HTTPUtils.wait_for_service_health(service_urls['neo4j_retrieval']), (
            "neo4j_retrieval service is not healthy"
        )
        assert HTTPUtils.wait_for_service_health(service_urls['ai_tasks']), (
            "ai_tasks service is not healthy"
        )
        assert HTTPUtils.wait_for_service_health(service_urls['gitlab_mock']), (
            "gitlab_mock service is not healthy"
        )

        # 1) Simulate browser SSO login
        authenticated_session = self._simulate_sso_login(ui_base)

        # 2) Simulate GitLab OAuth connection
        self._simulate_gitlab_oauth_connection(authenticated_session, ui_base)

        # 3) Create project via UI proxy
        create_resp = authenticated_session.post(
            f"{ui_base}/project/projects",
            json=test_project_data,
            timeout=TestConstants.DEFAULT_TIMEOUT,
        )
        assert create_resp.status_code == TestConstants.HTTP_CREATED, (
            f"Failed to create project via UI: {create_resp.status_code} {create_resp.text}"
        )
        project = create_resp.json()
        project_id = project["id"]

        # 4) Upload document and wait for RAG_READY
        with wa.ui_monitoring(project_id, redis_monitor):
            files = {
                "files": (
                    unique_test_filename,
                    io.BytesIO(test_pdf_content),
                    "application/pdf",
                )
            }
            upload_resp = authenticated_session.post(
                f"{ui_base}/project/projects/{project_id}/documents/upload",
                files=files,
                timeout=60,
            )
            assert upload_resp.status_code == TestConstants.HTTP_OK, (
                f"Upload failed via UI: {upload_resp.status_code} {upload_resp.text}"
            )

            # Wait for UI status sequence to RAG_READY
            seq = redis_monitor.iter_ui_sequence(
                project_id,
                [
                    TestConstants.PROJECT_STATUS_PROCESSING,
                    TestConstants.PROJECT_STATUS_ACTIVE,
                    TestConstants.PROJECT_STATUS_RAG_PROCESSING,
                    TestConstants.PROJECT_STATUS_RAG_READY,
                ],
                timeout_per_step=TestConstants.DOCUMENT_PROCESSING_TIMEOUT,
            )
            _ = next(seq)
            _ = next(seq)
            _ = next(seq)
            _ = next(seq)

        # 5) Start AI tasks progress monitoring
        redis_monitor.start_ai_tasks_monitoring(project_id)
        try:
            # 6) Trigger tasks generation via UI proxy
            requirements_text = """
            # COBOL Migration Requirements
            
            We need to migrate our legacy COBOL risk analytics system to modern Python microservices.
            
            ## Key Components
            - Risk calculation engine
            - Portfolio analysis module
            - Market data integration
            - Reporting system
            
            ## Goals
            - Maintain calculation accuracy
            - Improve performance
            - Add real-time capabilities
            """
            
            tasks_payload = {
                "project_id": project_id,
                "message": requirements_text,
            }
            tasks_resp = authenticated_session.post(
                f"{ui_base}/tasks/generate",
                json=tasks_payload,
                timeout=120,
            )
            assert tasks_resp.status_code == TestConstants.HTTP_OK, (
                f"Tasks generation failed via UI: {tasks_resp.status_code} {tasks_resp.text}"
            )

            # 7) Monitor AI tasks progress messages
            # Expected sequence: analyzing -> retrieving -> fetching -> drafting -> mapping -> evaluating -> completed
            ai_seq = redis_monitor.iter_ai_tasks_sequence(
                project_id,
                [
                    "analyzing_requirements",
                    "retrieving_context",
                    "fetching_backlog",
                    "drafting_backlog",
                    "mapping_duplicates",
                    "evaluating",
                ],
                timeout_per_step=TestConstants.DOCUMENT_PROCESSING_TIMEOUT,
            )
            
            # Validate each AI tasks progress message with comprehensive checks
            analyzing_msg = next(ai_seq)
            validate_workflow_progress_message(analyzing_msg, "analyzing_requirements", "ai_tasks_progress")
            
            retrieving_msg = next(ai_seq)
            validate_retrieving_context_message(retrieving_msg, "ai_tasks_progress")
            
            fetching_msg = next(ai_seq)
            validate_workflow_progress_message(fetching_msg, "fetching_backlog", "ai_tasks_progress")
            
            drafting_msg = next(ai_seq)
            validate_workflow_progress_message(drafting_msg, "drafting_backlog", "ai_tasks_progress")
            # Additional validation for drafting message
            drafting_details = drafting_msg.get("details_md", "")
            assert drafting_details, "drafting_backlog should have details_md"
            assert "draft" in drafting_details.lower() or "epic" in drafting_details.lower(), \
                f"drafting_backlog details should mention draft or epics: {drafting_details[:200]}"
            
            mapping_msg = next(ai_seq)
            validate_workflow_progress_message(mapping_msg, "mapping_duplicates", "ai_tasks_progress")
            
            evaluating_msg = next(ai_seq)
            validate_workflow_progress_message(evaluating_msg, "evaluating", "ai_tasks_progress", expect_score=True)
            # Evaluating should also have iteration
            assert "iteration" in evaluating_msg, "evaluating message should include iteration"
            eval_details = evaluating_msg.get("details_md", "")
            assert eval_details, "evaluating should have details_md"
            assert "evaluation" in eval_details.lower() or "score" in eval_details.lower(), \
                f"evaluating details should mention evaluation or score: {eval_details[:200]}"

            # 8) Comprehensively validate final backlog bundle response
            bundle = tasks_resp.json()
            validate_backlog_bundle_comprehensive(bundle, project_id)
            
        finally:
            try:
                redis_monitor.stop_ai_tasks_monitoring()
            except Exception:
                pass


