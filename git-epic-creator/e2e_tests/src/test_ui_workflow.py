"""
End-to-end tests via UI service proxy endpoints.

Flow:
- Create project using UI service `/project/projects`
- Upload dummy.pdf using `/project/projects/{id}/documents/upload`
- Wait for UI pub/sub statuses up to 'rag_ready'
- Trigger AI workflow via `/workflow/requirements`
- Track `ai_workflow_progress` pub/sub messages and assert final response
"""

from typing import Dict, Any

import io
import requests

from config import TestConstants
from services.redis_test_monitor import RedisTestMonitor
from services.workflow_assertions import WorkflowAssertions
from shared_utils import HTTPUtils


class TestUIRequirementsWorkflow:
    def _simulate_sso_login(self, ui_base: str) -> requests.Session:
        """
        Simulate browser SSO login flow to get authenticated session.
        
        Flow:
        1. GET /auth/login - UI service redirects to mock auth /authorize
        2. Mock auth immediately redirects back with code
        3. GET /auth/callback?code=... - UI service exchanges code for tokens, creates session
        4. Session cookie is now set and can be used for API calls
        
        Args:
            ui_base: Base URL of UI service
            
        Returns:
            Authenticated requests.Session with cookies
        """
        session = requests.Session()
        session.verify = False  # For self-signed certs
        
        # Initiate login - follow redirects automatically
        # This will go: /auth/login -> mock_auth /authorize -> /auth/callback
        response = session.get(
            f"{ui_base}/auth/login",
            allow_redirects=True,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        
        # Should end up at home page (/) after successful auth
        assert response.status_code == TestConstants.HTTP_OK, (
            f"SSO login failed: {response.status_code} {response.text}"
        )
        
        # Verify session is authenticated
        me_response = session.get(
            f"{ui_base}/auth/me",
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        assert me_response.status_code == TestConstants.HTTP_OK
        me_data = me_response.json()
        assert me_data.get("authenticated") is True, f"Session not authenticated: {me_data}"
        
        return session
    
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
        
        # 3) Trigger AI workflow via UI proxy and track ai_workflow_progress messages
        # Begin AI workflow monitoring for the same project
        # (requires RedisTestMonitor to support ai_workflow_progress)
        redis_monitor.start_ai_workflow_monitoring(project_id)  # type: ignore[attr-defined]
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
            _ = next(ai_seq)  # analyzing_prompt
            retrieving_msg = next(ai_seq)
            _ = next(ai_seq)  # drafting_requirements

            assert retrieving_msg.get("status") == "retrieving_context"
            details_md = retrieving_msg.get("details_md") or ""
            assert "### Retrieved context" in details_md
            assert "Items:" in details_md
            assert "- Top refs:" in details_md
            # Expect at least one listed ref line
            assert "  - " in details_md

            bundle = wf_resp.json()
            assert bundle.get("project_id") == project_id
            # Minimal sanity checks on response shape
            assert "prompt_id" in bundle
            assert isinstance(bundle.get("business_requirements", []), list)
            assert isinstance(bundle.get("functional_requirements", []), list)
        finally:
            try:
                redis_monitor.stop_ai_workflow_monitoring()  # type: ignore[attr-defined]
            except Exception:
                pass


