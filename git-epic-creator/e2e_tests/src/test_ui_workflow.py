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


class TestUIRequirementsWorkflow:
    def test_ui_end_to_end_requirements_workflow(
        self,
        service_urls: Dict[str, str],
        auth_headers: Dict[str, str],
        test_project_data: Dict[str, Any],
        test_pdf_content: bytes,
        unique_test_filename: str,
        redis_monitor: RedisTestMonitor,
        wa: WorkflowAssertions,
    ) -> None:
        """
        Full user flow driven through UI service proxies.
        """
        ui_base = service_urls["ui_service"]  # requires config addition

        # 1) Create project via UI proxy
        create_resp = requests.post(
            f"{ui_base}/project/projects",
            json=test_project_data,
            headers=auth_headers,
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
            upload_resp = requests.post(
                f"{ui_base}/project/projects/{project_id}/documents/upload",
                files=files,
                headers=auth_headers,
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

        # 3) Trigger AI workflow via UI proxy and track ai_workflow_progress messages
        # Begin AI workflow monitoring for the same project
        # (requires RedisTestMonitor to support ai_workflow_progress)
        redis_monitor.start_ai_workflow_monitoring(project_id)  # type: ignore[attr-defined]
        try:
            req_payload = {
                "project_id": project_id,
                "prompt": "Generate requirements for document processing and retrieval",
            }
            wf_resp = requests.post(
                f"{ui_base}/workflow/requirements",
                json=req_payload,
                headers=auth_headers,
                timeout=120,
            )
            assert wf_resp.status_code == TestConstants.HTTP_OK, (
                f"Workflow request failed via UI: {wf_resp.status_code} {wf_resp.text}"
            )

            # Expect a reasonable AI progress sequence
            ai_seq = redis_monitor.iter_ai_sequence(
                project_id,
                [
                    "analyzing_prompt",
                    "retrieving_context",
                    "drafting_requirements",
                ],
                timeout_per_step=TestConstants.DOCUMENT_PROCESSING_TIMEOUT,
            )  # type: ignore[attr-defined]
            _ = next(ai_seq)
            _ = next(ai_seq)
            _ = next(ai_seq)

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


