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


def validate_workflow_progress_message(
    msg: Dict[str, Any], 
    expected_status: str, 
    expected_message_type: str = "ai_workflow_progress",
    expect_score: bool = False
) -> None:
    """
    Comprehensive validation of AI workflow/tasks progress message structure.
    
    Validates message conforms to WorkflowProgressMessage schema:
    - message_type, project_id, status, message_id, timestamp (required)
    - iteration, score (optional, context-dependent)
    - thought_summary (required, quality-checked)
    - details_md (optional, markdown-validated)
    
    Args:
        msg: Progress message to validate
        expected_status: Expected status value
        expected_message_type: Expected message type (default: "ai_workflow_progress")
        expect_score: If True, score is required; if False, score should not be present
    """
    # 1. Required fields
    assert msg.get("message_type") == expected_message_type, \
        f"Wrong message type: {msg.get('message_type')}, expected: {expected_message_type}"
    assert msg.get("project_id"), "Missing project_id in progress message"
    assert msg.get("status") == expected_status, \
        f"Expected status '{expected_status}', got '{msg.get('status')}'"
    assert msg.get("message_id"), "Missing message_id in progress message"
    assert msg.get("timestamp"), "Missing timestamp in progress message"
    
    # 2. Validate thought_summary quality
    thought = msg.get("thought_summary", "")
    assert len(thought) > 10, \
        f"thought_summary should be substantive (>10 chars), got: '{thought[:50]}'"
    assert not thought.upper().startswith("ERROR"), \
        f"thought_summary should not start with ERROR: {thought[:100]}"
    
    # 3. Validate details_md if present
    if "details_md" in msg:
        details = msg["details_md"]
        assert isinstance(details, str), "details_md should be a string"
        assert len(details) > 0, "details_md should not be empty if present"
        # Check for markdown formatting indicators
        has_markdown = ("###" in details or "##" in details or 
                       "\n-" in details or "\n*" in details or
                       "**" in details)
        assert has_markdown, \
            f"details_md should contain markdown formatting: {details[:100]}"
    
    # 4. Validate score based on expectation
    if expect_score:
        # Only 'evaluating' status should have score
        assert "score" in msg, f"{expected_status} message should include score"
        score = msg["score"]
        assert isinstance(score, (int, float)), \
            f"Score should be numeric, got {type(score)}"
        assert 0.0 <= score <= 1.0, \
            f"Score {score} out of range [0,1]"
    else:
        # Other statuses should NOT have score
        assert "score" not in msg, \
            f"{expected_status} message should not include score, but got: {msg.get('score')}"
    
    # 5. Validate iteration if present
    if "iteration" in msg:
        iteration = msg["iteration"]
        assert isinstance(iteration, int), \
            f"Iteration should be int, got {type(iteration)}"
        assert iteration >= 1, \
            f"Iteration should be >=1, got {iteration}"


def validate_retrieving_context_message(msg: Dict[str, Any], expected_message_type: str = "ai_workflow_progress") -> None:
    """
    Specific validation for retrieving_context status messages.
    
    These messages should include details_md with:
    - "### Retrieved context" header
    - List of items or references retrieved
    
    Args:
        msg: Progress message to validate
        expected_message_type: Expected message type (default: "ai_workflow_progress")
    """
    validate_workflow_progress_message(msg, "retrieving_context", expected_message_type)
    
    details = msg.get("details_md", "")
    assert details, "retrieving_context should have details_md"
    assert "### Retrieved context" in details or "retrieved context" in details.lower(), \
        f"retrieving_context should show retrieved context header: {details[:200]}"
    assert ("Items:" in details or "Top refs:" in details or 
            "- " in details or "* " in details), \
        f"Should list retrieved items or references: {details[:200]}"


def validate_backlog_bundle_comprehensive(bundle: Dict[str, Any], project_id: str) -> None:
    """
    Comprehensive validation of AI tasks/backlog generation bundle structure.
    
    Validates complete backlog bundle including:
    - Project and prompt IDs
    - Epics with tasks structure
    - Score and quality thresholds
    - Assumptions and risks
    - Markdown text generation
    - Clarification questions (if low score)
    """
    # 1. Required top-level fields
    assert bundle.get("project_id") == project_id, \
        f"project_id mismatch: expected {project_id}, got {bundle.get('project_id')}"
    assert "prompt_id" in bundle, "Missing prompt_id in bundle"
    assert "epics" in bundle, "Missing epics in bundle"
    assert "score" in bundle, "Missing score in bundle"
    
    # 2. Validate score range
    score = bundle.get("score", 0.0)
    assert isinstance(score, (int, float)), f"Score should be numeric, got {type(score)}"
    assert 0.0 <= score <= 1.0, f"Score {score} out of range [0,1]"
    
    # 3. Validate epics structure
    epics = bundle.get("epics", [])
    assert isinstance(epics, list), "Epics should be a list"
    assert len(epics) > 0, "Should have at least one epic in generated backlog"
    
    for i, epic in enumerate(epics):
        # Required epic fields
        assert "id" in epic, f"Epic {i} missing id"
        assert "title" in epic, f"Epic {i} missing title"
        assert "description" in epic, f"Epic {i} missing description"
        assert "tasks" in epic, f"Epic {i} missing tasks"
        
        # Validate content quality
        assert len(epic["title"]) > 5, \
            f"Epic {i} title too short: '{epic['title']}'"
        assert len(epic["description"]) > 20, \
            f"Epic {i} description too short: '{epic['description'][:50]}'"
        
        # Validate tasks
        tasks = epic["tasks"]
        assert isinstance(tasks, list), f"Epic {i} tasks should be a list"
        assert len(tasks) > 0, \
            f"Epic '{epic['title']}' should have at least one task"
        
        for j, task in enumerate(tasks):
            assert "id" in task, f"Epic {i} task {j} missing id"
            assert "title" in task, f"Epic {i} task {j} missing title"
            assert len(task["title"]) > 5, \
                f"Epic {i} task {j} title too short: '{task['title']}'"
            
            # Optional but recommended fields
            if "description" in task:
                assert len(task["description"]) > 10, \
                    f"Epic {i} task {j} description too short"
            if "acceptance_criteria" in task:
                assert isinstance(task["acceptance_criteria"], list), \
                    f"Epic {i} task {j} acceptance_criteria should be a list"
    
    # 4. Validate assumptions and risks
    assert "assumptions" in bundle, "Missing assumptions in bundle"
    assert "risks" in bundle, "Missing risks in bundle"
    assert isinstance(bundle["assumptions"], list), "Assumptions should be a list"
    assert isinstance(bundle["risks"], list), "Risks should be a list"
    
    # Validate mock response content from TasksBacklogEngineerHandler
    # Mock returns epics with IDs like "EPIC-001", "EPIC-002" and tasks like "TASK-001"
    # and content about "Document Upload", "Cloud Storage", "Cross-Device Access", etc.
    all_text = " ".join(
        str(epic.get("title", "")) + " " + str(epic.get("description", ""))
        for epic in epics
    ).lower()
    mock_epic_keywords = ["document", "upload", "storage", "cloud", "device", "access", "file", "mobile", "api"]
    found_epic_keywords = [kw for kw in mock_epic_keywords if kw in all_text]
    assert len(found_epic_keywords) >= 2, \
        f"Epics should contain mock response content (Smallpdf features), found: {found_epic_keywords}"
    
    # Check for mock task IDs pattern (e.g., "TASK-001", "EPIC-001")
    all_ids = [epic.get("id", "") for epic in epics]
    for epic in epics:
        all_ids.extend([task.get("id", "") for task in epic.get("tasks", [])])
    id_text = " ".join(all_ids)
    # Should contain structured IDs (not just random strings)
    assert any(char.isdigit() for char in id_text), \
        "Epic/Task IDs should contain numbers (following mock pattern)"
    
    # 5. Validate markdown text if present
    if "markdown_text" in bundle:
        markdown = bundle["markdown_text"]
        assert isinstance(markdown, str), "markdown_text should be a string"
        assert len(markdown) > 100, "markdown_text should be substantive"
        assert ("# Generated Backlog" in markdown or "## Epic:" in markdown or
                "# " in markdown or "## " in markdown), \
            "Markdown should contain proper headings"
        
        # Verify at least some epic titles appear in markdown
        epic_titles_in_markdown = sum(1 for epic in epics if epic["title"] in markdown)
        assert epic_titles_in_markdown >= len(epics) // 2, \
            f"At least half of epic titles should appear in markdown (found {epic_titles_in_markdown}/{len(epics)})"
    
    # 6. Validate clarification questions if score is low
    if score < 0.70:
        assert "clarification_questions" in bundle, \
            f"Low score ({score}) should have clarification questions"
        questions = bundle.get("clarification_questions", [])
        assert len(questions) > 0, \
            f"Low score ({score}) should have at least one clarification question"
        
        for i, q in enumerate(questions):
            assert "id" in q, f"Question {i} missing id"
            assert "text" in q, f"Question {i} missing text"
            assert len(q["text"]) > 10, \
                f"Question {i} text too short: '{q['text']}'"
            
            # Optional but recommended fields
            if "expected_impact" in q:
                assert len(q["expected_impact"]) > 5, \
                    f"Question {i} expected_impact too short"
            if "axis" in q:
                assert q["axis"] in ["precision", "grounding", "response_relevancy", "completeness"], \
                    f"Question {i} has invalid axis: {q['axis']}"
            if "expected_score_gain" in q:
                assert 0.0 <= q["expected_score_gain"] <= 1.0, \
                    f"Question {i} expected_score_gain out of range"


class TestUIRequirementsWorkflow:
    def _simulate_gitlab_oauth_connection(self, session: requests.Session, ui_base: str) -> None:
        """
        Simulate GitLab OAuth connection flow programmatically.
        
        This mimics what a user would do in the browser:
        1. GET /auth/gitlab/authorize - initiates OAuth, redirects to GitLab
        2. GitLab mock immediately redirects back with code
        3. GET /auth/gitlab/callback - exchanges code for token, stores in Redis
        
        Args:
            session: Authenticated requests session (from SSO login)
            ui_base: Base URL of UI service
        """
        # Initiate GitLab OAuth flow
        # The UI service will redirect to gitlab-mock /oauth/authorize
        # which will immediately redirect back with an auth code
        response = session.get(
            f"{ui_base}/auth/gitlab/authorize",
            params={"redirect_uri": "/"},
            allow_redirects=True,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        
        assert response.status_code == TestConstants.HTTP_OK, (
            f"GitLab OAuth connection failed: {response.status_code} {response.text}"
        )
        
        # Verify GitLab connection status
        status_response = session.get(
            f"{ui_base}/auth/gitlab/status",
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        assert status_response.status_code == TestConstants.HTTP_OK
        status_data = status_response.json()
        assert status_data.get("connected") is True, (
            f"GitLab not connected after OAuth flow: {status_data}"
        )
    
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
            
            # Validate mock response content from EngineerHandler
            # Mock returns requirements with IDs like "BR-1", "BR-2", "FR-1", "FR-2"
            # and titles about "Upload", "Storage", "Smallpdf", etc.
            all_reqs = bundle.get("business_requirements", []) + bundle.get("functional_requirements", [])
            if len(all_reqs) > 0:
                # Check that requirements have expected structure
                first_req = all_reqs[0]
                assert "id" in first_req, "Requirement should have id field"
                assert "title" in first_req, "Requirement should have title field"
                assert "description" in first_req, "Requirement should have description field"
                
                # Check for mock content keywords (Smallpdf-related terms)
                all_text = " ".join(
                    str(r.get("title", "")) + " " + str(r.get("description", ""))
                    for r in all_reqs
                ).lower()
                mock_keywords = ["upload", "storage", "file", "document", "device", "access", "mobile"]
                found_keywords = [kw for kw in mock_keywords if kw in all_text]
                assert len(found_keywords) >= 2, \
                    f"Requirements should contain mock response content (Smallpdf features), found: {found_keywords}"
        finally:
            try:
                redis_monitor.stop_ai_workflow_monitoring()  # type: ignore[attr-defined]
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


