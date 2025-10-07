"""
Response structure validators for e2e tests.

This module provides response validation operations following
Single Responsibility Principle - focused on validating response structures.
"""

from __future__ import annotations

from typing import Dict, Any


class ResponseValidators:
    """
    Validators for API response structures.
    
    Provides validators for:
    - Workflow progress messages
    - Backlog bundle structure
    - Requirements bundle structure
    - Context retrieval messages
    """
    
    @staticmethod
    def validate_workflow_progress_message(
        msg: Dict[str, Any],
        expected_status: str,
        expected_message_type: str = "ai_requirements_progress",
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
            expected_message_type: Expected message type (default: "ai_requirements_progress")
            expect_score: If True, score is required; if False, score should not be present
            
        Raises:
            AssertionError: If message doesn't conform to expected structure
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
    
    @staticmethod
    def validate_retrieving_context_message(
        msg: Dict[str, Any],
        expected_message_type: str = "ai_requirements_progress"
    ) -> None:
        """
        Specific validation for retrieving_context status messages.
        
        These messages should include details_md with:
        - "### Retrieved context" header
        - List of items or references retrieved
        
        Args:
            msg: Progress message to validate
            expected_message_type: Expected message type (default: "ai_requirements_progress")
            
        Raises:
            AssertionError: If message doesn't conform to expected structure
        """
        ResponseValidators.validate_workflow_progress_message(
            msg, "retrieving_context", expected_message_type
        )
        
        details = msg.get("details_md", "")
        assert details, "retrieving_context should have details_md"
        assert "### Retrieved context" in details or "retrieved context" in details.lower(), \
            f"retrieving_context should show retrieved context header: {details[:200]}"
        assert ("Items:" in details or "Top refs:" in details or
                "- " in details or "* " in details), \
            f"Should list retrieved items or references: {details[:200]}"
    
    @staticmethod
    def validate_backlog_bundle_comprehensive(
        bundle: Dict[str, Any],
        project_id: str
    ) -> None:
        """
        Comprehensive validation of AI tasks/backlog generation bundle structure.
        
        Validates complete backlog bundle including:
        - Project and prompt IDs
        - Epics with tasks structure
        - Score and quality thresholds
        - Assumptions and risks
        - Markdown text generation
        - Clarification questions (if low score)
        
        Args:
            bundle: Backlog bundle response to validate
            project_id: Expected project UUID
            
        Raises:
            AssertionError: If bundle doesn't conform to expected structure
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

