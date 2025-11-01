"""Handlers for AI Tasks Service (backlog generation workflow)."""

import json
from typing import Any, Dict, List
import structlog
from handlers.base import BaseHandler

logger = structlog.get_logger(__name__)


class TasksRequirementsAnalystHandler(BaseHandler):
    """Handles RequirementsAnalyst requests from ai_tasks_service."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        system_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "system"
        )
        system_lower = system_content.lower()
        
        return (
            "senior technical architect" in system_lower
            and "intents" in system_lower
            and "entities" in system_lower
            and "constraints" in system_lower
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        response = {
            "intents": [
                "Upload files",
                "Enable optional Storage for processed files",
                "Access files across devices",
                "Enhance documents in one click",
                "Collaborate and request e-signatures",
            ],
            "entities": [
                "Document",
                "File",
                "Storage",
                "Library",
                "Mobile App",
                "User",
                "E-Signature",
                "Folder",
            ],
            "constraints": [
                "Seamless multi-device experience",
                "Secure cloud storage",
                "Fast document processing",
            ],
        }
        return json.dumps(response)


class TasksBacklogEngineerHandler(BaseHandler):
    """Handles BacklogEngineer requests from ai_tasks_service."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        system_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "system"
        )
        system_lower = system_content.lower()
        user_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "user"
        )
        
        return (
            "senior technical lead" in system_lower
            and "agile expert" in system_lower
            and "### Requirements" in user_content
            and "### Technical Context" in user_content
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        response = {
            "epics": [
                {
                    "id": "EPIC-001",
                    "title": "Document Upload and Organization",
                    "description": "Enable users to upload and organize digital documents with folder structure and library management",
                    "tasks": [
                        {
                            "id": "TASK-001",
                            "title": "Implement file upload API endpoint",
                            "description": "Create RESTful endpoint for uploading PDF, DOCX, and image files with validation",
                            "acceptance_criteria": [
                                "Given a signed-in user, When they upload a PDF file, Then the file is stored and appears in their library",
                                "Given an upload request, When file size exceeds 100MB, Then return appropriate error message",
                                "Given uploaded file, When stored, Then generate unique file ID and timestamp",
                            ],
                            "dependencies": [],
                        },
                        {
                            "id": "TASK-002",
                            "title": "Build folder management system",
                            "description": "Implement folder creation, deletion, and document organization functionality",
                            "acceptance_criteria": [
                                "Given files in library, When user creates folder, Then folder persists in database",
                                "Given files in library, When user organizes into folders, Then structure is maintained across sessions",
                                "Given nested folders, When user moves documents, Then parent-child relationships update correctly",
                            ],
                            "dependencies": ["TASK-001"],
                        },
                    ],
                },
                {
                    "id": "EPIC-002",
                    "title": "Cloud Storage Integration",
                    "description": "Implement optional cloud storage feature for processed files with user-controlled enablement",
                    "tasks": [
                        {
                            "id": "TASK-003",
                            "title": "Design storage configuration API",
                            "description": "Create API for users to enable/disable cloud storage with preference persistence",
                            "acceptance_criteria": [
                                "Given user settings, When storage is enabled, Then all processed files are saved",
                                "Given user settings, When storage is disabled, Then files are not persisted beyond download",
                                "Given storage toggle, When changed, Then preference saves immediately to user profile",
                            ],
                            "dependencies": [],
                        },
                        {
                            "id": "TASK-004",
                            "title": "Implement cloud storage backend",
                            "description": "Integrate with cloud storage provider (AWS S3/Azure Blob) for file persistence",
                            "acceptance_criteria": [
                                "Given storage enabled, When file is processed, Then file uploads to cloud storage",
                                "Given cloud failure, When upload fails, Then retry with exponential backoff",
                                "Given stored file, When accessed, Then presigned URL generated with 1-hour expiry",
                            ],
                            "dependencies": ["TASK-003"],
                        },
                    ],
                },
                {
                    "id": "EPIC-003",
                    "title": "Cross-Device Access",
                    "description": "Enable seamless file access across web, mobile, and tablet devices with real-time sync",
                    "tasks": [
                        {
                            "id": "TASK-005",
                            "title": "Build mobile API endpoints",
                            "description": "Create mobile-optimized REST API for file listing, upload, and download",
                            "acceptance_criteria": [
                                "Given mobile app, When user signs in, Then same files visible as web portal",
                                "Given mobile upload, When file uploaded, Then syncs to web portal within 5 seconds",
                                "Given poor network, When API called, Then gracefully handle timeouts with retry",
                            ],
                            "dependencies": [],
                        },
                    ],
                },
            ],
            "assumptions": [
                "Users have network access on all devices",
                "Cloud storage provider (AWS S3 or Azure Blob) is available",
                "Mobile app will be developed separately and consume these APIs",
                "User authentication system is in place",
            ],
            "risks": [
                "Large file uploads may timeout on slow networks",
                "Cloud storage costs may scale with user adoption",
                "Cross-device sync latency expectations unclear",
                "File format support scope may expand beyond initial PDF/DOCX",
            ],
        }
        return json.dumps(response)


class TasksConsistencyAuditorHandler(BaseHandler):
    """Handles ConsistencyAuditor requests from ai_tasks_service."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        system_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "system"
        )
        system_lower = system_content.lower()
        user_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "user"
        )
        
        return (
            ("senior qa lead" in system_lower or "qa lead" in system_lower)
            and "audit" in system_lower
            and "### Requirements" in user_content
            and "### Backlog" in user_content
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        response = {
            "issues": [
                "Acceptance criteria for TASK-005 should specify actual sync latency target instead of 'within 5 seconds'",
            ],
            "suggestions": [
                "Add tasks for document enhancement features (compress, convert, edit)",
                "Include tasks for e-signature workflow implementation",
                "Consider adding collaboration features (sharing, commenting)",
                "Add monitoring and analytics for file processing performance",
            ],
            "overlaps": [],
        }
        return json.dumps(response)


class TasksEvaluatorHandler(BaseHandler):
    """Handles Evaluator requests from ai_tasks_service.
    
    Supports both OLD format (coverage/specificity/feasibility/duplication scores)
    and NEW format (rationale + gaps only, scores come from DeepEval).
    """

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        system_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "system"
        )
        system_lower = system_content.lower()
        user_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "user"
        )
        
        # Must be Backlog Evaluator context
        is_evaluator = "backlog evaluator" in system_lower
        has_user_sections = (
            "### Requirements" in user_content
            and ("### Backlog" in user_content or "### Backlog & Findings" in user_content)
        )
        
        # OLD format: requests all 4 score fields explicitly
        requests_scores = (
            "coverage" in system_content
            and "specificity" in system_content
            and "feasibility" in system_content
            and "duplication" in system_content
        )
        
        # NEW format: requests only rationale and gaps (DeepEval provides scores)
        requests_rationale_gaps = (
            "rationale" in system_lower
            and "gaps" in system_lower
            and ("assess backlog quality" in system_lower or "backlog quality" in system_lower)
        )
        
        return is_evaluator and has_user_sections and (requests_scores or requests_rationale_gaps)

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        logger.info("ai_tasks_evaluator")
        
        # Check which format is requested based on system prompt
        system_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "system"
        )
        
        # NEW format: only rationale + gaps (DeepEval scores used)
        if "rationale" in system_content.lower() and "gaps" in system_content.lower() and "score" not in system_content.lower():
            response = {
                "rationale": "The backlog demonstrates strong technical specificity with clear Given/When/Then acceptance criteria. Epic decomposition follows INVEST principles with well-defined dependencies.",
                "gaps": [
                    "Performance testing strategy not explicitly defined in acceptance criteria",
                    "Migration rollback procedures unclear for some components",
                    "Integration testing approach between systems needs detail"
                ],
            }
        # OLD format: coverage/specificity/feasibility/duplication scores
        else:
            response = {
                "coverage": 0.78,
                "specificity": 0.82,
                "feasibility": 0.75,
                "duplication": 0.95,
                "rationale": "Backlog covers core requirements (upload, storage, cross-device access). Technical details are well-specified with API designs and Given/When/Then criteria. Feasibility is good with clear cloud storage assumptions. Some enhancement and collaboration features need further decomposition. No significant duplicates detected.",
                "gaps": [
                    "One-click document enhancements not decomposed into tasks",
                    "E-signature and collaboration features not included in backlog",
                    "No tasks for mobile app implementation details",
                ],
            }
        
        return json.dumps(response)


class TasksClarificationStrategistHandler(BaseHandler):
    """Handles ClarificationStrategist requests from ai_tasks_service."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        system_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "system"
        )
        system_lower = system_content.lower()
        user_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "user"
        )
        
        return (
            "clarification strategist" in system_lower
            and "formulate" in system_lower
            and "Requirements:" in user_content
            and "Scores:" in user_content
            and "Target:" in user_content
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        response = {
            "questions": [
                {
                    "id": "CLR-001",
                    "text": "What specific document enhancement features are required (compress, convert, OCR, merge, split)?",
                },
                {
                    "id": "CLR-002",
                    "text": "What is the scope of e-signature functionality (internal only or third-party integration like DocuSign)?",
                },
                {
                    "id": "CLR-003",
                    "text": "What collaboration features are needed (real-time co-editing, comments, sharing permissions)?",
                },
                {
                    "id": "CLR-004",
                    "text": "What are the supported file formats and size limits for uploads?",
                },
            ],
            "focus_areas": [
                "coverage",
                "specificity",
            ],
        }
        return json.dumps(response)

