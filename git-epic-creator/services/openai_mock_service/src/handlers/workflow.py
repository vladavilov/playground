import json
from typing import Any, Dict, List
import structlog
from handlers.base import BaseHandler

logger = structlog.get_logger(__name__)


class AnalystHandler(BaseHandler):
    """Handles PromptAnalyst requests."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        system_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "system"
        )
        system_lower = system_content.lower()
        
        return (
            ("senior requirements analyst" in system_lower)
            and ("respond only with json object" in system_lower)
            and ('"intents"' in system_content)
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        intents = {
            "intents": [
                "Upload files",
                "Enable optional Storage for processed files",
                "Access files across devices",
                "Enhance documents in one click",
                "Collaborate and request e-signatures",
            ]
        }
        return json.dumps(intents)


class EngineerHandler(BaseHandler):
    """Handles RequirementsEngineer requests."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        system_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "system"
        )
        system_lower = system_content.lower()
        user_content = "".join(
            str(m.get("content", "")) for m in messages if isinstance(m, dict)
        )
        
        return (
            "requirements engineer" in system_lower
            or "business_requirements" in system_content
            or "functional_requirements" in system_content
            or (
                "respond only with json object" in system_lower
                and "business_requirements" in system_content
            )
            or "'schema': {'business_requirements'" in user_content
            or '"business_requirements":' in user_content
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        payload = {
            "business_requirements": [
                {
                    "id": "BR-1",
                    "title": "Upload and organize digital documents",
                    "description": "Users can freely upload and organize digital documents within the Smallpdf experience.",
                    "acceptance_criteria": [
                        "Given a signed-in user, When they upload a PDF, Then the file appears in their library.",
                        "Given files in the library, When the user organizes them into folders, Then the structure persists.",
                    ],
                    "priority": "Must",
                },
                {
                    "id": "BR-2",
                    "title": "Optional Storage for processed files",
                    "description": "When 'Storage' is enabled, all processed files are stored and accessible later.",
                    "acceptance_criteria": [
                        "Given Storage is enabled, When a file is processed, Then it is saved to the user's stored files.",
                        "Given Storage is disabled, When a file is processed, Then it is not persisted beyond download.",
                    ],
                    "priority": "Must",
                },
                {
                    "id": "BR-3",
                    "title": "Access files across devices",
                    "description": "Users can access files on computer, phone, or tablet, with Mobile App sync to the portal.",
                    "acceptance_criteria": [
                        "Given stored files, When a user signs in on mobile, Then the same files are visible.",
                        "Given the Mobile App, When a file is uploaded there, Then it syncs and appears on the web portal.",
                    ],
                    "priority": "Should",
                },
            ],
            "functional_requirements": [
                {
                    "id": "FR-1",
                    "title": "Right-click context actions",
                    "description": "Provide right-click actions to convert, compress, or modify files.",
                    "acceptance_criteria": [
                        "Given a file in the library, When user right-clicks it, Then options include Convert, Compress, and Modify.",
                    ],
                    "priority": "Must",
                },
                {
                    "id": "FR-2",
                    "title": "Collaboration features",
                    "description": "Support requesting e-signatures, sending large files, and enabling the Smallpdf G Suite App.",
                    "acceptance_criteria": [
                        "Given a document, When user requests an e-signature, Then a signature workflow is initiated.",
                        "Given a document, When user selects 'Send large files', Then a shareable transfer link is created.",
                        "Given an admin, When they enable the Smallpdf G Suite App, Then the organization can use it.",
                    ],
                    "priority": "Should",
                },
                {
                    "id": "FR-3",
                    "title": "Enhance documents in one click",
                    "description": "Provide one-click enhancements.",
                    "acceptance_criteria": [
                        "Given an uploaded document, When 'Enhance' is clicked, Then the system applies predefined improvements.",
                    ],
                    "priority": "Could",
                },
            ],
            "assumptions": [
                "Users have network access on all devices.",
                "Mobile App account is linked to the same user identity.",
            ],
            "risks": [
                "Ambiguity around 'Enhance in one click' exact transformations.",
                "Large-file sending limits and quotas may impact UX.",
            ],
        }
        return json.dumps(payload)


class AuditorHandler(BaseHandler):
    """Handles ConsistencyAuditor requests."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        system_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "system"
        )
        user_content = "".join(
            str(m.get("content", "")) for m in messages if isinstance(m, dict)
        )
        
        auditor_markers = (
            "Return ONLY JSON with: {severity: number in [0,1], suggestions: string[]}",
            "senior requirements QA reviewer",
            "Critique the requirements",
        )
        
        return (
            any(marker in system_content for marker in auditor_markers)
            or ("'requirements':" in user_content)
            or ('"requirements":' in user_content)
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        audit = {
            "severity": 0.0,
            "suggestions": [
                "Clarify capabilities included in 'Enhance documents in one click'.",
                "Specify device support and sync latency for 'Access files anytime, anywhere'.",
                "Define size limits and retention for 'send large files'.",
                "State security controls when 'Storage' is enabled (e.g., encryption, retention).",
            ],
        }
        return json.dumps(audit)


class StrategistHandler(BaseHandler):
    """Handles QuestionStrategist requests."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        user_content = "".join(
            str(m.get("content", "")) for m in messages if isinstance(m, dict)
        )
        return ("axis_scores" in user_content) or ("questions" in user_content)

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        payload = [
            {
                "id": "Q1",
                "text": "What exact enhancements does 'Enhance in one click' perform?",
                "axis": "completeness",
                "priority": 1,
                "expected_score_gain": 0.15,
                "targets": [],
            },
            {
                "id": "Q2",
                "text": "What are the size limits, quotas, and expiry for 'send large files'?",
                "axis": "specificity",
                "priority": 1,
                "expected_score_gain": 0.12,
                "targets": [],
            },
            {
                "id": "Q3",
                "text": "What encryption and retention apply when 'Storage' is enabled?",
                "axis": "risk",
                "priority": 1,
                "expected_score_gain": 0.1,
                "targets": [],
            },
        ]
        return json.dumps({"questions": payload})

