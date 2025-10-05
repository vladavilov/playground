import json
import re
from typing import Any, Dict, List, Optional
import structlog
from handlers.base import BaseHandler

logger = structlog.get_logger(__name__)


class ExtractClaimsHandler(BaseHandler):
    """Handles extract_claims prompt with tuple/record/completion delimiters."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        return (
            "-target activity-" in lower_text
            and "extract all entities that match the predefined entity specification" in combined_text
            and "Format each claim as (" in combined_text
            and "When finished, output" in combined_text
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        system_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "system"
        )
        user_content = "".join(
            str(m.get("content", "")) for m in messages if isinstance(m, dict)
        )
        full_text = f"{system_content}\n{user_content}"
        
        def _extract(pattern: str, text: str) -> Optional[str]:
            m = re.search(pattern, text, flags=re.IGNORECASE | re.DOTALL)
            return m.group(1) if m else None

        record_delim = _extract(r"Use \*\*(.+?)\*\* as the list delimiter", full_text) or "##"
        completion_delim = _extract(r"When finished, output\s+(\S+)", full_text) or "<|COMPLETE|>"
        tuple_delim = "<|>"

        def claim(subject: str, obj: str, ctype: str, status: str, start: str, end: str, desc: str, source: str) -> str:
            return (
                f"{subject.upper()}{tuple_delim}{obj.upper()}{tuple_delim}{ctype.upper()}"
                f"{tuple_delim}{status.upper()}{tuple_delim}{start}{tuple_delim}{end}{tuple_delim}{desc}{tuple_delim}{source}"
            )

        c1 = claim(
            "Smallpdf", "Storage", "Feature Availability", "TRUE",
            "NONE", "NONE",
            "Storage retains processed files when enabled.",
            "\"When you enable the 'Storage' option, we'll also store all processed files here.\"",
        )
        c2 = claim(
            "Mobile App", "Smallpdf", "Synchronization", "TRUE",
            "NONE", "NONE",
            "Mobile App syncs files to the online portal.",
            "\"We'll also sync files from the Smallpdf Mobile App to our online portal\"",
        )
        return f"({c1})\n{record_delim}\n({c2})\n{completion_delim}"


class GlobalSearchHandler(BaseHandler):
    """Handles global search (map) prompts with JSON points."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        return (
            "The response should be JSON formatted as follows" in combined_text
            and '"points"' in combined_text
            and "map" in lower_text
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        points = {
            "points": [
                {"description": "Smallpdf centralizes document upload, organization, and sharing.", "score": 85},
                {"description": "Storage persists processed files when enabled, improving continuity.", "score": 70},
            ]
        }
        return json.dumps(points)


class BasicSearchHandler(BaseHandler):
    """Handles basic_search_system_prompt with markdown summary."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        system_content = "".join(
            str(m.get("content", ""))
            for m in messages
            if isinstance(m, dict) and m.get("role") == "system"
        )
        
        return (
            "---Role---" in system_content
            and "responding to questions about data in the tables provided" in system_content
            and "---Data tables---" in combined_text
            and "Style the response in markdown" in combined_text
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        return (
            "### Overview\n\n"
            "Smallpdf offers a unified document experience: upload, organize, and share in one place. When Storage is enabled, processed files are retained. Users can access files across devices with Mobile App sync. Right‑click actions (Convert, Compress, Modify) and collaboration (e‑signatures, large file sending, G Suite App) are supported."
        )


class QuestionGenerationHandler(BaseHandler):
    """Handles question generation prompts."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        return "generate a bulleted list of" in combined_text and "Use - marks as bullet points" in combined_text

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        return (
            "- What retention and security apply when Storage is enabled?\n"
            "- Which file formats are supported for Convert/Compress/Modify?\n"
            "- How does Mobile App sync frequency and conflict resolution work?\n"
            "- What are limits for large file sending (size, expiry)?\n"
            "- Can admins manage the G Suite App organization‑wide settings?"
        )


class SummarizeDescriptionsHandler(BaseHandler):
    """Handles summarize_descriptions prompt."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        return (
            "You are a helpful assistant responsible for generating a comprehensive summary of the data" in combined_text
            and "Description List:" in combined_text
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        return (
            "Smallpdf centralizes digital document tasks, enabling users to upload, organize, share, and enhance files in one place. "
            "With optional Storage, processed files remain available for later use. Files are accessible across devices, and the Mobile App syncs to the online portal. "
            "Right‑click actions (Convert, Compress, Modify) and collaboration features (e‑signatures, large file sending, G Suite App) streamline common workflows."
        )

