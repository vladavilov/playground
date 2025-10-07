import json
import re
from typing import Any, Dict, List, Optional
import structlog
from handlers.base import BaseHandler

logger = structlog.get_logger(__name__)


class GraphRAGExtractionHandler(BaseHandler):
    """Handles GraphRAG entity and relationship extraction requests."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        # Check if it's a continuation or loop-check prompt first
        if messages:
            last_msg = messages[-1] if messages else {}
            last_content = str(last_msg.get("content", "")) if isinstance(last_msg, dict) else ""
            
            if "MANY entities and relationships were missed" in last_content:
                return True
            if "Answer Y if there are still entities or relationships" in last_content:
                return True
        
        # Main extraction prompt detection
        return (
            ("entity_types:" in lower_text)
            and ("text:" in lower_text)
            and (
                "format each entity as (\"entity\"" in lower_text
                or "when finished, output" in lower_text
            )
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        # Handle gleaning continuation prompt
        if messages:
            last_msg = messages[-1] if messages else {}
            last_content = str(last_msg.get("content", "")) if isinstance(last_msg, dict) else ""
            
            if "MANY entities and relationships were missed" in last_content and "Add them below using the same format" in last_content:
                return ""
            
            if "Answer Y if there are still entities or relationships" in last_content and "Please answer with a single letter Y or N" in last_content:
                return "N"
        
        # Parse delimiters
        def _extract(pattern: str, text: str) -> Optional[str]:
            m = re.search(pattern, text, flags=re.IGNORECASE | re.DOTALL)
            return m.group(1) if m else None

        tuple_delim = _extract(r"Format each entity as \(\"entity\"(.*?)<entity_name>", combined_text)
        if not tuple_delim:
            tuple_delim = _extract(r"\(\"entity\"(.*?)<entity_name>", combined_text)
        record_delim = _extract(r"Use \*\*(.+?)\*\* as the list delimiter", combined_text)
        completion_delim = _extract(r"When finished, output\s+(\S+)", combined_text)

        td = tuple_delim or "<|>"
        rd = record_delim or "##"
        cd = completion_delim or "<|COMPLETE|>"

        entity_types_line = _extract(r"Entity_types:\s*([^\n\r]+)", combined_text) or "ORGANIZATION,PERSON,GEO,EVENT"
        parsed_types = [t.strip().upper() for t in entity_types_line.split(",") if t.strip()]
        if not parsed_types:
            parsed_types = ["ORGANIZATION", "PERSON", "GEO", "EVENT"]

        def ent(name: str, etype: str, desc: str) -> str:
            return f"(\"entity\"{td}{name.upper()}{td}{etype.upper()}{td}{desc})"

        def rel(src: str, tgt: str, desc: str, weight: float | int = 2) -> str:
            return f"(\"relationship\"{td}{src.upper()}{td}{tgt.upper()}{td}{desc}{td}{weight})"

        ORG = "ORGANIZATION"
        EVT = "EVENT"

        entity_records = [
            ent("Smallpdf", ORG, "Smallpdf is a platform for managing and transforming digital documents."),
            ent("Storage", ORG, "Optional feature that stores processed files when enabled."),
            ent("Mobile App", ORG, "Smallpdf Mobile App that syncs files to the online portal."),
            ent("G Suite App", ORG, "Integration that can be enabled for an organization."),
            ent("E-signatures", EVT, "Request and collect electronic signatures on documents."),
            ent("Enhance", EVT, "One-click enhancements for uploaded documents."),
            ent("Convert", EVT, "Action to convert files to other formats."),
            ent("Compress", EVT, "Action to compress files to smaller size."),
            ent("Modify", EVT, "Action to modify documents using available tools."),
        ]

        relationship_records = [
            rel("Smallpdf", "Storage", "Smallpdf stores processed files when Storage is enabled.", 2),
            rel("Mobile App", "Smallpdf", "The Mobile App syncs files to the Smallpdf online portal.", 2),
            rel("Smallpdf", "E-signatures", "Smallpdf supports requesting electronic signatures.", 2),
            rel("Smallpdf", "G Suite App", "Smallpdf can enable the G Suite App for organizations.", 2),
            rel("Smallpdf", "Enhance", "Smallpdf provides one-click enhancements.", 1),
            rel("Smallpdf", "Convert", "Smallpdf offers right-click convert actions.", 1),
            rel("Smallpdf", "Compress", "Smallpdf offers right-click compress actions.", 1),
            rel("Smallpdf", "Modify", "Smallpdf offers right-click modify actions.", 1),
        ]

        generated = ("\n" + rd + "\n").join([*entity_records, *relationship_records]) + f"\n{rd}\n{cd}"
        return generated


class CommunityReportGraphHandler(BaseHandler):
    """Handles graph-based community report generation."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        return (
            "write a comprehensive report of a community" in lower_text
            and '"findings"' in lower_text
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        report = {
            "title": "Smallpdf Workspace and Tools",
            "summary": (
                "Smallpdf provides a unified workspace for digital documents, enabling upload, organization, and sharing. "
                "When Storage is enabled, processed files are retained and accessible across devices, including sync from the Mobile App to the web portal. "
                "Right‑click actions support Convert/Compress/Modify, and collaboration features include e‑signatures, large file sending, and a G Suite App."
            ),
            "rating": 3.5,
            "rating_explanation": "Moderate impact: broad feature set with low inherent risk in the provided text.",
            "findings": [
                {
                    "summary": "Unified document management",
                    "explanation": "Users can upload, organize, and share documents in one place, reducing fragmentation across tools."
                },
                {
                    "summary": "Optional persistent Storage",
                    "explanation": "Processed files are stored when Storage is enabled, improving continuity and retrieval."
                },
                {
                    "summary": "Cross‑device access and sync",
                    "explanation": "Files are accessible on computer, phone, or tablet; the Mobile App syncs to the online portal."
                },
                {
                    "summary": "Productivity actions and collaboration",
                    "explanation": "Right‑click actions accelerate file conversions and edits; collaboration includes e‑signatures and large file transfer."
                }
            ]
        }
        return json.dumps(report)


class CommunityReportTextHandler(BaseHandler):
    """Handles text-based community report generation with date_range."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        return (
            "date range" in lower_text
            and "Return output as a well-formed JSON-formatted string" in combined_text
            and "importance rating" in lower_text
        )

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        report = {
            "title": "Smallpdf Document Experience Overview",
            "summary": (
                "Smallpdf centralizes digital document tasks: upload, organize, share, and enhance. "
                "Storage (optional) persists processed files. Cross‑device access with Mobile App sync extends availability. "
                "Actions include Convert/Compress/Modify; collaboration features include e‑signatures, large file sending, and G Suite integration."
            ),
            "rating": 3.8,
            "rating_explanation": "Important for productivity; the text highlights core capabilities without risk events.",
            "findings": [
                {"summary": "Central hub", "explanation": "Digital documents managed end‑to‑end in one place."},
                {"summary": "Sync and access", "explanation": "Mobile App sync to portal; access on computer/phone/tablet."}
            ],
            "date_range": ["", ""]
        }
        return json.dumps(report)

