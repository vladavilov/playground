import json
import re
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List
import structlog
from handlers.base import BaseHandler

logger = structlog.get_logger(__name__)


class FallbackGraphHandler(BaseHandler):
    """Default fallback handler that generates Smallpdf-grounded graph JSON."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        # This handler always returns True as it's the fallback
        return True

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        logger.warning("fallback_handler_used", 
                      message_preview=combined_text[:500] if combined_text else "",
                      message_length=len(combined_text) if combined_text else 0)
        
        # Check if this is a community analysis prompt
        if combined_text and combined_text.strip().startswith("You are an expert analyst tasked with summarizing communities in a knowledge graph."):
            logger.info("community_analysis_prompt_detected")
            return "some community summary"

        # Try to extract a UUID from the prompt; fall back to a new UUID
        project_id: str
        try:
            uuid_match = re.search(
                r"\b[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[1-5][0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}\b",
                combined_text or "",
            )
            project_id = uuid_match.group(0) if uuid_match else str(uuid.uuid4())
        except Exception:
            project_id = str(uuid.uuid4())

        now_iso = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

        # Build Smallpdf-grounded graph: PROJECT -> DOCUMENT -> REQUIREMENTs
        doc_title = "smallpdf_intro.txt"
        requirements: List[Dict[str, Any]] = [
            {"id": "R1", "title": "Upload and organize digital documents", "description": "Users can freely upload and organize digital documents within the Smallpdf experience."},
            {"id": "R2", "title": "Optional Storage for processed files", "description": "When 'Storage' is enabled, processed files are stored and accessible later."},
            {"id": "R3", "title": "Access files across devices", "description": "Users can access files on computer, phone, or tablet, with Mobile App sync to the portal."},
            {"id": "R4", "title": "Right-click context actions", "description": "Provide right-click actions to convert, compress, or modify files."},
            {"id": "R5", "title": "Enhance documents in one click", "description": "Provide one-click enhancements."},
            {"id": "R6", "title": "Collaboration features", "description": "Support requesting e-signatures, sending large files, and enabling the Smallpdf G Suite App."},
        ]

        nodes: List[Dict[str, Any]] = [
            {
                "id": "P",
                "label": "PROJECT",
                "properties": {
                    "project_id": project_id,
                    "title": f"Project {project_id[:8]}",
                    "creation_date": now_iso,
                    "modification_date": now_iso,
                },
            },
            {
                "id": "D0",
                "label": "DOCUMENT",
                "properties": {
                    "project_id": project_id,
                    "title": "Smallpdf Introduction",
                    "file_name": doc_title,
                    "file_type": "txt",
                    "content_type": "text/plain",
                    "creation_date": now_iso,
                    "modification_date": now_iso,
                },
            },
        ]
        # Append requirement nodes
        for r in requirements:
            nodes.append({
                "id": r["id"],
                "label": "REQUIREMENT",
                "properties": {
                    "project_id": project_id,
                    "title": r["title"],
                    "description": r["description"],
                    "creation_date": now_iso,
                    "modification_date": now_iso,
                },
            })

        relationships: List[Dict[str, Any]] = [
            {
                "type": "PROJECT_HAS_DOCUMENT",
                "start_node_id": "P",
                "end_node_id": "D0",
                "properties": {"source": "mock"},
            },
        ]
        for r in requirements:
            relationships.append({
                "type": "DOCUMENT_HAS_REQUIREMENT",
                "start_node_id": "D0",
                "end_node_id": r["id"],
                "properties": {"source": "mock"},
            })

        # Extracted entities from the Smallpdf text
        entity_specs: List[Dict[str, str]] = [
            {"id": "E1", "title": "Smallpdf"},
            {"id": "E2", "title": "Storage"},
            {"id": "E3", "title": "Mobile App"},
            {"id": "E4", "title": "G Suite App"},
            {"id": "E5", "title": "e-signatures"},
            {"id": "E6", "title": "convert"},
            {"id": "E7", "title": "compress"},
            {"id": "E8", "title": "modify"},
            {"id": "E9", "title": "computer"},
            {"id": "E10", "title": "phone"},
            {"id": "E11", "title": "tablet"},
            {"id": "E12", "title": "Enhance"},
        ]
        for e in entity_specs:
            nodes.append({
                "id": e["id"],
                "label": "ENTITY",
                "properties": {
                    "project_id": project_id,
                    "title": e["title"],
                    "creation_date": now_iso,
                    "modification_date": now_iso,
                },
            })

        # Map requirements to mentioned entities
        req_to_entities: Dict[str, List[str]] = {
            "R1": ["E1"],  # Smallpdf platform
            "R2": ["E2"],  # Storage option
            "R3": ["E3", "E9", "E10", "E11"],  # Mobile App and devices
            "R4": ["E6", "E7", "E8"],  # convert/compress/modify
            "R5": ["E12"],  # Enhance action
            "R6": ["E5", "E4"],  # e-signatures, G Suite App
        }
        for rid, eids in req_to_entities.items():
            for eid in eids:
                relationships.append({
                    "type": "REQUIREMENT_MENTIONS_ENTITY",
                    "start_node_id": rid,
                    "end_node_id": eid,
                    "properties": {"source": "mock"},
                })

        payload: Dict[str, Any] = {
            "nodes": nodes,
            "relationships": relationships,
        }

        return json.dumps(payload, ensure_ascii=False, indent=2)

