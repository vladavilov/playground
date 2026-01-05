"""Handlers for Docling remote VLM prompts (document_processing_service).

Docling uses OpenAI-compatible chat completions endpoints for picture descriptions.
In Azure mode it hits `/openai/deployments/{deployment}/chat/completions?...`,
and in OpenAI-compatible mode it hits `/v1/chat/completions`.

We keep this handler intentionally narrow so it does not overlap with
GraphRAG/workflow handlers.
"""

from __future__ import annotations

from typing import Any, Dict, List
import structlog

from handlers.base import BaseHandler

logger = structlog.get_logger(__name__)


def _has_vision_image_part(messages: List[Dict[str, Any]]) -> bool:
    """Detect OpenAI vision-style message content with image_url parts."""
    for m in messages:
        if not isinstance(m, dict):
            continue
        content = m.get("content")
        if isinstance(content, list):
            for part in content:
                if not isinstance(part, dict):
                    continue
                if part.get("type") == "image_url":
                    return True
                if "image_url" in part:
                    return True
        if isinstance(content, dict) and ("image_url" in content or content.get("type") == "image_url"):
            return True
    return False


class DoclingVlmHandler(BaseHandler):
    """Return deterministic non-empty text for Docling picture description calls."""

    def can_handle(self, messages: List[Dict[str, Any]], combined_text: str, lower_text: str) -> bool:
        # Primary discriminator: Docling prompt (default is explicit)
        if "docling format" in lower_text or "docling" in lower_text:
            return True

        # Secondary discriminator: vision-style content (Docling remote picture description)
        # Keep this behind an extra prompt hint to avoid catching unrelated vision users.
        if _has_vision_image_part(messages) and ("picture description" in lower_text or "describe the image" in lower_text):
            return True

        return False

    def generate_response(self, messages: List[Dict[str, Any]], combined_text: str, model: str) -> str:
        logger.info("docling_vlm_mock_response", model=model)

        # Keep output stable and obviously "Docling-ish" while still being plain text.
        # Docling downstream only requires non-empty content that looks like converted page text.
        return (
            "<docling>\n"
            "<page>\n"
            "<title>Mock Page</title>\n"
            "<paragraph>Converted page content in docling format with detailed descriptions.</paragraph>\n"
            "<figure alt=\"Mock figure description\">A diagram/table detected on the page.</figure>\n"
            "</page>\n"
            "</docling>"
        )


