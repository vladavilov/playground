"""Session management utilities.

Provides session ID generation and management for UI service.
"""

from __future__ import annotations

from fastapi import Request


def get_session_id(request: Request) -> str:
    """
    Get or create session ID for the request.
    
    Session IDs are used as keys for MSAL token cache and other session data.
    
    Args:
        request: FastAPI request object
        
    Returns:
        Session ID string (UUID format)
    """
    sid = request.session.get("sid")
    if not sid:
        import uuid
        sid = str(uuid.uuid4())
        request.session["sid"] = sid
    return sid


