"""
Mock GitLab service for local development.
Simulates GitLab OAuth and API endpoints.
"""
import base64
import hashlib
import secrets
import time
from typing import Dict, Any, Optional
from fastapi import FastAPI, Request, Query
from fastapi.responses import JSONResponse, RedirectResponse
import structlog
import uvicorn

from config import settings

logger = structlog.get_logger(__name__)

app = FastAPI(
    title="Mock GitLab Service",
    description="Simulates GitLab OAuth and API for local development"
)

# In-memory storage for OAuth flows
auth_codes: Dict[str, Dict[str, Any]] = {}
access_tokens: Dict[str, Dict[str, Any]] = {}


@app.get("/health")
async def health():
    """Health check endpoint."""
    return {"status": "ok", "service": "gitlab-mock-service"}


@app.get("/oauth/authorize")
async def oauth_authorize(
    client_id: str = Query(...),
    redirect_uri: str = Query(...),
    response_type: str = Query(...),
    state: str = Query(...),
    scope: str = Query(default="read_api"),
    code_challenge: Optional[str] = Query(default=None),
    code_challenge_method: Optional[str] = Query(default=None)
):
    """
    GitLab OAuth authorization endpoint.
    Simulates user authorization and returns authorization code.
    """
    if client_id != settings.GITLAB_OAUTH_CLIENT_ID:
        return JSONResponse(
            {"error": "invalid_client", "error_description": "Invalid client_id"},
            status_code=400
        )
    
    if response_type != "code":
        return JSONResponse(
            {"error": "unsupported_response_type", "error_description": "Only 'code' response_type is supported"},
            status_code=400
        )
    
    # Generate authorization code
    auth_code = secrets.token_urlsafe(32)
    
    # Store authorization code with associated data
    auth_codes[auth_code] = {
        "client_id": client_id,
        "redirect_uri": redirect_uri,
        "scope": scope,
        "state": state,
        "code_challenge": code_challenge,
        "code_challenge_method": code_challenge_method,
        "created_at": time.time(),
        "used": False
    }
    
    # Redirect back to application with authorization code
    return RedirectResponse(
        url=f"{redirect_uri}?code={auth_code}&state={state}",
        status_code=302
    )


@app.post("/oauth/token")
async def oauth_token(request: Request):
    """
    GitLab OAuth token endpoint.
    Exchanges authorization code for access token.
    
    Supports both form-based and HTTP Basic authentication for client credentials
    per OAuth 2.0 specification (RFC 6749 Section 2.3).
    Also validates PKCE code_verifier if code_challenge was provided.
    """
    form_data = await request.form()
    
    grant_type = form_data.get("grant_type")
    code = form_data.get("code")
    redirect_uri = form_data.get("redirect_uri")
    code_verifier = form_data.get("code_verifier")
    
    # Extract client credentials from form data or HTTP Basic Auth header
    client_id = form_data.get("client_id")
    client_secret = form_data.get("client_secret")
    
    # If not in form, check Authorization header for HTTP Basic Auth
    if not client_id or not client_secret:
        auth_header = request.headers.get("Authorization", "")
        if auth_header.startswith("Basic "):
            try:
                # Decode Base64 credentials
                credentials = base64.b64decode(auth_header[6:]).decode("utf-8")
                client_id, client_secret = credentials.split(":", 1)
                logger.info("Client credentials extracted from HTTP Basic Auth header")
            except Exception as e:
                logger.error("Failed to parse HTTP Basic Auth header", error=str(e))
                return JSONResponse(
                    {"error": "invalid_client", "error_description": "Invalid Authorization header format"},
                    status_code=401
                )
    
    logger.info(
        "Token request received",
        grant_type=grant_type,
        client_id=client_id,
        has_client_secret=bool(client_secret),
        has_code=bool(code),
        has_code_verifier=bool(code_verifier),
        redirect_uri=redirect_uri
    )
    
    # Validate grant type
    if grant_type != "authorization_code":
        return JSONResponse(
            {"error": "unsupported_grant_type", "error_description": "Only 'authorization_code' grant type is supported"},
            status_code=400
        )
    
    # Validate client credentials
    if client_id != settings.GITLAB_OAUTH_CLIENT_ID or client_secret != settings.GITLAB_OAUTH_CLIENT_SECRET:
        logger.warning(
            "Client credential mismatch",
            received_client_id=client_id,
            expected_client_id=settings.GITLAB_OAUTH_CLIENT_ID,
            credentials_match=False
        )
        return JSONResponse(
            {"error": "invalid_client", "error_description": "Invalid client credentials"},
            status_code=401
        )
    
    # Validate authorization code
    if not code or code not in auth_codes:
        return JSONResponse(
            {"error": "invalid_grant", "error_description": "Invalid authorization code"},
            status_code=400
        )
    
    auth_data = auth_codes[code]
    
    # Check if code was already used
    if auth_data["used"]:
        return JSONResponse(
            {"error": "invalid_grant", "error_description": "Authorization code already used"},
            status_code=400
        )
    
    # Check if code is expired (5 minutes)
    if time.time() - auth_data["created_at"] > 300:
        return JSONResponse(
            {"error": "invalid_grant", "error_description": "Authorization code expired"},
            status_code=400
        )
    
    # Validate redirect URI
    if redirect_uri != auth_data["redirect_uri"]:
        return JSONResponse(
            {"error": "invalid_grant", "error_description": "Redirect URI mismatch"},
            status_code=400
        )
    
    # Validate PKCE if code_challenge was provided during authorization
    if auth_data.get("code_challenge"):
        if not code_verifier:
            logger.warning("PKCE code_verifier missing but code_challenge was provided")
            return JSONResponse(
                {"error": "invalid_grant", "error_description": "PKCE code_verifier required"},
                status_code=400
            )
        
        # Verify code_verifier matches code_challenge
        code_challenge_method = auth_data.get("code_challenge_method", "plain")
        if code_challenge_method == "S256":
            # SHA256 hash and base64url encode the verifier
            verifier_hash = hashlib.sha256(code_verifier.encode()).digest()
            computed_challenge = base64.urlsafe_b64encode(verifier_hash).decode().rstrip("=")
        else:
            # Plain method (not recommended but supported)
            computed_challenge = code_verifier
        
        if computed_challenge != auth_data["code_challenge"]:
            logger.warning(
                "PKCE validation failed",
                expected_challenge=auth_data["code_challenge"],
                computed_challenge=computed_challenge,
                method=code_challenge_method
            )
            return JSONResponse(
                {"error": "invalid_grant", "error_description": "PKCE validation failed"},
                status_code=400
            )
        
        logger.info("PKCE validation successful")
    
    # Mark code as used
    auth_data["used"] = True
    
    # Generate access token
    access_token = secrets.token_urlsafe(32)
    refresh_token = secrets.token_urlsafe(32)
    
    # Store access token
    access_tokens[access_token] = {
        "client_id": client_id,
        "scope": auth_data["scope"],
        "user_id": settings.MOCK_USER_ID,
        "created_at": time.time(),
        "expires_in": 7200
    }
    
    return JSONResponse({
        "access_token": access_token,
        "token_type": "bearer",
        "expires_in": 7200,
        "refresh_token": refresh_token,
        "created_at": int(time.time())
    })


@app.get("/api/v4/user")
async def get_current_user(request: Request):
    """Get current authenticated user information."""
    # Extract token from Authorization header
    auth_header = request.headers.get("Authorization", "")
    if not auth_header.startswith("Bearer "):
        return JSONResponse(
            {"error": "unauthorized", "error_description": "Missing or invalid authorization token"},
            status_code=401
        )
    
    token = auth_header.replace("Bearer ", "")
    
    if token not in access_tokens:
        return JSONResponse(
            {"error": "unauthorized", "error_description": "Invalid access token"},
            status_code=401
        )
    
    return {
        "id": settings.MOCK_USER_ID,
        "username": settings.MOCK_USERNAME,
        "name": "Mock User",
        "email": settings.MOCK_USER_EMAIL,
        "state": "active",
        "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
        "web_url": f"http://localhost:8006/{settings.MOCK_USERNAME}",
        "created_at": "2024-01-01T00:00:00.000Z",
        "is_admin": False
    }


@app.get("/api/v4/groups/{group_id}/epics")
async def list_group_epics(group_id: int, request: Request):
    """List epics for a group."""
    # Mock epics data
    epics = [
        {
            "id": 1,
            "iid": 1,
            "group_id": group_id,
            "title": "Migration Planning Epic",
            "description": "Plan and coordinate the COBOL to modern stack migration",
            "state": "opened",
            "web_url": f"http://localhost:8006/groups/group-{group_id}/-/epics/1",
            "author": {
                "id": settings.MOCK_USER_ID,
                "username": settings.MOCK_USERNAME,
                "name": "Mock User",
                "state": "active",
                "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
                "web_url": f"http://localhost:8006/{settings.MOCK_USERNAME}"
            },
            "start_date": "2024-01-01",
            "end_date": "2024-12-31",
            "created_at": "2024-01-01T00:00:00.000Z",
            "updated_at": "2024-01-15T00:00:00.000Z",
            "labels": ["migration", "planning"],
            "upvotes": 5,
            "downvotes": 0
        },
        {
            "id": 2,
            "iid": 2,
            "group_id": group_id,
            "title": "Risk Analytics Modernization",
            "description": "Modernize risk analytics components",
            "state": "opened",
            "web_url": f"http://localhost:8006/groups/group-{group_id}/-/epics/2",
            "author": {
                "id": settings.MOCK_USER_ID,
                "username": settings.MOCK_USERNAME,
                "name": "Mock User",
                "state": "active",
                "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
                "web_url": f"http://localhost:8006/{settings.MOCK_USERNAME}"
            },
            "start_date": "2024-02-01",
            "end_date": "2024-06-30",
            "created_at": "2024-01-10T00:00:00.000Z",
            "updated_at": "2024-01-20T00:00:00.000Z",
            "labels": ["risk-analytics", "modernization"],
            "upvotes": 3,
            "downvotes": 0
        }
    ]
    
    return epics


@app.get("/api/v4/groups/{group_id}/epics/{epic_iid}/issues")
async def list_epic_issues(group_id: int, epic_iid: int, request: Request):
    """List issues associated with an epic."""
    # Mock issues data
    issues = [
        {
            "id": 101,
            "iid": 1,
            "project_id": settings.MOCK_PROJECT_ID,
            "title": "Analyze COBOL codebase structure",
            "description": "Perform comprehensive analysis of existing COBOL codebase",
            "state": "opened",
            "created_at": "2024-01-05T00:00:00.000Z",
            "updated_at": "2024-01-10T00:00:00.000Z",
            "closed_at": None,
            "labels": ["analysis", "cobol"],
            "milestone": None,
            "assignees": [
                {
                    "id": settings.MOCK_USER_ID,
                    "username": settings.MOCK_USERNAME,
                    "name": "Mock User",
                    "state": "active",
                    "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
                    "web_url": f"http://localhost:8006/{settings.MOCK_USERNAME}"
                }
            ],
            "author": {
                "id": settings.MOCK_USER_ID,
                "username": settings.MOCK_USERNAME,
                "name": "Mock User",
                "state": "active",
                "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
                "web_url": f"http://localhost:8006/{settings.MOCK_USERNAME}"
            },
            "user_notes_count": 2,
            "upvotes": 1,
            "downvotes": 0,
            "due_date": "2024-01-31",
            "confidential": False,
            "web_url": f"http://localhost:8006/project-{settings.MOCK_PROJECT_ID}/issues/1",
            "time_stats": {
                "time_estimate": 0,
                "total_time_spent": 0,
                "human_time_estimate": None,
                "human_total_time_spent": None
            }
        },
        {
            "id": 102,
            "iid": 2,
            "project_id": settings.MOCK_PROJECT_ID,
            "title": "Identify dependencies and data flows",
            "description": "Map out all dependencies between COBOL modules and data flows",
            "state": "opened",
            "created_at": "2024-01-06T00:00:00.000Z",
            "updated_at": "2024-01-12T00:00:00.000Z",
            "closed_at": None,
            "labels": ["analysis", "dependencies"],
            "milestone": None,
            "assignees": [],
            "author": {
                "id": settings.MOCK_USER_ID,
                "username": settings.MOCK_USERNAME,
                "name": "Mock User",
                "state": "active",
                "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
                "web_url": f"http://localhost:8006/{settings.MOCK_USERNAME}"
            },
            "user_notes_count": 0,
            "upvotes": 0,
            "downvotes": 0,
            "due_date": "2024-02-15",
            "confidential": False,
            "web_url": f"http://localhost:8006/project-{settings.MOCK_PROJECT_ID}/issues/2",
            "time_stats": {
                "time_estimate": 0,
                "total_time_spent": 0,
                "human_time_estimate": None,
                "human_total_time_spent": None
            }
        }
    ]
    
    return issues


@app.get("/api/v4/projects/{project_id}/issues")
async def list_project_issues(project_id: int, request: Request):
    """List issues for a project."""
    # Return same mock data as epic issues
    return await list_epic_issues(settings.MOCK_GROUP_ID, 1, request)


if __name__ == "__main__":
    import os
    port = int(os.getenv("API_PORT", settings.PORT))
    uvicorn.run(
        app,
        host=settings.HOST,
        port=port,
        log_level="info"
    )

