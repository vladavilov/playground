"""
Mock GitLab service for local development.
Simulates GitLab OAuth and API endpoints.
"""
import base64
import hashlib
import secrets
import time
from typing import Dict, Any, Optional, List
from fastapi import FastAPI, Request, Query, HTTPException, Header
from fastapi.responses import JSONResponse, RedirectResponse
from pydantic import BaseModel
import structlog
import uvicorn

from config import settings
from shared.src.configuration.logging_config import configure_logging

# Configure shared JSON logging
configure_logging()
logger = structlog.get_logger(__name__)

app = FastAPI(
    title="Mock GitLab Service",
    description="Simulates GitLab OAuth and API for local development"
)

# In-memory storage for OAuth flows
auth_codes: Dict[str, Dict[str, Any]] = {}
access_tokens: Dict[str, Dict[str, Any]] = {}

# In-memory storage for projects, groups, epics, and issues
projects: Dict[str, Dict[str, Any]] = {}
groups: Dict[int, Dict[str, Any]] = {}
epics: Dict[int, Dict[str, Any]] = {}
issues: Dict[int, Dict[str, Any]] = {}

# Counters for auto-incrementing IDs
epic_counter = 1
issue_counter = 1
link_counter = 1

# Link storage
epic_links: Dict[int, Dict[str, Any]] = {}  # link_id -> link_data
issue_links: Dict[int, Dict[str, Any]] = {}  # link_id -> link_data
epic_issue_links: Dict[int, Dict[str, Any]] = {}  # link_id -> link_data


# Pydantic models for request bodies
class EpicCreateRequest(BaseModel):
    title: str
    description: Optional[str] = ""
    labels: Optional[str] = None


class EpicUpdateRequest(BaseModel):
    title: Optional[str] = None
    description: Optional[str] = None
    labels: Optional[str] = None


class IssueCreateRequest(BaseModel):
    title: str
    description: Optional[str] = ""
    labels: Optional[List[str]] = None


class IssueUpdateRequest(BaseModel):
    title: Optional[str] = None
    description: Optional[str] = None
    labels: Optional[List[str]] = None


class IssueLinkRequest(BaseModel):
    target_project_id: str
    target_issue_iid: int
    link_type: str = "relates_to"


class EpicRelatedLinkRequest(BaseModel):
    target_group_id: int
    target_epic_iid: int
    link_type: str = "relates_to"


class EpicIssueLinkRequest(BaseModel):
    issue_id: int


def validate_bearer_token(authorization: Optional[str], private_token: Optional[str] = None) -> Optional[Dict[str, Any]]:
    """
    Validate Bearer token or PRIVATE-TOKEN and return token data if valid.
    
    Supports both authentication methods:
    - Authorization: Bearer <token> (standard OAuth)
    - PRIVATE-TOKEN: <token> (python-gitlab default)
    """
    token = None
    
    # Check Authorization header (Bearer token)
    if authorization and authorization.startswith("Bearer "):
        token = authorization.replace("Bearer ", "")
        logger.debug("Received Bearer token", token_preview=f"{token[:8]}..." if token else None)
    # Check PRIVATE-TOKEN header (python-gitlab)
    elif private_token:
        token = private_token
        logger.debug("Received PRIVATE-TOKEN", token_preview=f"{token[:8]}..." if token else None)
    else:
        logger.warning("No authentication token provided", has_auth=bool(authorization), has_private=bool(private_token))
    
    if not token:
        return None
    
    token_exists = token in access_tokens
    logger.debug("Token validation", token_exists=token_exists, total_tokens=len(access_tokens))
    return access_tokens.get(token)


def require_auth(
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
) -> Dict[str, Any]:
    """
    Dependency to require valid authentication.
    
    Accepts both:
    - Authorization: Bearer <token>
    - PRIVATE-TOKEN: <token> (used by python-gitlab)
    """
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    return token_data


def normalize_project_id(project_id: str) -> str:
    """Normalize project ID (handle both UUID and numeric formats)."""
    return project_id


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
    
    This endpoint validates the client_id against configured OAuth credentials
    and generates an authorization code for the token exchange step.
    """
    logger.info(
        "OAuth authorization request received",
        client_id=client_id[:20] + "..." if len(client_id) > 20 else client_id,
        redirect_uri=redirect_uri,
        response_type=response_type,
        scope=scope,
        has_code_challenge=bool(code_challenge),
        expected_client_id=settings.GITLAB_OAUTH_CLIENT_ID[:20] + "..." if len(settings.GITLAB_OAUTH_CLIENT_ID) > 20 else settings.GITLAB_OAUTH_CLIENT_ID
    )
    
    if client_id != settings.GITLAB_OAUTH_CLIENT_ID:
        logger.warning(
            "OAuth authorization rejected: client_id mismatch",
            received_client_id=client_id[:20] + "..." if len(client_id) > 20 else client_id,
            expected_client_id=settings.GITLAB_OAUTH_CLIENT_ID[:20] + "..." if len(settings.GITLAB_OAUTH_CLIENT_ID) > 20 else settings.GITLAB_OAUTH_CLIENT_ID
        )
        return JSONResponse(
            {"error": "invalid_client", "error_description": "Invalid client_id"},
            status_code=400
        )
    
    if response_type != "code":
        logger.warning("OAuth authorization rejected: unsupported response_type", response_type=response_type)
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
    
    logger.info(
        "OAuth authorization code generated",
        auth_code_length=len(auth_code),
        redirect_uri=redirect_uri,
        total_auth_codes=len(auth_codes)
    )
    
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
    
    Authentication Methods:
    1. Form-based: client_id and client_secret in request body
    2. HTTP Basic Auth: client_id:client_secret in Authorization header
    """
    form_data = await request.form()
    
    grant_type = form_data.get("grant_type")
    code = form_data.get("code")
    redirect_uri = form_data.get("redirect_uri")
    code_verifier = form_data.get("code_verifier")
    
    # Extract client credentials from form data or HTTP Basic Auth header
    client_id = form_data.get("client_id")
    client_secret = form_data.get("client_secret")
    auth_method = "form"
    
    # If not in form, check Authorization header for HTTP Basic Auth
    if not client_id or not client_secret:
        auth_header = request.headers.get("Authorization", "")
        if auth_header.startswith("Basic "):
            try:
                # Decode Base64 credentials
                credentials = base64.b64decode(auth_header[6:]).decode("utf-8")
                client_id, client_secret = credentials.split(":", 1)
                auth_method = "basic_auth"
                logger.info("Client credentials extracted from HTTP Basic Auth header")
            except Exception as e:
                logger.error("Failed to parse HTTP Basic Auth header", error=str(e))
                return JSONResponse(
                    {"error": "invalid_client", "error_description": "Invalid Authorization header format"},
                    status_code=401
                )
    
    logger.info(
        "OAuth token request received",
        grant_type=grant_type,
        client_id=client_id[:20] + "..." if client_id and len(client_id) > 20 else client_id,
        has_client_secret=bool(client_secret),
        has_code=bool(code),
        has_code_verifier=bool(code_verifier),
        redirect_uri=redirect_uri,
        auth_method=auth_method,
        expected_client_id=settings.GITLAB_OAUTH_CLIENT_ID[:20] + "..." if len(settings.GITLAB_OAUTH_CLIENT_ID) > 20 else settings.GITLAB_OAUTH_CLIENT_ID
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
    
    logger.info(
        "OAuth token issued successfully",
        client_id=client_id[:20] + "..." if len(client_id) > 20 else client_id,
        access_token_length=len(access_token),
        expires_in=7200,
        scope=auth_data["scope"],
        total_active_tokens=len(access_tokens)
    )
    
    return JSONResponse({
        "access_token": access_token,
        "token_type": "bearer",
        "expires_in": 7200,
        "refresh_token": refresh_token,
        "created_at": int(time.time())
    })


@app.on_event("startup")
async def initialize_mock_data():
    """Initialize mock projects and groups on startup."""
    # Create a default group
    groups[settings.MOCK_GROUP_ID] = {
        "id": settings.MOCK_GROUP_ID,
        "name": "Mock Group",
        "path": "mock-group",
        "description": "Default mock group for testing",
        "visibility": "private",
        "web_url": f"http://localhost:8011/groups/mock-group"
    }
    
    # Create a default project (supports both UUID and numeric ID)
    default_project = {
        "id": settings.MOCK_PROJECT_ID,
        "name": "Mock Project",
        "path": "mock-project",
        "description": "Default mock project for testing",
        "visibility": "private",
        "web_url": f"http://localhost:8011/mock-group/mock-project",
        "namespace": {
            "id": settings.MOCK_GROUP_ID,
            "name": "Mock Group",
            "path": "mock-group",
            "kind": "group",
            "full_path": "mock-group"
        }
    }
    
    # Store with both numeric and common UUID formats
    projects[str(settings.MOCK_PROJECT_ID)] = default_project
    projects["492f6a56-5d16-4544-b6a6-4355ef8c3904"] = default_project
    
    logger.info("Mock data initialized", projects=len(projects), groups=len(groups))


@app.get("/api/v4/user")
async def get_current_user(
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """Get current authenticated user information."""
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    return {
        "id": settings.MOCK_USER_ID,
        "username": settings.MOCK_USERNAME,
        "name": "Mock User",
        "email": settings.MOCK_USER_EMAIL,
        "state": "active",
        "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
        "web_url": f"http://localhost:8011/{settings.MOCK_USERNAME}",
        "created_at": "2024-01-01T00:00:00.000Z",
        "is_admin": False
    }


@app.get("/api/v4/projects/{project_id}")
async def get_project(
    project_id: str,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """Get project by numeric ID - generates mock data dynamically."""
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    normalized_id = normalize_project_id(project_id)
    project = projects.get(normalized_id)
    
    # If not in stored projects, generate mock data dynamically
    if not project:
        try:
            # Generate deterministic group/project from numeric ID
            numeric_id = int(project_id)
            group_id = numeric_id // 10  # Derive group ID
            
            project = {
                "id": numeric_id,
                "name": f"Mock Project {numeric_id}",
                "path": f"mock-project-{numeric_id}",
                "description": f"Dynamically generated mock project for ID {numeric_id}",
                "visibility": "private",
                "web_url": f"http://localhost:8011/mock-group-{group_id}/mock-project-{numeric_id}",
                "namespace": {
                    "id": group_id,
                    "name": f"Mock Group {group_id}",
                    "path": f"mock-group-{group_id}",
                    "kind": "group",
                    "full_path": f"mock-group-{group_id}"
                }
            }
        except ValueError:
            raise HTTPException(status_code=404, detail="Project not found")
    
    return project


@app.get("/api/v4/projects/{full_path:path}")
async def get_project_by_path(
    full_path: str,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """Get project by path - generates mock data from path."""
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    # Generate mock project from path
    path_parts = full_path.split('/')
    project_name = path_parts[-1] if path_parts else "project"
    namespace_path = '/'.join(path_parts[:-1]) if len(path_parts) > 1 else "namespace"
    
    # Generate deterministic ID from path (same path = same ID)
    project_id = int(hashlib.md5(full_path.encode()).hexdigest()[:8], 16) % 10000
    
    return {
        "id": project_id,
        "name": project_name.replace('-', ' ').title(),
        "path": project_name,
        "path_with_namespace": full_path,
        "description": f"Mock project for {full_path}",
        "visibility": "private",
        "web_url": f"http://localhost:8011/{full_path}",
        "namespace": {
            "id": project_id // 10,
            "name": namespace_path.split('/')[-1].replace('-', ' ').title(),
            "path": namespace_path,
            "kind": "group",
            "full_path": namespace_path
        }
    }



@app.get("/api/v4/groups/{group_id}")
async def get_group(
    group_id: int,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """Get group by ID - generates mock data dynamically."""
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    group = groups.get(group_id)
    
    # If not in stored groups, generate mock data dynamically
    if not group:
        group = {
            "id": group_id,
            "name": f"Mock Group {group_id}",
            "path": f"mock-group-{group_id}",
            "description": f"Dynamically generated mock group for ID {group_id}",
            "visibility": "private",
            "web_url": f"http://localhost:8011/groups/mock-group-{group_id}"
        }
    
    return group


@app.get("/api/v4/groups/{group_id}/epics")
async def list_group_epics(
    group_id: int,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """List epics for a group - always returns mock data."""
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    # Generate deterministic epic IDs from group ID
    epic_id_1 = (group_id * 100) + 1
    epic_id_2 = (group_id * 100) + 2
    
    # Always return mock epics for any group
    group_epics = [
        {
            "id": epic_id_1,
            "iid": 1,
            "group_id": group_id,
            "title": f"Epic: Planning Phase for Group {group_id}",
            "description": f"Strategic planning epic for mock group {group_id}",
            "state": "opened",
            "web_url": f"http://localhost:8011/groups/mock-group-{group_id}/-/epics/1",
            "author": {
                "id": settings.MOCK_USER_ID,
                "username": settings.MOCK_USERNAME,
                "name": "Mock User",
                "state": "active",
                "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
                "web_url": f"http://localhost:8011/{settings.MOCK_USERNAME}"
            },
            "start_date": "2024-01-01",
            "end_date": "2024-12-31",
            "created_at": "2024-01-01T00:00:00.000Z",
            "updated_at": "2024-01-15T00:00:00.000Z",
            "labels": ["planning", "mock"],
            "upvotes": 5,
            "downvotes": 0
        },
        {
            "id": epic_id_2,
            "iid": 2,
            "group_id": group_id,
            "title": f"Epic: Implementation Phase for Group {group_id}",
            "description": f"Implementation epic for mock group {group_id}",
            "state": "opened",
            "web_url": f"http://localhost:8011/groups/mock-group-{group_id}/-/epics/2",
            "author": {
                "id": settings.MOCK_USER_ID,
                "username": settings.MOCK_USERNAME,
                "name": "Mock User",
                "state": "active",
                "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
                "web_url": f"http://localhost:8011/{settings.MOCK_USERNAME}"
            },
            "start_date": "2024-02-01",
            "end_date": "2024-06-30",
            "created_at": "2024-01-10T00:00:00.000Z",
            "updated_at": "2024-01-20T00:00:00.000Z",
            "labels": ["implementation", "mock"],
            "upvotes": 3,
            "downvotes": 0
        }
    ]
    
    return group_epics


@app.post("/api/v4/groups/{group_id}/epics", status_code=201)
async def create_epic(
    group_id: int,
    request: EpicCreateRequest,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """Create a new epic in a group."""
    global epic_counter
    
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    if group_id not in groups:
        raise HTTPException(status_code=404, detail="Group not found")
    
    epic_id = epic_counter
    epic_counter += 1
    
    labels = request.labels.split(",") if request.labels else []
    
    epic = {
        "id": epic_id,
        "iid": epic_id,
        "group_id": group_id,
        "title": request.title,
        "description": request.description or "",
        "state": "opened",
        "web_url": f"http://localhost:8011/groups/group-{group_id}/-/epics/{epic_id}",
        "author": {
            "id": settings.MOCK_USER_ID,
            "username": settings.MOCK_USERNAME,
            "name": "Mock User",
            "state": "active",
            "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
            "web_url": f"http://localhost:8011/{settings.MOCK_USERNAME}"
        },
        "start_date": None,
        "end_date": None,
        "created_at": time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime()),
        "updated_at": time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime()),
        "labels": labels,
        "upvotes": 0,
        "downvotes": 0
    }
    
    epics[epic_id] = epic
    logger.info("Epic created", epic_id=epic_id, group_id=group_id, title=request.title)
    
    return epic


@app.put("/api/v4/groups/{group_id}/epics/{epic_id}")
async def update_epic(
    group_id: int,
    epic_id: int,
    request: EpicUpdateRequest,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """Update an existing epic."""
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    if group_id not in groups:
        raise HTTPException(status_code=404, detail="Group not found")
    
    if epic_id not in epics:
        raise HTTPException(status_code=404, detail="Epic not found")
    
    epic = epics[epic_id]
    
    if request.title is not None:
        epic["title"] = request.title
    if request.description is not None:
        epic["description"] = request.description
    if request.labels is not None:
        epic["labels"] = request.labels.split(",") if request.labels else []
    
    epic["updated_at"] = time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime())
    
    logger.info("Epic updated", epic_id=epic_id, group_id=group_id)
    
    return epic


@app.get("/api/v4/groups/{group_id}/epics/{epic_id}")
async def get_epic(
    group_id: int,
    epic_id: int,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """Get a specific epic by ID."""
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    if group_id not in groups:
        raise HTTPException(status_code=404, detail="Group not found")
    
    if epic_id not in epics:
        raise HTTPException(status_code=404, detail="Epic not found")
    
    return epics[epic_id]


@app.get("/api/v4/groups/{group_id}/epics/{epic_iid}/issues")
async def list_epic_issues(
    group_id: int,
    epic_iid: int,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """List issues associated with an epic."""
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    if group_id not in groups:
        raise HTTPException(status_code=404, detail="Group not found")
    
    # Return stored issues (mock data for backward compatibility)
    epic_issues = [
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
    
    return epic_issues


@app.get("/api/v4/projects/{project_id}/issues")
async def list_project_issues(
    project_id: str,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """List issues for a project - always returns mock data."""
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    # Parse project ID
    try:
        numeric_project_id = int(project_id)
    except ValueError:
        normalized_id = normalize_project_id(project_id)
        if normalized_id in projects:
            numeric_project_id = projects[normalized_id]["id"]
        else:
            raise HTTPException(status_code=404, detail="Project not found")
    
    # Generate deterministic issue IDs from project ID
    issue_id_1 = (numeric_project_id * 1000) + 1
    issue_id_2 = (numeric_project_id * 1000) + 2
    issue_id_3 = (numeric_project_id * 1000) + 3
    
    # Always return mock issues for any project
    project_issues = [
        {
            "id": issue_id_1,
            "iid": 1,
            "project_id": numeric_project_id,
            "title": f"Issue: Setup infrastructure for project {numeric_project_id}",
            "description": f"Initial infrastructure setup for mock project {numeric_project_id}",
            "state": "opened",
            "created_at": "2024-01-05T00:00:00.000Z",
            "updated_at": "2024-01-10T00:00:00.000Z",
            "closed_at": None,
            "labels": ["infrastructure", "setup"],
            "milestone": None,
            "assignees": [],
            "author": {
                "id": settings.MOCK_USER_ID,
                "username": settings.MOCK_USERNAME,
                "name": "Mock User",
                "state": "active",
                "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
                "web_url": f"http://localhost:8011/{settings.MOCK_USERNAME}"
            },
            "user_notes_count": 0,
            "upvotes": 1,
            "downvotes": 0,
            "due_date": None,
            "confidential": False,
            "web_url": f"http://localhost:8011/project-{numeric_project_id}/issues/1",
            "time_stats": {
                "time_estimate": 0,
                "total_time_spent": 0,
                "human_time_estimate": None,
                "human_total_time_spent": None
            }
        },
        {
            "id": issue_id_2,
            "iid": 2,
            "project_id": numeric_project_id,
            "title": f"Issue: Implement core features for project {numeric_project_id}",
            "description": f"Core feature implementation for mock project {numeric_project_id}",
            "state": "opened",
            "created_at": "2024-01-06T00:00:00.000Z",
            "updated_at": "2024-01-12T00:00:00.000Z",
            "closed_at": None,
            "labels": ["feature", "development"],
            "milestone": None,
            "assignees": [],
            "author": {
                "id": settings.MOCK_USER_ID,
                "username": settings.MOCK_USERNAME,
                "name": "Mock User",
                "state": "active",
                "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
                "web_url": f"http://localhost:8011/{settings.MOCK_USERNAME}"
            },
            "user_notes_count": 1,
            "upvotes": 2,
            "downvotes": 0,
            "due_date": None,
            "confidential": False,
            "web_url": f"http://localhost:8011/project-{numeric_project_id}/issues/2",
            "time_stats": {
                "time_estimate": 0,
                "total_time_spent": 0,
                "human_time_estimate": None,
                "human_total_time_spent": None
            }
        },
        {
            "id": issue_id_3,
            "iid": 3,
            "project_id": numeric_project_id,
            "title": f"Issue: Testing and documentation for project {numeric_project_id}",
            "description": f"Testing and documentation for mock project {numeric_project_id}",
            "state": "opened",
            "created_at": "2024-01-07T00:00:00.000Z",
            "updated_at": "2024-01-13T00:00:00.000Z",
            "closed_at": None,
            "labels": ["testing", "documentation"],
            "milestone": None,
            "assignees": [],
            "author": {
                "id": settings.MOCK_USER_ID,
                "username": settings.MOCK_USERNAME,
                "name": "Mock User",
                "state": "active",
                "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
                "web_url": f"http://localhost:8011/{settings.MOCK_USERNAME}"
            },
            "user_notes_count": 0,
            "upvotes": 0,
            "downvotes": 0,
            "due_date": None,
            "confidential": False,
            "web_url": f"http://localhost:8011/project-{numeric_project_id}/issues/3",
            "time_stats": {
                "time_estimate": 0,
                "total_time_spent": 0,
                "human_time_estimate": None,
                "human_total_time_spent": None
            }
        }
    ]
    
    return project_issues


@app.post("/api/v4/projects/{project_id}/issues", status_code=201)
async def create_issue(
    project_id: str,
    request: IssueCreateRequest,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """Create a new issue in a project."""
    global issue_counter
    
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    normalized_id = normalize_project_id(project_id)
    if normalized_id not in projects:
        raise HTTPException(status_code=404, detail="Project not found")
    
    issue_id = issue_counter
    issue_counter += 1
    
    issue = {
        "id": issue_id,
        "iid": issue_id,
        "project_id": normalized_id,
        "title": request.title,
        "description": request.description or "",
        "state": "opened",
        "created_at": time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime()),
        "updated_at": time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime()),
        "closed_at": None,
        "labels": request.labels or [],
        "milestone": None,
        "assignees": [],
        "author": {
            "id": settings.MOCK_USER_ID,
            "username": settings.MOCK_USERNAME,
            "name": "Mock User",
            "state": "active",
            "avatar_url": "https://www.gravatar.com/avatar/mock?s=80&d=identicon",
            "web_url": f"http://localhost:8011/{settings.MOCK_USERNAME}"
        },
        "user_notes_count": 0,
        "upvotes": 0,
        "downvotes": 0,
        "due_date": None,
        "confidential": False,
        "web_url": f"http://localhost:8011/project-{normalized_id}/issues/{issue_id}",
        "time_stats": {
            "time_estimate": 0,
            "total_time_spent": 0,
            "human_time_estimate": None,
            "human_total_time_spent": None
        }
    }
    
    issues[issue_id] = issue
    logger.info("Issue created", issue_id=issue_id, project_id=normalized_id, title=request.title)
    
    return issue


@app.put("/api/v4/projects/{project_id}/issues/{issue_iid}")
async def update_issue(
    project_id: str,
    issue_iid: int,
    request: IssueUpdateRequest,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """Update an existing issue."""
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    normalized_id = normalize_project_id(project_id)
    if normalized_id not in projects:
        raise HTTPException(status_code=404, detail="Project not found")
    
    if issue_iid not in issues:
        raise HTTPException(status_code=404, detail="Issue not found")
    
    issue = issues[issue_iid]
    
    if request.title is not None:
        issue["title"] = request.title
    if request.description is not None:
        issue["description"] = request.description
    if request.labels is not None:
        issue["labels"] = request.labels
    
    issue["updated_at"] = time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime())
    
    logger.info("Issue updated", issue_id=issue_iid, project_id=normalized_id)
    
    return issue


@app.get("/api/v4/projects/{project_id}/issues/{issue_iid}")
async def get_issue(
    project_id: str,
    issue_iid: int,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """Get a specific issue by ID."""
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    normalized_id = normalize_project_id(project_id)
    if normalized_id not in projects:
        raise HTTPException(status_code=404, detail="Project not found")
    
    if issue_iid not in issues:
        raise HTTPException(status_code=404, detail="Issue not found")
    
    return issues[issue_iid]


@app.post("/api/v4/projects/{project_id}/issues/{issue_iid}/links", status_code=201)
async def create_issue_link(
    project_id: str,
    issue_iid: int,
    request: IssueLinkRequest,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """Create a link between two issues."""
    global link_counter
    
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    normalized_id = normalize_project_id(project_id)
    if normalized_id not in projects:
        raise HTTPException(status_code=404, detail="Project not found")
    
    if issue_iid not in issues:
        raise HTTPException(status_code=404, detail="Source issue not found")
    
    # Validate target issue exists
    target_issue_iid = request.target_issue_iid
    if target_issue_iid not in issues:
        raise HTTPException(status_code=404, detail="Target issue not found")
    
    link_id = link_counter
    link_counter += 1
    
    link = {
        "id": link_id,
        "source_project_id": normalized_id,
        "source_issue_iid": issue_iid,
        "target_project_id": request.target_project_id,
        "target_issue_iid": target_issue_iid,
        "link_type": request.link_type,
        "created_at": time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime())
    }
    
    issue_links[link_id] = link
    logger.info(
        "Issue link created",
        link_id=link_id,
        source_iid=issue_iid,
        target_iid=target_issue_iid,
        link_type=request.link_type
    )
    
    return link


@app.post("/api/v4/groups/{group_id}/epics/{epic_iid}/related_epics", status_code=201)
async def create_epic_related_link(
    group_id: int,
    epic_iid: int,
    request: EpicRelatedLinkRequest,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """Create a related epic link."""
    global link_counter
    
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    if group_id not in groups:
        raise HTTPException(status_code=404, detail="Group not found")
    
    if epic_iid not in epics:
        raise HTTPException(status_code=404, detail="Source epic not found")
    
    # Validate target epic exists
    target_epic_iid = request.target_epic_iid
    if target_epic_iid not in epics:
        raise HTTPException(status_code=404, detail="Target epic not found")
    
    link_id = link_counter
    link_counter += 1
    
    link = {
        "id": link_id,
        "source_group_id": group_id,
        "source_epic_iid": epic_iid,
        "target_group_id": request.target_group_id,
        "target_epic_iid": target_epic_iid,
        "link_type": request.link_type,
        "created_at": time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime())
    }
    
    epic_links[link_id] = link
    logger.info(
        "Epic related link created",
        link_id=link_id,
        source_iid=epic_iid,
        target_iid=target_epic_iid,
        link_type=request.link_type
    )
    
    return link


@app.post("/api/v4/groups/{group_id}/epics/{epic_iid}/issues", status_code=201)
async def link_issue_to_epic(
    group_id: int,
    epic_iid: int,
    request: EpicIssueLinkRequest,
    authorization: Optional[str] = Header(None),
    private_token: Optional[str] = Header(None, alias="PRIVATE-TOKEN")
):
    """Link an issue to an epic (parent-child relationship)."""
    global link_counter
    
    token_data = validate_bearer_token(authorization, private_token)
    if not token_data:
        raise HTTPException(status_code=401, detail="Unauthorized")
    
    if group_id not in groups:
        raise HTTPException(status_code=404, detail="Group not found")
    
    if epic_iid not in epics:
        raise HTTPException(status_code=404, detail="Epic not found")
    
    # Validate issue exists (using issue.id, not iid)
    issue_id = request.issue_id
    issue_found = None
    for iss in issues.values():
        if iss["id"] == issue_id:
            issue_found = iss
            break
    
    if not issue_found:
        raise HTTPException(status_code=404, detail="Issue not found")
    
    link_id = link_counter
    link_counter += 1
    
    link = {
        "id": link_id,
        "epic_iid": epic_iid,
        "issue_id": issue_id,
        "group_id": group_id,
        "created_at": time.strftime("%Y-%m-%dT%H:%M:%S.000Z", time.gmtime())
    }
    
    epic_issue_links[link_id] = link
    logger.info(
        "Issue linked to epic",
        link_id=link_id,
        epic_iid=epic_iid,
        issue_id=issue_id
    )
    
    # Return the issue object (as GitLab API does)
    return issue_found


if __name__ == "__main__":
    import os
    port = int(os.getenv("API_PORT", settings.PORT))
    
    # SSL configuration for HTTPS
    ssl_keyfile = os.getenv("SSL_KEYFILE", "/app/gitlab_mock_service/gitlab.key")
    ssl_certfile = os.getenv("SSL_CERTFILE", "/app/gitlab_mock_service/gitlab.crt")
    
    uvicorn.run(
        app,
        host=settings.HOST,
        port=port,
        log_level="info",
        ssl_keyfile=ssl_keyfile,
        ssl_certfile=ssl_certfile
    )

