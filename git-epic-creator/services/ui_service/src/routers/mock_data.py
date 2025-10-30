"""Mock data for UI testing - ai-requirements-service and ai-tasks-service responses."""

from uuid import UUID
from typing import Dict, Any


def get_requirements_bundle_mock(project_id: UUID, prompt: str) -> Dict[str, Any]:
    """Mock successful RequirementsBundle with high score."""
    return {
        "prompt_id": "a1b2c3d4-e5f6-4789-a012-3456789abcde",
        "project_id": str(project_id),
        "business_requirements": [
            {
                "id": "BR-001",
                "title": "User Authentication System",
                "description": "System must provide secure user authentication mechanism",
                "rationale": "Essential for user identity management and security",
                "acceptance_criteria": [
                    "Users can register with email and password",
                    "Users can login with valid credentials",
                    "Failed login attempts are logged"
                ],
                "priority": "Must"
            }
        ],
        "functional_requirements": [
            {
                "id": "FR-001",
                "title": "OAuth2 Integration",
                "description": "Implement OAuth2 authentication flow",
                "rationale": "Industry standard for secure authentication",
                "acceptance_criteria": [
                    "Support authorization code flow",
                    "Token refresh mechanism implemented",
                    "Secure token storage"
                ],
                "priority": "Must"
            },
            {
                "id": "FR-002",
                "title": "Session Management",
                "description": "Manage user sessions securely",
                "rationale": "Required for stateful user interactions",
                "acceptance_criteria": [
                    "Sessions expire after 24 hours",
                    "Session invalidation on logout",
                    "Concurrent session handling"
                ],
                "priority": "Should"
            }
        ],
        "assumptions": [
            "OAuth2 provider is available and configured",
            "HTTPS is enforced in production environment"
        ],
        "risks": [
            "OAuth2 provider downtime may block authentication",
            "Session storage scaling may require distributed cache"
        ],
        "score": 0.85,
        "clarification_questions": None
    }


def get_requirements_bundle_needs_clarification_mock(project_id: UUID, prompt: str) -> Dict[str, Any]:
    """Mock RequirementsBundle with low score and clarification questions."""
    return {
        "prompt_id": "b2c3d4e5-f6a7-4890-b123-456789abcdef",
        "project_id": str(project_id),
        "business_requirements": [
            {
                "id": "BR-001",
                "title": "Authentication System",
                "description": "System needs authentication",
                "rationale": "Security requirement",
                "acceptance_criteria": [
                    "Users can login"
                ],
                "priority": "Must"
            }
        ],
        "functional_requirements": [],
        "assumptions": ["Standard web authentication"],
        "risks": ["Unclear security requirements"],
        "score": 0.55,
        "clarification_questions": [
            {
                "id": "Q1",
                "text": "What authentication mechanism should be supported (OAuth2, SAML, basic auth)?",
                "options": ["OAuth2", "SAML", "Basic Auth", "Multi-factor"],
                "expected_impact": "Will define technical implementation approach",
                "axis": "completeness",
                "priority": 1,
                "expected_score_gain": 0.15,
                "targets": ["BR-001"]
            },
            {
                "id": "Q2",
                "text": "What is the expected number of concurrent users?",
                "options": ["< 100", "100-1000", "1000-10000", "> 10000"],
                "expected_impact": "Will inform session storage and scaling design",
                "axis": "feasibility",
                "priority": 2,
                "expected_score_gain": 0.10,
                "targets": ["BR-001"]
            }
        ]
    }


def get_backlog_bundle_mock(project_id: UUID, message: str) -> Dict[str, Any]:
    """Mock successful GeneratedBacklogBundle with high score."""
    return {
        "prompt_id": "c3d4e5f6-a7b8-4901-c234-56789abcdef0",
        "project_id": str(project_id),
        "epics": [
            {
                "id": "EPIC-001",
                "title": "User Authentication System",
                "description": "Implement comprehensive user authentication with OAuth2 support",
                "tasks": [
                    {
                        "id": "TASK-001",
                        "title": "Implement OAuth2 authorization code flow",
                        "description": "Set up OAuth2 provider integration with authorization code grant type",
                        "acceptance_criteria": [
                            "Given user initiates login, when redirected to OAuth2 provider, then authorization code is returned",
                            "Given valid authorization code, when exchanged for tokens, then access and refresh tokens are received"
                        ],
                        "dependencies": [],
                        "similar": [
                            {
                                "kind": "issue",
                                "id": "42",
                                "status": "closed",
                                "similarity": 0.87,
                                "url": "https://gitlab.example.com/project/-/issues/42"
                            }
                        ]
                    },
                    {
                        "id": "TASK-002",
                        "title": "Implement token refresh mechanism",
                        "description": "Add automatic token refresh before expiration",
                        "acceptance_criteria": [
                            "Given access token expires in < 5 minutes, when refresh is triggered, then new tokens are obtained",
                            "Given refresh token is invalid, when refresh fails, then user is logged out"
                        ],
                        "dependencies": ["TASK-001"],
                        "similar": None
                    }
                ],
                "similar": None
            },
            {
                "id": "EPIC-002",
                "title": "Session Management",
                "description": "Secure session handling and lifecycle management",
                "tasks": [
                    {
                        "id": "TASK-003",
                        "title": "Implement session storage",
                        "description": "Set up Redis-based session store with TTL",
                        "acceptance_criteria": [
                            "Given user logs in, when session created, then session stored in Redis with 24h TTL",
                            "Given session expires, when user accesses protected resource, then 401 is returned"
                        ],
                        "dependencies": [],
                        "similar": None
                    },
                    {
                        "id": "TASK-004",
                        "title": "Implement logout functionality",
                        "description": "Clean session invalidation on logout",
                        "acceptance_criteria": [
                            "Given user logs out, when logout is confirmed, then session is removed from Redis",
                            "Given session is invalidated, when user tries to use it, then 401 is returned"
                        ],
                        "dependencies": ["TASK-003"],
                        "similar": None
                    }
                ],
                "similar": None
            }
        ],
        "assumptions": [
            "Redis is available for session storage",
            "OAuth2 provider supports refresh tokens"
        ],
        "risks": [
            "Redis cluster failure may invalidate all sessions",
            "OAuth2 provider rate limits may affect user experience"
        ],
        "score": 0.82,
        "coverage_components": {
            "coverage": 0.85,
            "specificity": 0.80,
            "feasibility": 0.83,
            "duplication": 0.78
        },
        "clarification_questions": None,
        "markdown_text": """# Backlog

## Epic: User Authentication System

Implement comprehensive user authentication with OAuth2 support

### TASK-001: Implement OAuth2 authorization code flow
Set up OAuth2 provider integration with authorization code grant type

**Acceptance Criteria:**
- Given user initiates login, when redirected to OAuth2 provider, then authorization code is returned
- Given valid authorization code, when exchanged for tokens, then access and refresh tokens are received

**Dependencies:** None

### TASK-002: Implement token refresh mechanism
Add automatic token refresh before expiration

**Acceptance Criteria:**
- Given access token expires in < 5 minutes, when refresh is triggered, then new tokens are obtained
- Given refresh token is invalid, when refresh fails, then user is logged out

**Dependencies:** TASK-001

## Epic: Session Management

Secure session handling and lifecycle management

### TASK-003: Implement session storage
Set up Redis-based session store with TTL

**Acceptance Criteria:**
- Given user logs in, when session created, then session stored in Redis with 24h TTL
- Given session expires, when user accesses protected resource, then 401 is returned

**Dependencies:** None

### TASK-004: Implement logout functionality
Clean session invalidation on logout

**Acceptance Criteria:**
- Given user logs out, when logout is confirmed, then session is removed from Redis
- Given session is invalidated, when user tries to use it, then 401 is returned

**Dependencies:** TASK-003
"""
    }


def get_backlog_bundle_needs_clarification_mock(project_id: UUID, message: str) -> Dict[str, Any]:
    """Mock GeneratedBacklogBundle with low score and clarification questions."""
    return {
        "prompt_id": "d4e5f6a7-b8c9-4012-d345-6789abcdef01",
        "project_id": str(project_id),
        "epics": [
            {
                "id": "EPIC-001",
                "title": "Authentication",
                "description": "User authentication",
                "tasks": [
                    {
                        "id": "TASK-001",
                        "title": "Add login",
                        "description": "Implement login",
                        "acceptance_criteria": ["User can login"],
                        "dependencies": [],
                        "similar": None
                    }
                ],
                "similar": None
            }
        ],
        "assumptions": ["Web application"],
        "risks": ["Unclear requirements"],
        "score": 0.58,
        "coverage_components": {
            "coverage": 0.50,
            "specificity": 0.45,
            "feasibility": 0.70,
            "duplication": 0.80
        },
        "clarification_questions": [
            {
                "id": "Q1",
                "text": "What session timeout duration is required?"
            },
            {
                "id": "Q2",
                "text": "Should the system support concurrent sessions from multiple devices?"
            }
        ],
        "markdown_text": "# Backlog\n\n## Epic: Authentication\n\n### TASK-001: Add login\nImplement login\n"
    }


# Progress messages for SSE simulation
REQUIREMENTS_PROGRESS_MESSAGES = [
    {"project_id": "PROJECT_ID", "status": "analyzing_prompt", "thought_summary": "Decomposing user requirements", "message_id": "m1", "timestamp": "2024-01-01T10:00:00Z"},
    {"project_id": "PROJECT_ID", "status": "retrieving_context", "thought_summary": "Fetching relevant context from knowledge base", "message_id": "m2", "timestamp": "2024-01-01T10:00:05Z"},
    {"project_id": "PROJECT_ID", "status": "drafting_requirements", "thought_summary": "Synthesizing business and functional requirements", "message_id": "m3", "timestamp": "2024-01-01T10:00:10Z"},
    {"project_id": "PROJECT_ID", "status": "evaluating", "score": 0.85, "thought_summary": "Evaluating requirement quality", "message_id": "m4", "timestamp": "2024-01-01T10:00:15Z"},
    {"project_id": "PROJECT_ID", "status": "completed", "score": 0.85, "thought_summary": "Requirements generation completed successfully", "message_id": "m5", "timestamp": "2024-01-01T10:00:20Z"}
]

TASKS_PROGRESS_MESSAGES = [
    {"project_id": "PROJECT_ID", "status": "analyzing_requirements", "thought_summary": "Analyzing requirements and extracting intents", "message_id": "t1", "timestamp": "2024-01-01T10:00:00Z"},
    {"project_id": "PROJECT_ID", "status": "retrieving_context", "thought_summary": "Retrieving technical context from GraphRAG", "message_id": "t2", "timestamp": "2024-01-01T10:00:05Z"},
    {"project_id": "PROJECT_ID", "status": "fetching_backlog", "thought_summary": "Fetching existing backlog from GitLab", "message_id": "t3", "timestamp": "2024-01-01T10:00:08Z"},
    {"project_id": "PROJECT_ID", "status": "drafting_backlog", "thought_summary": "Synthesizing epics and tasks with INVEST principles", "message_id": "t4", "timestamp": "2024-01-01T10:00:12Z"},
    {"project_id": "PROJECT_ID", "status": "mapping_duplicates", "thought_summary": "Computing similarity scores with existing work items", "message_id": "t5", "timestamp": "2024-01-01T10:00:18Z"},
    {"project_id": "PROJECT_ID", "status": "evaluating", "score": 0.82, "thought_summary": "Evaluating backlog quality", "message_id": "t6", "timestamp": "2024-01-01T10:00:22Z"},
    {"project_id": "PROJECT_ID", "status": "completed", "score": 0.82, "thought_summary": "Backlog generation completed successfully", "message_id": "t7", "timestamp": "2024-01-01T10:00:25Z"}
]

