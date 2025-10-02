# E2E Tests Authentication Fix Summary

## Problem Identified

The e2e tests were failing with "Remote end closed connection without response" because they were trying to use **Azure AD tokens** from the mock auth service to authenticate with backend services. However, backend services don't validate Azure AD tokens - they validate **LOCAL JWT tokens** signed with `LOCAL_JWT_SECRET`.

## Architecture Understanding

### Two Authentication Mechanisms:

1. **UI Service SSO (Azure AD/MSAL)**:
   - Users log in via Azure AD
   - UI service creates session in Redis
   - UI service mints S2S JWT tokens signed with `LOCAL_JWT_SECRET`
   - These tokens are forwarded to backend services

2. **Backend Services (LOCAL JWT)**:
   - Validate tokens using `LOCAL_JWT_SECRET` (shared secret)
   - Use `get_local_user_verified` dependency from `utils.local_auth`
   - Expect Bearer tokens with claims: `oid`, `preferred_username`, `roles`

### Services Authentication Pattern:

- **project_management_service**: Requires LOCAL JWT
- **ai_workflow_service**: Requires LOCAL JWT  
- **neo4j_retrieval_service**: Requires LOCAL JWT
- **neo4j_maintenance_service**: Requires LOCAL JWT
- **db_init_service**: Requires LOCAL JWT
- **UI service**: Requires Azure SSO for browser access, mints LOCAL JWT for backend calls

## Solution Implemented

### 1. Created `_create_local_jwt_token()` Helper Function

Location: `e2e_tests/src/conftest.py`

```python
def _create_local_jwt_token(oid: str = None, roles: list = None, username: str = None) -> str:
    """
    Create LOCAL JWT token for backend service authentication.
    
    Backend services validate tokens using LOCAL_JWT_SECRET (shared secret),
    NOT Azure AD tokens. This mimics what UI service does when minting S2S tokens.
    """
    secret = os.getenv("LOCAL_JWT_SECRET", "test-secret-key-for-e2e")
    now = int(time.time())
    
    claims = {
        "oid": oid or str(uuid.uuid4()),
        "preferred_username": username or "test.user@example.com",
        "roles": roles or ["Admin", "User"],
        "iss": "e2e-tests",
        "aud": "backend-services",
        "iat": now,
        "nbf": now,
        "exp": now + 3600,  # 1 hour validity
    }
    
    return jwt.encode(claims, secret, algorithm="HS256")
```

### 2. Updated `auth_headers` Fixture

**Before** (WRONG - tried to use Azure AD):
```python
@pytest.fixture
def auth_headers(service_urls, auth_config):
    # Called mock auth service for Azure AD tokens
    token_response = requests.post(...)
    return {"Authorization": f"Bearer {token_data['access_token']}"}
```

**After** (CORRECT - uses LOCAL JWT):
```python
@pytest.fixture
def auth_headers() -> Dict[str, str]:
    """Get authentication headers with LOCAL JWT token for backend services."""
    token = _create_local_jwt_token(roles=["Admin", "User"])
    return {"Authorization": f"Bearer {token}"}
```

### 3. Updated `postgres_initialized` Fixture

Now uses LOCAL JWT token instead of trying to get Azure AD token from mock auth service.

### 4. Updated `ensure_clean_session_setup` Fixture

Uses the updated `auth_headers` fixture which now provides LOCAL JWT tokens.

## Files Modified

1. **`e2e_tests/src/conftest.py`**:
   - Added imports: `time`, `os`, `jwt` from `jose`
   - Added `_create_local_jwt_token()` helper function
   - Simplified `auth_headers` fixture to create LOCAL JWT tokens
   - Updated `postgres_initialized` to use LOCAL JWT tokens
   - Fixture `ensure_clean_session_setup` already properly uses `auth_headers`

2. **`e2e_tests/src/test_neo4j_drift_search.py`**:
   - Added `auth_headers` parameter to `test_retrieval_service` function
   - Passed `headers=auth_headers` to `HTTPUtils.make_request_with_retry()`

## Test Execution Flow

### Direct Backend Testing (Most Tests):
```
Test → auth_headers fixture → _create_local_jwt_token() → LOCAL JWT → Backend Service
```

### UI Service Testing (test_ui_workflow.py):
```
Browser/Test → UI Service (SSO session) → UI mints S2S token → Backend Service
```

## Environment Variables Required

- **`LOCAL_JWT_SECRET`**: Shared secret for JWT signing/validation
  - Must be consistent across all services
  - Defaults to `"test-secret-key-for-e2e"` in tests if not set
  - In production/docker-compose, should be set in `docker-compose.env`

## Verification

To verify the fix works:

1. Ensure `LOCAL_JWT_SECRET` is set consistently across services
2. Run e2e tests: `pytest e2e_tests/src/`
3. Tests should now properly authenticate with backend services

## Key Takeaway

**E2E tests must use LOCAL JWT tokens (not Azure AD tokens) when calling backend services directly.**

Only UI service tests that go through the browser flow need Azure SSO authentication.

