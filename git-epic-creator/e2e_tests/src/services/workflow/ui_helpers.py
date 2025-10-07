"""
UI service authentication helpers for e2e tests.

This module provides UI service authentication operations following
Single Responsibility Principle - focused on SSO and OAuth flows.
"""

from __future__ import annotations

import requests

from config import TestConstants


class UIHelpers:
    """
    UI service authentication and OAuth operations.
    
    Provides operations for:
    - SSO login simulation
    - GitLab OAuth connection simulation
    - Session management
    """
    
    @staticmethod
    def simulate_sso_login(ui_base: str) -> requests.Session:
        """
        Simulate browser SSO login flow to get authenticated session.
        
        Flow:
        1. GET /auth/login - UI service redirects to mock auth /authorize
        2. Mock auth immediately redirects back with code
        3. GET /auth/callback?code=... - UI service exchanges code for tokens, creates session
        4. Session cookie is now set and can be used for API calls
        
        Args:
            ui_base: Base URL of UI service
            
        Returns:
            Authenticated requests.Session with cookies
            
        Raises:
            AssertionError: If SSO login fails or session is not authenticated
        """
        session = requests.Session()
        session.verify = False  # For self-signed certs
        
        # Initiate login - follow redirects automatically
        # This will go: /auth/login -> mock_auth /authorize -> /auth/callback
        response = session.get(
            f"{ui_base}/auth/login",
            allow_redirects=True,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        
        # Should end up at home page (/) after successful auth
        assert response.status_code == TestConstants.HTTP_OK, (
            f"SSO login failed: {response.status_code} {response.text}"
        )
        
        # Verify session is authenticated
        me_response = session.get(
            f"{ui_base}/auth/me",
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        assert me_response.status_code == TestConstants.HTTP_OK
        me_data = me_response.json()
        assert me_data.get("authenticated") is True, (
            f"Session not authenticated: {me_data}"
        )
        
        return session
    
    @staticmethod
    def simulate_gitlab_oauth_connection(
        session: requests.Session,
        ui_base: str
    ) -> None:
        """
        Simulate GitLab OAuth connection flow programmatically.
        
        This mimics what a user would do in the browser:
        1. GET /auth/gitlab/authorize - initiates OAuth, redirects to GitLab
        2. GitLab mock immediately redirects back with code
        3. GET /auth/gitlab/callback - exchanges code for token, stores in Redis
        
        Args:
            session: Authenticated requests session (from SSO login)
            ui_base: Base URL of UI service
            
        Raises:
            AssertionError: If OAuth flow fails or GitLab is not connected
        """
        # Initiate GitLab OAuth flow
        # The UI service will redirect to gitlab-mock /oauth/authorize
        # which will immediately redirect back with an auth code
        response = session.get(
            f"{ui_base}/auth/gitlab/authorize",
            params={"redirect_uri": "/projects.html"},
            allow_redirects=True,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        
        assert response.status_code == TestConstants.HTTP_OK, (
            f"GitLab OAuth connection failed: {response.status_code} {response.text}"
        )
        
        # Verify GitLab connection status
        status_response = session.get(
            f"{ui_base}/auth/gitlab/status",
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        assert status_response.status_code == TestConstants.HTTP_OK
        status_data = status_response.json()
        assert status_data.get("connected") is True, (
            f"GitLab not connected after OAuth flow: {status_data}"
        )

