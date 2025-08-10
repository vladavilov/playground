"""
Integration tests for the mock auth service with persistent key management.
"""
import os
import tempfile
import pytest
from fastapi.testclient import TestClient
from unittest.mock import patch
from jose import jwt
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.hazmat.primitives import serialization

from main import app
from config import settings


class TestMockAuthServiceIntegration:
    """Integration tests for the mock auth service."""

    def setup_method(self):
        """Set up test client."""
        self.client = TestClient(app)
        self.tenant_id = settings.AZURE_AD_TENANT_ID

    def test_jwks_endpoint_returns_consistent_key(self):
        """Test that JWKS endpoint returns consistent key across multiple calls."""
        # Make first request
        response1 = self.client.get(f"/{self.tenant_id}/discovery/v2.0/keys")
        assert response1.status_code == 200
        jwks1 = response1.json()
        
        # Make second request
        response2 = self.client.get(f"/{self.tenant_id}/discovery/v2.0/keys")
        assert response2.status_code == 200
        jwks2 = response2.json()
        
        # Verify keys are identical
        assert jwks1 == jwks2
        assert len(jwks1["keys"]) == 1
        
        key = jwks1["keys"][0]
        assert key["kty"] == "RSA"
        assert key["use"] == "sig"
        assert "kid" in key
        assert "n" in key
        assert "e" in key

    def test_token_endpoint_uses_consistent_key_id(self):
        """Test that token endpoint uses consistent key ID in JWT headers."""
        # Get JWKS to know the expected key ID
        jwks_response = self.client.get(f"/{self.tenant_id}/discovery/v2.0/keys")
        assert jwks_response.status_code == 200
        expected_kid = jwks_response.json()["keys"][0]["kid"]
        
        # Get token
        token_response = self.client.post(
            f"/{self.tenant_id}/oauth2/v2.0/token",
            data={"grant_type": "client_credentials"}
        )
        assert token_response.status_code == 200
        token_data = token_response.json()
        
        # Decode token header without verification to check kid
        token = token_data["access_token"]
        header = jwt.get_unverified_header(token)
        
        assert header["kid"] == expected_kid

    def test_token_can_be_verified_with_jwks_key(self):
        """Test that tokens can be verified using the public key from JWKS."""
        # Get JWKS
        jwks_response = self.client.get(f"/{self.tenant_id}/discovery/v2.0/keys")
        assert jwks_response.status_code == 200
        jwk = jwks_response.json()["keys"][0]
        
        # Get token
        token_response = self.client.post(
            f"/{self.tenant_id}/oauth2/v2.0/token",
            data={"grant_type": "client_credentials"}
        )
        assert token_response.status_code == 200
        token = token_response.json()["access_token"]
        
        # Reconstruct public key from JWK
        from jose.utils import base64url_decode
        import struct
        
        n = int.from_bytes(base64url_decode(jwk["n"].encode('utf-8')), 'big')
        e = int.from_bytes(base64url_decode(jwk["e"].encode('utf-8')), 'big')
        
        # Create RSA public key from components
        from cryptography.hazmat.primitives.asymmetric.rsa import RSAPublicNumbers
        public_numbers = RSAPublicNumbers(e, n)
        public_key = public_numbers.public_key()
        
        # Verify token signature
        try:
            decoded_token = jwt.decode(
                token,
                public_key,
                algorithms=["RS256"],
                audience=settings.AZURE_AD_CLIENT_ID,
                issuer=f"{str(settings.AZURE_AD_AUTHORITY).rstrip('/')}/{self.tenant_id}/v2.0"
            )
            # If we get here, verification succeeded
            assert "aud" in decoded_token
            assert "iss" in decoded_token
            assert "exp" in decoded_token
        except jwt.JWTError as e:
            pytest.fail(f"Token verification failed: {str(e)}")

    def test_openid_configuration_endpoint(self):
        """Test that OpenID configuration endpoint works correctly."""
        response = self.client.get(f"/{self.tenant_id}/v2.0/.well-known/openid-configuration")
        assert response.status_code == 200
        
        config = response.json()
        assert config["issuer"] == f"{str(settings.AZURE_AD_AUTHORITY).rstrip('/')}/{self.tenant_id}/v2.0"
        assert config["jwks_uri"] == f"{str(settings.AZURE_AD_AUTHORITY).rstrip('/')}/{self.tenant_id}/discovery/v2.0/keys"
        assert config["token_endpoint"] == f"{str(settings.AZURE_AD_AUTHORITY).rstrip('/')}/{self.tenant_id}/oauth2/v2.0/token"

    def test_persistent_keys_across_service_restarts(self):
        """Test that keys persist across service restarts (simulated by creating new KeyManager)."""
        with tempfile.TemporaryDirectory() as temp_dir:
            key_file_path = os.path.join(temp_dir, 'private_key.pem')
            kid_file_path = os.path.join(temp_dir, 'key_id.txt')
            
            with patch.dict(os.environ, {}, clear=True):
                with patch('key_manager.KeyManager._get_key_file_path', return_value=key_file_path):
                    with patch('key_manager.KeyManager._get_kid_file_path', return_value=kid_file_path):
                        # Create first KeyManager instance
                        from key_manager import KeyManager
                        first_key_manager = KeyManager()
                        first_jwk = first_key_manager.get_jwk()
                        
                        # Simulate service restart by creating new KeyManager instance
                        # with the same file paths
                        second_key_manager = KeyManager()
                        second_jwk = second_key_manager.get_jwk()
                        
                        # Verify keys are the same
                        assert first_jwk["kid"] == second_jwk["kid"]
                        assert first_jwk["n"] == second_jwk["n"]
                        assert first_jwk["e"] == second_jwk["e"]

    def test_invalid_tenant_id_returns_404(self):
        """Test that invalid tenant ID returns 404."""
        invalid_tenant = "invalid-tenant-id"
        
        # Test JWKS endpoint
        response = self.client.get(f"/{invalid_tenant}/discovery/v2.0/keys")
        assert response.status_code == 404
        
        # Test token endpoint
        response = self.client.post(f"/{invalid_tenant}/oauth2/v2.0/token")
        assert response.status_code == 404
        
        # Test OpenID configuration endpoint
        response = self.client.get(f"/{invalid_tenant}/v2.0/.well-known/openid-configuration")
        assert response.status_code == 404