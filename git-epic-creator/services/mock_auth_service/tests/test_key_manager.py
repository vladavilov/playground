"""
Tests for the key management functionality of the mock auth service.
"""
import os
import tempfile
import pytest
from unittest.mock import patch
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.hazmat.primitives import serialization

from key_manager import KeyManager


class TestKeyManager:
    """Test cases for KeyManager class."""

    def test_load_key_from_environment_variable(self):
        """Test loading RSA private key from environment variable."""
        # Generate a test private key
        private_key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
        pem_data = private_key.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption()
        ).decode('utf-8')
        
        with patch.dict(os.environ, {'MOCK_AUTH_PRIVATE_KEY': pem_data}):
            # Need to patch the settings to reload with new environment
            from config import Settings
            test_settings = Settings()
            with patch('key_manager.settings', test_settings):
                key_manager = KeyManager()
                loaded_key = key_manager.get_private_key()
                
                # Verify the key is loaded correctly
                assert loaded_key is not None
                assert isinstance(loaded_key, rsa.RSAPrivateKey)
                
                # Verify key properties match
                original_numbers = private_key.private_numbers()
                loaded_numbers = loaded_key.private_numbers()
                assert original_numbers.d == loaded_numbers.d

    def test_load_key_from_file(self):
        """Test loading RSA private key from file when env var is not set."""
        # Generate a test private key
        private_key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
        pem_data = private_key.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption()
        ).decode('utf-8')
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.pem') as temp_file:
            temp_file.write(pem_data)
            temp_file_path = temp_file.name
        
        try:
            with patch.dict(os.environ, {}, clear=True):
                with patch('key_manager.KeyManager._get_key_file_path', return_value=temp_file_path):
                    key_manager = KeyManager()
                    loaded_key = key_manager.get_private_key()
                    
                    # Verify the key is loaded correctly
                    assert loaded_key is not None
                    assert isinstance(loaded_key, rsa.RSAPrivateKey)
        finally:
            os.unlink(temp_file_path)

    def test_generate_and_save_key_when_none_exists(self):
        """Test key generation and saving when no key exists."""
        with tempfile.TemporaryDirectory() as temp_dir:
            key_file_path = os.path.join(temp_dir, 'private_key.pem')
            
            with patch.dict(os.environ, {}, clear=True):
                with patch('key_manager.KeyManager._get_key_file_path', return_value=key_file_path):
                    key_manager = KeyManager()
                    generated_key = key_manager.get_private_key()
                    
                    # Verify key was generated
                    assert generated_key is not None
                    assert isinstance(generated_key, rsa.RSAPrivateKey)
                    
                    # Verify key was saved to file
                    assert os.path.exists(key_file_path)
                    
                    # Verify saved key can be loaded
                    with open(key_file_path, 'r') as f:
                        saved_pem = f.read()
                    
                    loaded_key = serialization.load_pem_private_key(
                        saved_pem.encode('utf-8'),
                        password=None
                    )
                    
                    # Verify keys match
                    original_numbers = generated_key.private_numbers()
                    loaded_numbers = loaded_key.private_numbers()
                    assert original_numbers.d == loaded_numbers.d

    def test_consistent_key_id_across_restarts(self):
        """Test that key ID remains consistent across service restarts."""
        with tempfile.TemporaryDirectory() as temp_dir:
            key_file_path = os.path.join(temp_dir, 'private_key.pem')
            kid_file_path = os.path.join(temp_dir, 'key_id.txt')
            
            with patch.dict(os.environ, {}, clear=True):
                with patch('key_manager.KeyManager._get_key_file_path', return_value=key_file_path):
                    with patch('key_manager.KeyManager._get_kid_file_path', return_value=kid_file_path):
                        # First instance - should generate key and ID
                        key_manager1 = KeyManager()
                        key_id1 = key_manager1.get_key_id()
                        
                        # Second instance - should load same key and ID
                        key_manager2 = KeyManager()
                        key_id2 = key_manager2.get_key_id()
                        
                        # Verify key IDs are consistent
                        assert key_id1 == key_id2
                        assert key_id1 is not None
                        assert len(key_id1) > 0

    def test_load_key_id_from_environment(self):
        """Test loading key ID from environment variable."""
        test_key_id = "test-key-id-123"
        
        with patch.dict(os.environ, {'MOCK_AUTH_KEY_ID': test_key_id}):
            # Need to patch the settings to reload with new environment
            from config import Settings
            test_settings = Settings()
            with patch('key_manager.settings', test_settings):
                key_manager = KeyManager()
                loaded_key_id = key_manager.get_key_id()
                
                assert loaded_key_id == test_key_id

    def test_invalid_private_key_in_environment(self):
        """Test handling of invalid private key in environment variable."""
        invalid_pem = "-----BEGIN PRIVATE KEY-----\nINVALID_KEY_DATA\n-----END PRIVATE KEY-----"
        
        with patch.dict(os.environ, {'MOCK_AUTH_PRIVATE_KEY': invalid_pem}):
            # Need to patch the settings to reload with new environment
            from config import Settings
            test_settings = Settings()
            with patch('key_manager.settings', test_settings):
                with pytest.raises(ValueError, match="Invalid private key"):
                    KeyManager()

    def test_invalid_private_key_in_file(self):
        """Test handling of invalid private key in file."""
        invalid_pem = "-----BEGIN PRIVATE KEY-----\nINVALID_KEY_DATA\n-----END PRIVATE KEY-----"
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.pem') as temp_file:
            temp_file.write(invalid_pem)
            temp_file_path = temp_file.name
        
        try:
            with patch.dict(os.environ, {}, clear=True):
                with patch('key_manager.KeyManager._get_key_file_path', return_value=temp_file_path):
                    with pytest.raises(ValueError, match="Invalid private key"):
                        KeyManager()
        finally:
            os.unlink(temp_file_path)

    def test_get_public_key(self):
        """Test getting public key from private key."""
        key_manager = KeyManager()
        private_key = key_manager.get_private_key()
        public_key = key_manager.get_public_key()
        
        # Verify public key is derived from private key
        assert public_key is not None
        expected_public_key = private_key.public_key()
        
        # Compare public key numbers
        expected_numbers = expected_public_key.public_numbers()
        actual_numbers = public_key.public_numbers()
        
        assert expected_numbers.n == actual_numbers.n
        assert expected_numbers.e == actual_numbers.e

    def test_get_jwk(self):
        """Test getting JWK (JSON Web Key) representation."""
        key_manager = KeyManager()
        jwk = key_manager.get_jwk()
        
        # Verify JWK structure
        assert isinstance(jwk, dict)
        assert jwk['kty'] == 'RSA'
        assert jwk['use'] == 'sig'
        assert 'kid' in jwk
        assert 'n' in jwk  # RSA modulus
        assert 'e' in jwk  # RSA exponent
        
        # Verify key ID matches
        assert jwk['kid'] == key_manager.get_key_id()