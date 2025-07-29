"""
Tests for KeyManager integration with centralized configuration.
"""
import os
import tempfile
import pytest
from unittest.mock import patch
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.hazmat.primitives import serialization

from src.key_manager import KeyManager
from src.config import Settings


class TestKeyManagerConfigIntegration:
    """Test cases for KeyManager integration with Settings configuration."""

    def test_key_manager_uses_settings_for_private_key(self):
        """Test that KeyManager uses Settings for private key instead of direct os.getenv."""
        # Generate a test private key
        private_key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
        pem_data = private_key.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption()
        ).decode('utf-8')
        
        # Create custom settings with the private key
        test_settings = Settings(MOCK_AUTH_PRIVATE_KEY=pem_data)
        
        with patch('src.key_manager.settings', test_settings):
            key_manager = KeyManager()
            loaded_key = key_manager.get_private_key()
            
            # Verify the key is loaded correctly from settings
            assert loaded_key is not None
            assert isinstance(loaded_key, rsa.RSAPrivateKey)
            
            # Verify key properties match
            original_numbers = private_key.private_numbers()
            loaded_numbers = loaded_key.private_numbers()
            assert original_numbers.d == loaded_numbers.d

    def test_key_manager_uses_settings_for_key_id(self):
        """Test that KeyManager uses Settings for key ID instead of direct os.getenv."""
        test_key_id = "settings-test-key-id-123"
        
        # Create custom settings with the key ID
        test_settings = Settings(MOCK_AUTH_KEY_ID=test_key_id)
        
        with patch('src.key_manager.settings', test_settings):
            key_manager = KeyManager()
            loaded_key_id = key_manager.get_key_id()
            
            assert loaded_key_id == test_key_id

    def test_key_manager_fallback_when_settings_empty(self):
        """Test that KeyManager falls back to file/generation when settings are empty."""
        # Create settings with empty values (default behavior)
        test_settings = Settings(MOCK_AUTH_PRIVATE_KEY="", MOCK_AUTH_KEY_ID="")
        
        with tempfile.TemporaryDirectory() as temp_dir:
            key_file_path = os.path.join(temp_dir, 'private_key.pem')
            kid_file_path = os.path.join(temp_dir, 'key_id.txt')
            
            with patch('src.key_manager.settings', test_settings):
                with patch('src.key_manager.KeyManager._get_key_file_path', return_value=key_file_path):
                    with patch('src.key_manager.KeyManager._get_kid_file_path', return_value=kid_file_path):
                        key_manager = KeyManager()
                        
                        # Should generate new key and ID since settings are empty
                        generated_key = key_manager.get_private_key()
                        generated_key_id = key_manager.get_key_id()
                        
                        assert generated_key is not None
                        assert isinstance(generated_key, rsa.RSAPrivateKey)
                        assert generated_key_id is not None
                        assert len(generated_key_id) > 0
                        
                        # Verify files were created
                        assert os.path.exists(key_file_path)
                        assert os.path.exists(kid_file_path)

    def test_key_manager_prioritizes_settings_over_files(self):
        """Test that KeyManager prioritizes settings over file-based keys."""
        # Generate a test private key for settings
        settings_private_key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
        settings_pem_data = settings_private_key.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption()
        ).decode('utf-8')
        
        # Generate a different private key for file
        file_private_key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
        file_pem_data = file_private_key.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption()
        ).decode('utf-8')
        
        settings_key_id = "settings-key-id"
        file_key_id = "file-key-id"
        
        with tempfile.TemporaryDirectory() as temp_dir:
            key_file_path = os.path.join(temp_dir, 'private_key.pem')
            kid_file_path = os.path.join(temp_dir, 'key_id.txt')
            
            # Write file-based key and ID
            with open(key_file_path, 'w', encoding='utf-8') as f:
                f.write(file_pem_data)
            with open(kid_file_path, 'w', encoding='utf-8') as f:
                f.write(file_key_id)
            
            # Create settings with different key and ID
            test_settings = Settings(
                MOCK_AUTH_PRIVATE_KEY=settings_pem_data,
                MOCK_AUTH_KEY_ID=settings_key_id
            )
            
            with patch('src.key_manager.settings', test_settings):
                with patch('src.key_manager.KeyManager._get_key_file_path', return_value=key_file_path):
                    with patch('src.key_manager.KeyManager._get_kid_file_path', return_value=kid_file_path):
                        key_manager = KeyManager()
                        
                        # Should use settings values, not file values
                        loaded_key = key_manager.get_private_key()
                        loaded_key_id = key_manager.get_key_id()
                        
                        # Verify it used settings key, not file key
                        settings_numbers = settings_private_key.private_numbers()
                        loaded_numbers = loaded_key.private_numbers()
                        assert settings_numbers.d == loaded_numbers.d
                        
                        # Verify it used settings key ID, not file key ID
                        assert loaded_key_id == settings_key_id
                        assert loaded_key_id != file_key_id