"""
Key management module for the mock auth service.
Handles loading, generating, and persisting RSA keys for JWT signing.
"""
import os
import base64
import uuid
from pathlib import Path
from typing import Optional

from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.hazmat.primitives import serialization

from config import settings


class KeyManager:
    """Manages RSA keys for JWT signing with persistence across service restarts."""
    
    def __init__(self):
        """Initialize the key manager and load or generate keys."""
        self._private_key: Optional[rsa.RSAPrivateKey] = None
        self._key_id: Optional[str] = None
        self._load_or_generate_keys()
    
    def _load_or_generate_keys(self) -> None:
        """Load keys from environment/file or generate new ones if none exist."""
        try:
            # Try to load private key from environment variable first
            if self._load_key_from_environment():
                return
            
            # Try to load private key from file
            if self._load_key_from_file():
                return
            
            # Generate new keys if none exist
            self._generate_and_save_keys()
            
        except Exception as e:
            raise ValueError(f"Failed to initialize keys: {str(e)}")
    
    def _load_key_from_environment(self) -> bool:
        """Load private key from environment variable."""
        pem_data = settings.MOCK_AUTH_PRIVATE_KEY
        if not pem_data:
            return False
        
        try:
            self._private_key = serialization.load_pem_private_key(
                pem_data.encode('utf-8'),
                password=None
            )
            if not isinstance(self._private_key, rsa.RSAPrivateKey):
                raise ValueError("Key is not an RSA private key")
            return True
        except Exception as e:
            raise ValueError(f"Invalid private key in environment variable: {str(e)}")
    
    def _load_key_from_file(self) -> bool:
        """Load private key from file."""
        key_file_path = self._get_key_file_path()
        if not os.path.exists(key_file_path):
            return False
        
        try:
            with open(key_file_path, 'r', encoding='utf-8') as f:
                pem_data = f.read()
            
            self._private_key = serialization.load_pem_private_key(
                pem_data.encode('utf-8'),
                password=None
            )
            if not isinstance(self._private_key, rsa.RSAPrivateKey):
                raise ValueError("Key is not an RSA private key")
            return True
        except Exception as e:
            raise ValueError(f"Invalid private key in file {key_file_path}: {str(e)}")
    
    def _generate_and_save_keys(self) -> None:
        """Generate new RSA keys and save them to file."""
        # Generate new private key
        self._private_key = rsa.generate_private_key(
            public_exponent=65537,
            key_size=2048
        )
        
        # Save private key to file
        key_file_path = self._get_key_file_path()
        os.makedirs(os.path.dirname(key_file_path), exist_ok=True)
        
        pem_data = self._private_key.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption()
        )
        
        with open(key_file_path, 'wb') as f:
            f.write(pem_data)
    
    def _get_key_file_path(self) -> str:
        """Get the path where the private key file should be stored."""
        # Use a persistent location in the service directory
        service_dir = Path(__file__).parent.parent
        keys_dir = service_dir / 'keys'
        return str(keys_dir / 'private_key.pem')
    
    def _get_kid_file_path(self) -> str:
        """Get the path where the key ID file should be stored."""
        service_dir = Path(__file__).parent.parent
        keys_dir = service_dir / 'keys'
        return str(keys_dir / 'key_id.txt')
    
    def get_private_key(self) -> rsa.RSAPrivateKey:
        """Get the RSA private key."""
        if self._private_key is None:
            raise RuntimeError("Private key not initialized")
        return self._private_key
    
    def get_public_key(self) -> rsa.RSAPublicKey:
        """Get the RSA public key derived from the private key."""
        return self.get_private_key().public_key()
    
    def get_key_id(self) -> str:
        """Get the key ID, loading from environment/file or generating if needed."""
        if self._key_id is not None:
            return self._key_id
        
        # Try to load key ID from environment variable
        env_key_id = settings.MOCK_AUTH_KEY_ID
        if env_key_id:
            self._key_id = env_key_id
            return self._key_id
        
        # Try to load key ID from file
        kid_file_path = self._get_kid_file_path()
        if os.path.exists(kid_file_path):
            with open(kid_file_path, 'r', encoding='utf-8') as f:
                self._key_id = f.read().strip()
                return self._key_id
        
        # Generate new key ID and save it
        self._key_id = f"mock-auth-{str(uuid.uuid4())[:8]}"
        os.makedirs(os.path.dirname(kid_file_path), exist_ok=True)
        with open(kid_file_path, 'w', encoding='utf-8') as f:
            f.write(self._key_id)
        
        return self._key_id
    
    def get_jwk(self) -> dict:
        """Get the JSON Web Key (JWK) representation of the public key."""
        public_key = self.get_public_key()
        public_numbers = public_key.public_numbers()
        
        # Convert RSA modulus and exponent to base64url encoding
        n = self._base64url_encode(
            public_numbers.n.to_bytes((public_numbers.n.bit_length() + 7) // 8, 'big')
        )
        e = self._base64url_encode(
            public_numbers.e.to_bytes((public_numbers.e.bit_length() + 7) // 8, 'big')
        )
        
        return {
            "kty": "RSA",
            "use": "sig",
            "kid": self.get_key_id(),
            "n": n,
            "e": e
        }
    
    def _base64url_encode(self, data: bytes) -> str:
        """Helper function to encode bytes to base64url."""
        return base64.urlsafe_b64encode(data).rstrip(b'=').decode('utf-8')