#!/usr/bin/env python3
"""
Generate self-signed SSL certificates for mock auth service.
This script creates certificates valid for local development and testing.
"""
import os
import sys
import ipaddress
from datetime import datetime, timedelta, timezone
from cryptography import x509
from cryptography.x509.oid import NameOID, ExtensionOID
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import rsa


def generate_self_signed_cert(
    cert_path: str = "cert.pem",
    key_path: str = "key.pem",
    validity_days: int = 365
):
    """
    Generate a self-signed certificate and private key for local development.
    
    Args:
        cert_path: Path to save the certificate
        key_path: Path to save the private key
        validity_days: Number of days the certificate is valid
    """
    # Generate private key
    private_key = rsa.generate_private_key(
        public_exponent=65537,
        key_size=2048,
    )
    
    # Certificate subject and issuer (same for self-signed)
    subject = issuer = x509.Name([
        x509.NameAttribute(NameOID.COUNTRY_NAME, "US"),
        x509.NameAttribute(NameOID.STATE_OR_PROVINCE_NAME, "Development"),
        x509.NameAttribute(NameOID.LOCALITY_NAME, "Local"),
        x509.NameAttribute(NameOID.ORGANIZATION_NAME, "Mock Auth Service"),
        x509.NameAttribute(NameOID.COMMON_NAME, "localhost"),
    ])
    
    # Create certificate
    cert = (
        x509.CertificateBuilder()
        .subject_name(subject)
        .issuer_name(issuer)
        .public_key(private_key.public_key())
        .serial_number(x509.random_serial_number())
        .not_valid_before(datetime.now(timezone.utc))
        .not_valid_after(datetime.now(timezone.utc) + timedelta(days=validity_days))
        .add_extension(
            x509.SubjectAlternativeName([
                x509.DNSName("localhost"),
                x509.DNSName("mock-auth-service"),
                x509.DNSName("host.docker.internal"),
                x509.IPAddress(ipaddress.IPv4Address("127.0.0.1")),
            ]),
            critical=False,
        )
        .add_extension(
            x509.BasicConstraints(ca=False, path_length=None),
            critical=True,
        )
        .add_extension(
            x509.KeyUsage(
                digital_signature=True,
                key_encipherment=True,
                content_commitment=False,
                data_encipherment=False,
                key_agreement=False,
                key_cert_sign=False,
                crl_sign=False,
                encipher_only=False,
                decipher_only=False,
            ),
            critical=True,
        )
        .add_extension(
            x509.ExtendedKeyUsage([
                x509.oid.ExtendedKeyUsageOID.SERVER_AUTH,
            ]),
            critical=True,
        )
        .sign(private_key, hashes.SHA256())
    )
    
    # Write private key
    with open(key_path, "wb") as f:
        f.write(
            private_key.private_bytes(
                encoding=serialization.Encoding.PEM,
                format=serialization.PrivateFormat.PKCS8,
                encryption_algorithm=serialization.NoEncryption(),
            )
        )
    
    # Write certificate
    with open(cert_path, "wb") as f:
        f.write(cert.public_bytes(serialization.Encoding.PEM))
    
    print(f"✓ Generated self-signed certificate: {cert_path}")
    print(f"✓ Generated private key: {key_path}")
    print(f"✓ Valid for {validity_days} days")
    print(f"✓ Subject Alternative Names: localhost, mock-auth-service, host.docker.internal, 127.0.0.1")


if __name__ == "__main__":
    # Get paths from arguments or use defaults
    cert_dir = os.path.dirname(os.path.abspath(__file__))
    cert_path = os.path.join(cert_dir, "cert.pem")
    key_path = os.path.join(cert_dir, "key.pem")
    
    if len(sys.argv) > 1:
        cert_path = sys.argv[1]
    if len(sys.argv) > 2:
        key_path = sys.argv[2]
    
    generate_self_signed_cert(cert_path, key_path)
