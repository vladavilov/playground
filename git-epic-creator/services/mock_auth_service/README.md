# Mock Azure AD Authentication Service

A FastAPI-based mock authentication service that emulates Azure AD OIDC endpoints for local development and testing. This service provides JWT token generation with persistent RSA key management and supports all standard OIDC discovery endpoints.

## Features

- **OIDC Discovery Endpoint**: Provides standard OpenID Connect configuration
- **JWT Token Generation**: Issues signed JWT tokens with configurable claims
- **JWKS Endpoint**: Serves public keys for token verification
- **Persistent Key Management**: Maintains RSA keys across service restarts
- **Flexible Key Configuration**: Supports environment variables and file-based key storage
- **Docker Support**: Ready for containerized deployment

## Configuration

### Environment Variables

The service can be configured using the following environment variables:

#### Core Service Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `AZURE_AD_TENANT_ID` | `e7963c3a-3b3a-43b6-9426-89e433d07e69` | Azure AD tenant ID used in token claims and endpoints |
| `AZURE_AD_CLIENT_ID` | `a9e304a9-5b6c-4ef7-9b37-23a579a6d7be` | Client ID (audience) for issued tokens |
| `AZURE_AD_AUTHORITY` | `http://mock-auth-service:8005` | Base URL for the mock service endpoints |
| `API_PORT` | `8000` | Port on which the service runs |

#### Key Management Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `MOCK_AUTH_PRIVATE_KEY` | `""` | RSA private key in PEM format for JWT signing |
| `MOCK_AUTH_KEY_ID` | `""` | Key ID (kid) for JWT header |

### Configuration Examples

#### Basic Configuration (.env file)
```bash
# Core service settings
AZURE_AD_TENANT_ID=your-tenant-id
AZURE_AD_CLIENT_ID=your-client-id
AZURE_AD_AUTHORITY=http://localhost:8005

# Optional: Custom port
API_PORT=8005
```

#### Advanced Configuration with Custom Keys
```bash
# Use custom RSA private key
MOCK_AUTH_PRIVATE_KEY="-----BEGIN PRIVATE KEY-----
MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC...
-----END PRIVATE KEY-----"

# Use custom key ID
MOCK_AUTH_KEY_ID=my-custom-key-2024
```

## Key Management

### How Keys Work

The service uses a hierarchical approach to load RSA keys for JWT signing:

1. **Environment Variable**: If `MOCK_AUTH_PRIVATE_KEY` is set, uses that key
2. **File Storage**: Looks for `keys/private_key.pem` in the service directory
3. **Auto-Generation**: Generates new 2048-bit RSA keys if none exist

Key IDs follow the same pattern:
1. **Environment Variable**: Uses `MOCK_AUTH_KEY_ID` if set
2. **File Storage**: Loads from `keys/key_id.txt`
3. **Auto-Generation**: Creates UUID-based key ID (format: `mock-auth-{8-char-uuid}`)

### Key Generation Commands

#### Generate RSA Private Key
```bash
# Generate 2048-bit RSA private key
openssl genrsa -out private_key.pem 2048

# Extract public key (for verification)
openssl rsa -in private_key.pem -pubout -out public_key.pem

# View key details
openssl rsa -in private_key.pem -text -noout
```

#### Convert Key for Environment Variable
```bash
# Convert multi-line PEM to single line for environment variable
awk 'NF {sub(/\r/, ""); printf "%s\\n",$0;}' private_key.pem
```

#### Generate Key ID
```bash
# Generate a custom key ID
echo "my-service-$(date +%Y%m%d)" > key_id.txt

# Or use UUID-based approach
echo "mock-auth-$(uuidgen | cut -c1-8)" > key_id.txt
```

### Key Persistence

Keys are automatically persisted in the `keys/` directory:
- `keys/private_key.pem`: RSA private key in PEM format
- `keys/key_id.txt`: Key ID string

This ensures consistent token signing across service restarts.

## Usage

### Local Development

#### Using Python directly
```bash
# Install dependencies
pip install -e .

# Run the service
python -m src.main

# Or with custom port
API_PORT=8005 python -m src.main
```

#### Using Docker
```bash
# Build the image
docker build -t mock-auth-service .

# Run with default configuration
docker run -p 8005:8000 mock-auth-service

# Run with custom environment variables
docker run -p 8005:8000 \
  -e AZURE_AD_TENANT_ID=your-tenant-id \
  -e AZURE_AD_CLIENT_ID=your-client-id \
  mock-auth-service
```

#### Using Docker Compose
```yaml
version: '3.8'
services:
  mock-auth-service:
    build: ./services/mock_auth_service
    ports:
      - "8005:8000"
    environment:
      - AZURE_AD_TENANT_ID=your-tenant-id
      - AZURE_AD_CLIENT_ID=your-client-id
      - AZURE_AD_AUTHORITY=http://mock-auth-service:8000
    volumes:
      - ./keys:/app/keys  # Persist keys across container restarts
```

### API Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/health` | GET | Health check endpoint |
| `/{tenant_id}/v2.0/.well-known/openid-configuration` | GET | OIDC discovery document |
| `/{tenant_id}/discovery/v2.0/keys` | GET | JWKS endpoint (public keys) |
| `/{tenant_id}/oauth2/v2.0/token` | POST | Token endpoint (issues JWT) |
| `/{tenant_id}/oauth2/v2.0/authorize` | GET | Authorization endpoint (mock) |

### Example Token Request
```bash
curl -X POST "http://localhost:8005/e7963c3a-3b3a-43b6-9426-89e433d07e69/oauth2/v2.0/token" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "grant_type=client_credentials&client_id=your-client-id&client_secret=your-secret"
```

## Troubleshooting

### Common Key Issues

#### Issue: "Invalid private key in environment variable"
**Symptoms**: Service fails to start with key parsing error
**Causes**:
- Malformed PEM format in `MOCK_AUTH_PRIVATE_KEY`
- Missing BEGIN/END markers
- Incorrect line endings or encoding

**Solutions**:
```bash
# Verify key format
openssl rsa -in your_key.pem -check

# Regenerate key if corrupted
openssl genrsa -out new_private_key.pem 2048

# Ensure proper PEM format with correct headers
cat your_key.pem | head -1  # Should show "-----BEGIN PRIVATE KEY-----"
cat your_key.pem | tail -1  # Should show "-----END PRIVATE KEY-----"
```

#### Issue: "Failed to initialize keys"
**Symptoms**: Service startup failure with key initialization error
**Causes**:
- Permission issues with `keys/` directory
- Corrupted key files
- Invalid key format in files

**Solutions**:
```bash
# Check directory permissions
ls -la keys/
chmod 755 keys/
chmod 644 keys/*.pem keys/*.txt

# Remove corrupted keys (service will regenerate)
rm -rf keys/
mkdir keys

# Verify file contents
file keys/private_key.pem  # Should show "PEM RSA private key"
```

#### Issue: "Key is not an RSA private key"
**Symptoms**: Key loading fails with type validation error
**Causes**:
- Using EC (Elliptic Curve) keys instead of RSA
- Using public key instead of private key
- Using encrypted private key

**Solutions**:
```bash
# Check key type
openssl rsa -in keys/private_key.pem -text -noout | head -1

# Generate correct RSA private key
openssl genrsa -out keys/private_key.pem 2048

# Convert encrypted key to unencrypted (if needed)
openssl rsa -in encrypted_key.pem -out keys/private_key.pem
```

### JWT Token Issues

#### Issue: Token verification fails in client applications
**Symptoms**: "Invalid signature" or "Key not found" errors
**Causes**:
- Key ID mismatch between token header and JWKS
- Client using wrong JWKS endpoint
- Key rotation without client refresh

**Solutions**:
```bash
# Verify JWKS endpoint returns correct key
curl http://localhost:8005/{tenant-id}/discovery/v2.0/keys

# Check token header for key ID
echo "your.jwt.token" | cut -d. -f1 | base64 -d | jq .kid

# Ensure client is using correct JWKS URL
# Should be: http://your-service/{tenant-id}/discovery/v2.0/keys
```

#### Issue: "Tenant not found" errors
**Symptoms**: 404 responses from endpoints
**Causes**:
- Mismatched tenant ID in URL vs configuration
- Client using wrong tenant ID

**Solutions**:
```bash
# Verify configured tenant ID
echo $AZURE_AD_TENANT_ID

# Check endpoint URL format
# Correct: http://localhost:8005/{AZURE_AD_TENANT_ID}/oauth2/v2.0/token
# Wrong: http://localhost:8005/oauth2/v2.0/token
```

### Service Connectivity Issues

#### Issue: "Connection refused" or service unreachable
**Symptoms**: Client cannot connect to mock service
**Causes**:
- Service not running
- Port conflicts
- Network configuration issues
- Docker networking problems

**Solutions**:
```bash
# Check if service is running
curl http://localhost:8005/health

# Verify port binding
netstat -tlnp | grep 8005

# Check Docker container status
docker ps | grep mock-auth-service

# Test Docker networking
docker exec -it your-container curl http://localhost:8000/health
```

#### Issue: CORS errors in browser applications
**Symptoms**: Browser blocks requests to mock service
**Causes**:
- Missing CORS configuration
- Different origins between client and service

**Solutions**:
- Configure CORS middleware in FastAPI application
- Use same origin for client and service in development
- Set up proper reverse proxy configuration

### Configuration Validation

#### Issue: Invalid configuration values
**Symptoms**: Service starts but behaves unexpectedly
**Causes**:
- Invalid URL formats in `AZURE_AD_AUTHORITY`
- Empty or malformed tenant/client IDs

**Solutions**:
```bash
# Validate URL format
echo $AZURE_AD_AUTHORITY | grep -E '^https?://'

# Check ID formats (should be UUIDs)
echo $AZURE_AD_TENANT_ID | grep -E '^[0-9a-f-]{36}$'
echo $AZURE_AD_CLIENT_ID | grep -E '^[0-9a-f-]{36}$'

# Test configuration with health endpoint
curl http://localhost:8005/health
```

## Development

### Running Tests
```bash
# Install test dependencies
pip install -e .[test]

# Run all tests
pytest

# Run specific test categories
pytest test_key_manager.py -v
pytest test_config.py -v
```

### Project Structure
```
mock_auth_service/
├── src/
│   ├── __init__.py
│   ├── main.py          # FastAPI application
│   ├── config.py        # Configuration management
│   └── key_manager.py   # RSA key management
├── keys/                # Persistent key storage
│   ├── private_key.pem  # RSA private key (auto-generated)
│   └── key_id.txt       # Key ID (auto-generated)
├── tests/               # Test files
├── Dockerfile           # Container configuration
├── pyproject.toml       # Python project configuration
└── README.md           # This file
```

## Security Considerations

⚠️ **Important**: This is a mock service intended for development and testing only.

- **Do not use in production**: This service bypasses real authentication
- **Key Security**: Private keys are stored unencrypted for development convenience
- **Token Validation**: Tokens are signed but contain mock user data
- **Network Security**: Service should only be accessible within development environment

For production applications, always use real Azure AD or other production-grade identity providers.