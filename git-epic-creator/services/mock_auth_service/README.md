# Mock Azure AD Authentication Service

A FastAPI-based mock authentication service that emulates Azure AD OIDC endpoints for local development and testing. This service provides JWT token generation with persistent RSA key management and supports all standard OIDC discovery endpoints.

## Features

- **MCP Authorization Spec Compliance**: Supports all three RFC 8414 discovery URL formats required by MCP clients
- **OIDC Discovery Endpoint**: Provides standard OpenID Connect configuration
- **JWT Token Generation**: Issues signed JWT tokens with configurable claims
- **JWKS Endpoint**: Serves public keys for token verification
- **PKCE Support**: Supports S256 and plain code challenge methods
- **Persistent Key Management**: Maintains RSA keys across service restarts
- **Flexible Key Configuration**: Supports environment variables and file-based key storage
- **Docker Support**: Ready for containerized deployment

## Configuration

### Environment Variables

The service can be configured using the following environment variables:

#### Core Service Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `AZURE_AD_TENANT_ID` / `AZURE_TENANT_ID` | `e7963c3a-3b3a-43b6-9426-89e433d07e69` | Azure AD tenant ID used in token claims and endpoints |
| `AZURE_AD_CLIENT_ID` / `AZURE_CLIENT_ID` | `a9e304a9-5b6c-4ef7-9b37-23a579a6d7be` | Client ID (audience) for issued tokens |
| `AZURE_AD_AUTHORITY` | `http://mock-auth-service:8005` | Base URL for the mock service endpoints |
| `API_PORT` | `8000` | Port on which the service runs |

#### Key Management Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `MOCK_AUTH_PRIVATE_KEY` | `""` | RSA private key in PEM format for JWT signing |
| `MOCK_AUTH_KEY_ID` | `""` | Key ID (kid) for JWT header |
| `AZURE_CLIENT_SECRET` / `MOCK_CLIENT_SECRET` | `""` | Optional client secret check at token endpoint |

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

#### Authorization Server Metadata Discovery (RFC 8414)

MCP clients per the [MCP Authorization spec](https://modelcontextprotocol.io/specification/draft/basic/authorization) try these endpoints in order:

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/.well-known/oauth-authorization-server/{tenant_id}/v2.0` | GET | RFC 8414 path insertion (tried FIRST) |
| `/.well-known/openid-configuration/{tenant_id}/v2.0` | GET | OIDC path insertion (tried SECOND) |
| `/{tenant_id}/v2.0/.well-known/openid-configuration` | GET | OIDC path appending (tried THIRD) |

All three endpoints return the same Authorization Server Metadata including `authorization_endpoint`, `token_endpoint`, etc.

#### OAuth Flow Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/health` | GET | Health check endpoint |
| `/{tenant_id}/discovery/v2.0/keys` | GET | JWKS endpoint (public keys for token verification) |
| `/{tenant_id}/oauth2/v2.0/token` | POST | Token endpoint: `authorization_code`, `refresh_token` grants |
| `/{tenant_id}/oauth2/v2.0/authorize` | GET | Authorization endpoint (issues code; supports PKCE S256/plain) |
| `/{tenant_id}/v2.0/userinfo` | GET | UserInfo endpoint (returns mock user profile) |
| `/{tenant_id}/oauth2/v2.0/devicecode` | POST | Device code endpoint (mock) |
| `/{tenant_id}/oauth2/v2.0/logout` | POST | Logout endpoint (mock) |

### Token Endpoint Authentication

The token endpoint supports both OAuth client authentication methods:

- **client_secret_basic**: Credentials in `Authorization: Basic base64(client_id:client_secret)` header
- **client_secret_post**: Credentials in form body (`client_id`, `client_secret` fields)

### Example Token Request
Authorization code exchange:
```bash
curl -X POST "http://localhost:8005/e7963c3a-3b3a-43b6-9426-89e433d07e69/oauth2/v2.0/token" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "grant_type=authorization_code&client_id=$AZURE_CLIENT_ID&client_secret=$AZURE_CLIENT_SECRET&code=$CODE&redirect_uri=http://localhost/callback&code_verifier=$VERIFIER"
```

Refresh grant:
```bash
curl -X POST "http://localhost:8005/e7963c3a-3b3a-43b6-9426-89e433d07e69/oauth2/v2.0/token" \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "grant_type=refresh_token&client_id=$AZURE_CLIENT_ID&client_secret=$AZURE_CLIENT_SECRET&refresh_token=$RTOKEN"
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