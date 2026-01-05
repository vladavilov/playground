"""
Gateway policy constants and helpers for Envoy `ext_authz`.
"""

SERVICE_TOKEN_AUDIENCE = "internal-services"

# Only these upstream routes receive x-user-id (trusted user identity) from the gateway.
X_USER_ID_ALLOWED_PREFIXES: tuple[str, ...] = (
    "/project/",
    "/gitlab/",
    "/auth/gitlab/",
)


def is_x_user_id_allowed(path: str) -> bool:
    path = path if path.startswith("/") else f"/{path}"
    return any(path.startswith(prefix) for prefix in X_USER_ID_ALLOWED_PREFIXES)


