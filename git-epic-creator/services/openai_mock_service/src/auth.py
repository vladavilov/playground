from fastapi import HTTPException, Request
from config import get_config


def require_authentication(request: Request) -> None:
    config = get_config()
    required_key = config["OAI_KEY"]
    if not required_key:
        return
    auth_header = request.headers.get("authorization")
    api_key_header = request.headers.get("api-key") or request.headers.get("x-api-key")
    expected = f"Bearer {required_key}"
    if auth_header == expected:
        return
    if api_key_header == required_key:
        return
    raise HTTPException(status_code=401, detail="Unauthorized")

