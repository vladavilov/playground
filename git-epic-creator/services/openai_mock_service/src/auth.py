from fastapi import HTTPException, Request
from configuration.common_config import get_app_settings


def require_authentication(request: Request) -> None:
    settings = get_app_settings()
    required_key = settings.llm.OAI_KEY
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

