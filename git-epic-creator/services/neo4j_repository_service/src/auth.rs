use axum::{
    http::{HeaderMap, StatusCode},
    response::{IntoResponse, Response},
};
use base64::Engine;
use hmac::{Hmac, Mac};
use serde::Deserialize;
use serde_json::json;
use sha2::Sha256;
use subtle::ConstantTimeEq;

#[derive(Debug, Deserialize)]
struct Claims {
    sub: Option<String>,
    iss: Option<String>,
    exp: Option<u64>,
}

fn unauthorized(message: &str) -> Response {
    (
        StatusCode::UNAUTHORIZED,
        axum::Json(json!({ "error": message })),
    )
        .into_response()
}

fn extract_bearer(headers: &HeaderMap) -> Option<String> {
    let v = headers.get(axum::http::header::AUTHORIZATION)?.to_str().ok()?;
    let (scheme, token) = v.split_once(' ')?;
    if !scheme.eq_ignore_ascii_case("bearer") {
        return None;
    }
    let token = token.trim();
    if token.is_empty() {
        return None;
    }
    Some(token.to_string())
}

fn b64url_decode(s: &str) -> Result<Vec<u8>, ()> {
    base64::engine::general_purpose::URL_SAFE_NO_PAD
        .decode(s.as_bytes())
        .map_err(|_| ())
}

fn b64url_encode(bytes: &[u8]) -> String {
    base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes)
}

fn now_unix_seconds() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}

pub fn verify_s2s_jwt(headers: &HeaderMap) -> Result<(), Response> {
    let token = extract_bearer(headers).ok_or_else(|| unauthorized("Authentication required"))?;

    let secret = std::env::var("LOCAL_JWT_SECRET")
        .map(|v| v.trim().to_string())
        .unwrap_or_default();
    if secret.is_empty() {
        return Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            axum::Json(json!({ "error": "LOCAL_JWT_SECRET is not set" })),
        )
            .into_response());
    }

    let mut parts = token.split('.');
    let header_b64 = parts.next().unwrap_or("");
    let payload_b64 = parts.next().unwrap_or("");
    let sig_b64 = parts.next().unwrap_or("");
    if header_b64.is_empty() || payload_b64.is_empty() || sig_b64.is_empty() || parts.next().is_some() {
        return Err(unauthorized("Invalid or expired token"));
    }

    // Verify signature: HS256 over "<header>.<payload>"
    let signing_input = format!("{}.{}", header_b64, payload_b64);
    let sig = b64url_decode(sig_b64).map_err(|_| unauthorized("Invalid or expired token"))?;

    let mut mac = Hmac::<Sha256>::new_from_slice(secret.as_bytes())
        .map_err(|_| unauthorized("Invalid or expired token"))?;
    mac.update(signing_input.as_bytes());
    let expected = mac.finalize().into_bytes();

    if expected.as_slice().ct_eq(sig.as_slice()).unwrap_u8() != 1 {
        return Err(unauthorized("Invalid or expired token"));
    }

    // Parse claims from payload
    let payload = b64url_decode(payload_b64).map_err(|_| unauthorized("Invalid or expired token"))?;
    let claims: Claims =
        serde_json::from_slice(&payload).map_err(|_| unauthorized("Invalid or expired token"))?;

    // Verify expiration
    let exp = claims.exp.unwrap_or(0);
    if exp == 0 || now_unix_seconds() >= exp {
        return Err(unauthorized("Invalid or expired token"));
    }

    let sub = claims.sub.unwrap_or_default();
    let iss = claims.iss.unwrap_or_default();

    if sub != "api-gateway" {
        return Err(unauthorized("Invalid token: invalid caller"));
    }
    if iss != "authentication-service" {
        return Err(unauthorized("Invalid token: invalid issuer"));
    }

    Ok(())
}


