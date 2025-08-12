"""Common retry/DLQ policy helpers for tasks across services."""

from __future__ import annotations

from typing import Optional, Tuple

from configuration.retry_config import get_retry_settings


def compute_retry_decision(current_attempts: int) -> Tuple[bool, Optional[int], int]:
    """
    Compute whether to send to DLQ or retry with backoff.

    Args:
        current_attempts: Attempts value from the last try (non-negative integer)

    Returns:
        (to_dlq, countdown_seconds_or_None, next_attempts)
    """
    settings = get_retry_settings()
    max_attempts = settings.RETRY_MAX_ATTEMPTS
    backoff_base = settings.RETRY_BACKOFF_BASE_SEC
    backoff_factor = settings.RETRY_BACKOFF_FACTOR
    backoff_max = settings.RETRY_BACKOFF_MAX_SEC

    next_attempts = int(current_attempts or 0) + 1
    to_dlq = next_attempts >= max_attempts

    if to_dlq:
        return True, None, next_attempts

    exponent = max(0, next_attempts - 1)
    countdown = backoff_base * (backoff_factor ** exponent)
    countdown = min(countdown, backoff_max)
    return False, countdown, next_attempts


