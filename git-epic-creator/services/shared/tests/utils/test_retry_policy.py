def test_compute_retry_decision_backoff(monkeypatch):
    monkeypatch.setenv("RETRY_MAX_ATTEMPTS", "3")
    monkeypatch.setenv("RETRY_BACKOFF_BASE_SEC", "2")
    monkeypatch.setenv("RETRY_BACKOFF_FACTOR", "2")
    monkeypatch.setenv("RETRY_BACKOFF_MAX_SEC", "60")

    from utils.retry_policy import compute_retry_decision

    to_dlq, countdown, next_attempts = compute_retry_decision(0)
    assert to_dlq is False
    assert countdown == 2  # 2 * 2^0
    assert next_attempts == 1

    to_dlq2, countdown2, next_attempts2 = compute_retry_decision(1)
    assert to_dlq2 is False
    assert countdown2 == 4  # 2 * 2^1
    assert next_attempts2 == 2


def test_compute_retry_decision_hits_dlq(monkeypatch):
    monkeypatch.setenv("RETRY_MAX_ATTEMPTS", "3")
    monkeypatch.setenv("RETRY_BACKOFF_BASE_SEC", "1")
    monkeypatch.setenv("RETRY_BACKOFF_FACTOR", "2")
    monkeypatch.setenv("RETRY_BACKOFF_MAX_SEC", "60")

    from utils.retry_policy import compute_retry_decision

    # current attempts=2 => next_attempts=3 => DLQ
    to_dlq, countdown, next_attempts = compute_retry_decision(2)
    assert to_dlq is True
    assert countdown is None
    assert next_attempts == 3


