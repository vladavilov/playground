from unittest.mock import MagicMock
from uuid import uuid4

from persistence.repo_index_store import canonical_json_bytes, sha256_hex, upsert_repo_index


def test_canonical_json_bytes_is_stable() -> None:
    a = {"b": 2, "a": 1}
    b = {"a": 1, "b": 2}
    assert canonical_json_bytes(a) == canonical_json_bytes(b) == b'{"a":1,"b":2}'


def test_upsert_computes_content_sha256_and_calls_session() -> None:
    # Mock PostgresClient.get_sync_session() context manager and session behavior.
    session = MagicMock()
    session.query.return_value.filter.return_value.filter.return_value.one_or_none.return_value = None

    cm = MagicMock()
    cm.__enter__.return_value = session
    cm.__exit__.return_value = False

    pg = MagicMock()
    pg.get_sync_session.return_value = cm

    repo_index = {"files": [{"path": "a.cbl"}], "project_id": "x"}
    res = upsert_repo_index(
        postgres_client=pg,
        project_id=uuid4(),
        repo_fingerprint="fp",
        repo_index_json=repo_index,
        created_by="tester",
    )

    assert res.content_sha256 == sha256_hex(canonical_json_bytes(repo_index))
    session.add.assert_called_once()


