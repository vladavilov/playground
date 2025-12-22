import importlib
import sys
from unittest.mock import Mock, patch

from fastapi.testclient import TestClient


def _import_main_with_mocked_clients():
    """
    Import service main module with DB clients mocked to avoid real connections.

    Note: tests run with pythonpath=["src"], so module name is `main`.
    """
    sys.modules.pop("main", None)

    with patch("utils.app_factory.get_postgres_client", return_value=Mock()), patch(
        "utils.app_factory.get_neo4j_client", return_value=Mock()
    ):
        import main  # type: ignore

        importlib.reload(main)
        return main


def test_health_returns_required_keys() -> None:
    main = _import_main_with_mocked_clients()
    client = TestClient(main.app)
    resp = client.get("/health")

    assert resp.status_code == 200
    assert resp.json() == {"status": "ok"}
