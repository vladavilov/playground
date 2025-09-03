import pytest
from typing import Any, Dict, List, Callable, Tuple, Optional, Iterable
from unittest.mock import MagicMock, call
import types

from ingestion.community_summarizer import CommunitySummarizerService


class _FakeRecord(dict):
    def single(self) -> "_FakeRecord":
        return self


class _FakeResult:
    def __init__(self, rows: List[Dict[str, Any]]):
        self._rows: List[Dict[str, Any]] = rows

    def single(self) -> Optional[Dict[str, Any]]:
        return self._rows[0] if self._rows else None

    def __iter__(self) -> Iterable[Dict[str, Any]]:
        return iter(self._rows)


class _FakeSession:
    def __init__(self, scripted_runs: List[Tuple[Callable[[str, Dict[str, Any]], bool], Any]]):
        # scripted_runs: list of tuples (matcher, result)
        self._scripted_runs = scripted_runs

    def __enter__(self) -> "_FakeSession":
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> bool:
        return False

    def run(self, query: str, params: Dict[str, Any] | None = None):
        params = params or {}
        for matcher, result in self._scripted_runs:
            if matcher(query, params):
                return result
        raise AssertionError(f"Unexpected query: {query} params: {params}")


class _FakeDriver:
    def __init__(self, session: _FakeSession):
        self._session = session

    def session(self) -> _FakeSession:
        return self._session


@pytest.fixture
def make_service(monkeypatch):
    def _factory(session: _FakeSession) -> CommunitySummarizerService:
        # Stub config accessor to avoid importing real settings
        monkeypatch.setattr(
            "ingestion.community_summarizer.get_graphrag_settings",
            lambda: types.SimpleNamespace(OAI_MODEL="fake-model"),
            raising=True,
        )
        driver = _FakeDriver(session)
        # Avoid building real LLM/settings
        monkeypatch.setattr(CommunitySummarizerService, "_build_llm", lambda self: MagicMock())
        svc = CommunitySummarizerService(driver)
        # Stub LLM summary method to deterministic text
        monkeypatch.setattr(svc, "_get_llm_summary", lambda prompt: "SUMMARY")
        # Spy update calls
        svc._update_community_summary = MagicMock()
        return svc
    return _factory


def _match_max_level(query: str, params: Dict[str, Any]) -> bool:
    return "max(c.level)" in query


def _match_level_ids(level: int) -> Callable[[str, Dict[str, Any]], bool]:
    def _m(query: str, params: Dict[str, Any]) -> bool:
        return "RETURN c.id AS id" in query and params.get("level") == level
    return _m


def _match_children_of(pid: str) -> Callable[[str, Dict[str, Any]], bool]:
    def _m(query: str, params: Dict[str, Any]) -> bool:
        return "OPTIONAL MATCH (p)<-[:IN_COMMUNITY]-(child:__Community__)" in query and params.get("pid") == pid
    return _m


@pytest.mark.asyncio
async def test_summarize_level0_only_no_higher_levels(make_service):
    # Arrange: Only level 0 exists
    scripted = [
        (_match_max_level, _FakeResult([_FakeRecord({"max_level": 0})])),
    ]
    session = _FakeSession(scripted)
    svc = make_service(session)

    input_communities = [
        {"communityId": "0-A", "nodes": [], "rels": []},
        {"communityId": "0-B", "nodes": [], "rels": []},
    ]

    # Act
    result = await svc.summarize_communities(input_communities)

    # Assert
    assert result["success"] is True
    assert result["successful_summaries"] == 2
    assert svc._update_community_summary.call_count == 2
    svc._update_community_summary.assert_any_call("0-A", "SUMMARY")
    svc._update_community_summary.assert_any_call("0-B", "SUMMARY")


@pytest.mark.asyncio
async def test_summarize_with_higher_levels(monkeypatch, make_service):
    # Arrange: levels 0..2, with one L1 (1-A) and one L2 (2-A)
    scripted = [
        (_match_max_level, _FakeResult([_FakeRecord({"max_level": 2})])),
        (_match_level_ids(1), _FakeResult([_FakeRecord({"id": "1-A"})])),
        (_match_level_ids(2), _FakeResult([_FakeRecord({"id": "2-A"})])),
        (_match_children_of("1-A"), _FakeResult([
            _FakeRecord({
                "communityId": "1-A",
                "communities": [
                    {"id": "0-A", "summary": "sum A", "numberOfChildren": 0},
                    {"id": "0-B", "summary": "sum B", "numberOfChildren": 0},
                ],
            })
        ])),
        (_match_children_of("2-A"), _FakeResult([
            _FakeRecord({
                "communityId": "2-A",
                "communities": [
                    {"id": "1-A", "summary": "parent sum", "numberOfChildren": 1},
                ],
            })
        ])),
    ]
    session = _FakeSession(scripted)
    svc = make_service(session)

    # Make level-0 input
    input_communities = [
        {"communityId": "0-A", "nodes": [], "rels": []},
        {"communityId": "0-B", "nodes": [], "rels": []},
    ]

    # Act
    res = await svc.summarize_communities(input_communities)

    # Assert: 2 (level-0) + 2 (L1, L2)
    assert res["success"] is True
    assert res["successful_summaries"] == 4
    calls = [call.args for call in svc._update_community_summary.call_args_list]
    updated_ids = [cid for cid, _ in calls]
    assert set(updated_ids) == {"0-A", "0-B", "1-A", "2-A"}
