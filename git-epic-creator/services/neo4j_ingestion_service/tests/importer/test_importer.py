from unittest.mock import MagicMock, patch
import pandas as pd


def test_importer_reads_parquet_and_upserts_batches(tmp_path):
    workdir = tmp_path / "proj-x"
    out = workdir / "output"
    out.mkdir(parents=True)

    # Pretend these files exist; importer should attempt to read them
    for name in [
        "documents.parquet",
        "text_units.parquet",
        "entities.parquet",
        "relationships.parquet",
        "communities.parquet",
        "community_reports.parquet",
    ]:
        (out / name).touch()

    # Mock pandas parquet reads to return DataFrames of specific sizes
    with patch("ingestion.importer.pd.read_parquet") as read_parquet:
        read_parquet.side_effect = [
            pd.DataFrame([{"id": "d1"}, {"id": "d2"}]),
            pd.DataFrame([{"id": "tu1"}]),
            pd.DataFrame([{"id": "e1"}]),
            pd.DataFrame([{"id": "r1"}, {"id": "r2"}, {"id": "r3"}]),
            pd.DataFrame([]),
            pd.DataFrame([{"id": "cr1"}]),
        ]

        # Mock Neo4j client
        client = MagicMock()
        client.execute_query_with_retry.return_value = []

        from ingestion.importer import import_graphrag_outputs

        counts = import_graphrag_outputs(workdir, client)

        assert counts == {
            "documents": 2,
            "text_units": 1,
            "entities": 1,
            "relationships": 3,
            "communities": 0,
            "community_reports": 1,
        }
        assert client.execute_query_with_retry.call_count > 0


def test_ensure_constraints_builds_expected_queries():
    from ingestion.importer import ensure_constraints

    client = MagicMock()
    client.execute_query_with_retry.return_value = []

    ensure_constraints(client)

    calls = [c.args[0] for c in client.execute_query_with_retry.call_args_list]
    assert any("CREATE CONSTRAINT" in q for q in calls)


def test_importer_uses_batched_upserts(tmp_path, monkeypatch):
    workdir = tmp_path / "proj-batch"
    out = workdir / "output"
    out.mkdir(parents=True)
    for name in [
        "documents.parquet",
        "text_units.parquet",
        "entities.parquet",
        "relationships.parquet",
        "communities.parquet",
        "community_reports.parquet",
    ]:
        (out / name).touch()

    rows = [{"id": f"x{i}"} for i in range(5)]
    with patch("ingestion.importer.pd.read_parquet", side_effect=[pd.DataFrame(rows)] * 6):
        client = MagicMock()
        client.execute_query_with_retry.return_value = []

        from ingestion.importer import import_graphrag_outputs

        import_graphrag_outputs(workdir, client)

        # At least constraints + multiple batch queries should be executed
        assert client.execute_query_with_retry.call_count >= 3


