import pytest


def test_bundle_dtos_drop_unknown_keys_and_coerce_lists():
    np = pytest.importorskip("numpy")

    from ingestion_ms.dtos import RequirementsGraphBundleRequest

    req = RequirementsGraphBundleRequest.model_validate(
        {
            "project_id": "p1",
            "documents": [{"id": "d1", "unknown": "x", "metadata": {"file_name": "a.txt"}}],
            "chunks": [{"id": "c1", "document_ids": np.array(["d1"]), "extra": 1}],
            "entities": [{"id": "e1", "text_unit_ids": ("c1",), "more": {"x": 1}}],
            "relationships": [{"source": "A", "target": "B", "text_unit_ids": np.array(["c1"])}],
            "community_reports": [{"community": 1.0, "level": "0", "title": "t"}],
            "communities": [{"community": "1", "entity_ids": [123, "e1"], "text_unit_ids": None}],
        }
    )

    payload = req.model_dump(mode="python")
    assert payload["project_id"] == "p1"
    assert payload["documents"][0] == {"id": "d1", "title": "a.txt"}
    assert payload["chunks"][0]["document_ids"] == ["d1"]
    assert payload["entities"][0]["text_unit_ids"] == ["c1"]
    assert payload["relationships"][0]["text_unit_ids"] == ["c1"]
    assert payload["community_reports"][0]["community"] == 1
    assert payload["communities"][0]["entity_ids"] == ["123", "e1"]
    assert payload["communities"][0]["text_unit_ids"] == []


def test_embedding_dto_converts_ndarray_to_list():
    np = pytest.importorskip("numpy")

    from ingestion_ms.dtos import ChunkEmbeddingRow

    m = ChunkEmbeddingRow.model_validate({"id": "c1", "embedding": np.array([1.0, 2.0, 3.0])})
    assert m.model_dump(mode="python") == {"id": "c1", "embedding": [1.0, 2.0, 3.0]}


