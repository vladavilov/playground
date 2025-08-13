import pytest
from unittest.mock import patch


def test_graphrag_settings_defaults():
    from configuration.graphrag_config import GraphRAGSettings

    settings = GraphRAGSettings()

    assert settings.RAG_WORKSPACE_ROOT == "./graphrag"
    assert settings.GRAPHRAG_LLM_THREAD_COUNT == 8
    assert settings.GRAPHRAG_EMBEDDING_THREAD_COUNT == 8


@patch.dict(
    "os.environ",
    {
        "RAG_WORKSPACE_ROOT": "/tmp/graphrag-root",
        "GRAPHRAG_LLM_THREAD_COUNT": "4",
        "GRAPHRAG_EMBEDDING_THREAD_COUNT": "16",
    },
    clear=False,
)
def test_graphrag_settings_env_override():
    from configuration.graphrag_config import GraphRAGSettings

    settings = GraphRAGSettings()

    assert settings.RAG_WORKSPACE_ROOT == "/tmp/graphrag-root"
    assert settings.GRAPHRAG_LLM_THREAD_COUNT == 4
    assert settings.GRAPHRAG_EMBEDDING_THREAD_COUNT == 16


def test_graphrag_settings_validation_errors():
    from pydantic import ValidationError
    with patch.dict("os.environ", {"GRAPHRAG_LLM_THREAD_COUNT": "0"}):
        from configuration.graphrag_config import GraphRAGSettings
        with pytest.raises(ValidationError):
            GraphRAGSettings()

    with patch.dict("os.environ", {"GRAPHRAG_EMBEDDING_THREAD_COUNT": "-1"}):
        from configuration.graphrag_config import GraphRAGSettings
        with pytest.raises(ValidationError):
            GraphRAGSettings()


