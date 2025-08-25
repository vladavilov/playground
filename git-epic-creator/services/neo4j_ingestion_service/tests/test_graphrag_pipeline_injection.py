import json
from unittest.mock import patch
from types import SimpleNamespace
import sys


def test_prepare_document_from_json_injects_project_id_into_object(tmp_path):
	with patch.dict(sys.modules, {
		"neo4j": SimpleNamespace(Driver=object),
		"neo4j_graphrag": SimpleNamespace(),
		"neo4j_graphrag.embeddings": SimpleNamespace(OpenAIEmbeddings=lambda **_: object()),
		"neo4j_graphrag.llm": SimpleNamespace(OpenAILLM=lambda **_: object(), AzureOpenAILLM=lambda **_: object()),
		"neo4j_graphrag.experimental": SimpleNamespace(),
		"neo4j_graphrag.experimental.pipeline": SimpleNamespace(),
		"neo4j_graphrag.experimental.pipeline.kg_builder": SimpleNamespace(SimpleKGPipeline=(lambda *a, **k: SimpleNamespace(run=lambda **_: None))),
		"neo4j_graphrag.indexes": SimpleNamespace(create_vector_index=lambda *a, **k: None),
		"configuration": SimpleNamespace(),
		"configuration.graphrag_config": SimpleNamespace(get_graphrag_settings=lambda: SimpleNamespace(
			OAI_EMBED_MODEL="text-embedding-3-small",
			OAI_MODEL="gpt-4o-mini",
			OAI_KEY="test-key",
			OAI_BASE_URL=None,
			OAI_API_VERSION=None
		)),
	}):
		sys.modules.pop("ingestion.graphrag_pipeline", None)
		from ingestion.graphrag_pipeline import prepare_document_from_json

		data = {"text": "hello"}
		path = tmp_path / "doc.json"
		path.write_text(json.dumps(data), encoding="utf-8")

		document = prepare_document_from_json(str(path), "proj-1")

		# Assert project_id is present in the document
		assert document["project_id"] == "proj-1"
		assert "hello" in document["text"]
		assert "path" in document


def test_prepare_document_from_json_handles_list_data(tmp_path):
	with patch.dict(sys.modules, {
		"neo4j": SimpleNamespace(Driver=object),
		"neo4j_graphrag": SimpleNamespace(),
		"neo4j_graphrag.embeddings": SimpleNamespace(OpenAIEmbeddings=lambda **_: object()),
		"neo4j_graphrag.llm": SimpleNamespace(OpenAILLM=lambda **_: object(), AzureOpenAILLM=lambda **_: object()),
		"neo4j_graphrag.experimental": SimpleNamespace(),
		"neo4j_graphrag.experimental.pipeline": SimpleNamespace(),
		"neo4j_graphrag.experimental.pipeline.kg_builder": SimpleNamespace(SimpleKGPipeline=(lambda *a, **k: SimpleNamespace(run=lambda **_: None))),
		"neo4j_graphrag.indexes": SimpleNamespace(create_vector_index=lambda *a, **k: None),
		"configuration": SimpleNamespace(),
		"configuration.graphrag_config": SimpleNamespace(get_graphrag_settings=lambda: SimpleNamespace(
			OAI_EMBED_MODEL="text-embedding-3-small",
			OAI_MODEL="gpt-4o-mini",
			OAI_KEY="test-key",
			OAI_BASE_URL=None,
			OAI_API_VERSION=None
		)),
	}):
		sys.modules.pop("ingestion.graphrag_pipeline", None)
		from ingestion.graphrag_pipeline import prepare_document_from_json

		data = [{"text": "a"}, {"text": "b"}]
		path = tmp_path / "list.json"
		path.write_text(json.dumps(data), encoding="utf-8")

		document = prepare_document_from_json(str(path), "proj-2")

		# The function should handle list data by extracting the first item or creating a summary
		assert document["project_id"] == "proj-2"
		assert "path" in document
		assert "text" in document


