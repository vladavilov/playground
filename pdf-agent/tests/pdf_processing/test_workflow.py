import os
import json
from unittest.mock import patch, MagicMock, PropertyMock
from agno.workflow import RunResponse
from agno.models.response import ModelResponse
from src.pdf_processing.workflow import PDFExtractionWorkflow
from src.config import get_settings

# Set environment variables for the test session
os.environ["AZURE_OPENAI_API_KEY"] = "test_key"
os.environ["AZURE_OPENAI_ENDPOINT"] = "https://test.openai.azure.com/"
os.environ["AZURE_OPENAI_LLM_DEPLOYMENT_NAME"] = "test_llm_deployment"
os.environ["AZURE_OPENAI_EMBEDDINGS_DEPLOYMENT_NAME"] = "test_embeddings_deployment"


@patch("src.pdf_processing.workflow.PDFExtractionWorkflow._search_vector_store")
@patch("src.pdf_processing.workflow.AzureOpenAIEmbedder")
def test_pdf_extraction_workflow_agent_integration(
    mock_azure_embedder: MagicMock,
    mock_search_vector_store: MagicMock,
    mock_chat_model: MagicMock,
):
    """
    Integration test verifying the workflow creates a new agent for each
    property group and passes the correct RAG context.
    """
    pdf_path = "tests/test_data/pdfs/sample.pdf"
    expected_chunks = ["Hello World.", "This is page 2."]
    mock_agent_response_content = {"greeting_text": "Hello World."}

    settings = get_settings()
    settings.property_groups = [
        {"group_name": "greeting", "rag_query": "A query.", "properties": [{"name": "greeting_text"}]}
    ]
    settings.TOP_K = 1

    mock_embedder_instance = MagicMock()
    mock_embedder_instance.embed_documents.return_value = [[0.1, 0.2], [0.3, 0.4]]
    mock_azure_embedder.return_value = mock_embedder_instance

    mock_search_vector_store.return_value = [0]
    
    # Configure the mock model from the fixture
    mock_chat_model.response.return_value.content = json.dumps(mock_agent_response_content)

    with patch("src.pdf_processing.workflow.get_settings", return_value=settings):
        workflow = PDFExtractionWorkflow()
        # Replace the workflow's chat model with our mock
        workflow.chat_model = mock_chat_model
        result = workflow.run(pdf_file_path=pdf_path)

    mock_chat_model.response.assert_called_once()

    assert isinstance(result, RunResponse)
    final_data = result.content["rag_results"][0]
    assert final_data["extracted_data"] == mock_agent_response_content
    assert final_data["relevant_chunks"] == [expected_chunks[0]]
    mock_search_vector_store.assert_called_once()


@patch("src.pdf_processing.workflow.AzureOpenAIEmbedder")
def test_workflow_aggregates_results(
    mock_azure_embedder: MagicMock,
    mock_chat_model: MagicMock,
):
    """
    Tests that the workflow correctly aggregates the extracted data from
    multiple property groups into a single dictionary.
    """
    pdf_path = "tests/test_data/pdfs/sample.pdf"
    
    settings = get_settings()
    settings.property_groups = [
        {
            "group_name": "group1",
            "rag_query": "Query 1",
            "properties": [{"name": "prop1"}],
        },
        {
            "group_name": "group2",
            "rag_query": "Query 2",
            "properties": [{"name": "prop2"}],
        },
    ]
    settings.TOP_K = 1

    mock_embedder_instance = MagicMock()
    mock_embedder_instance.embed_documents.return_value = [[0.1, 0.2]]
    mock_embedder_instance.embed_query.side_effect = [[0.1], [0.2]]
    mock_azure_embedder.return_value = mock_embedder_instance

    # Configure the mock model to return different responses on subsequent calls
    # We use a side effect on the `content` attribute of the mock response object
    # that the `mock_chat_model` fixture provides. This preserves the other
    # attributes of the mock response, like `tool_executions`.
    type(mock_chat_model.response.return_value).content = PropertyMock(
        side_effect=[
            json.dumps({"prop1": "value1"}),
            json.dumps({"prop2": "value2"}),
        ]
    )

    with patch("src.pdf_processing.workflow.get_settings", return_value=settings):
        # We need to re-patch search_vector_store here because the workflow
        # instance is created inside the test.
        with patch("src.pdf_processing.workflow.PDFExtractionWorkflow._search_vector_store") as mock_search:
            mock_search.return_value = [0]
            
            workflow = PDFExtractionWorkflow()
            workflow.chat_model = mock_chat_model
            result = workflow.run(pdf_file_path=pdf_path)

    assert isinstance(result, RunResponse)
    assert mock_chat_model.response.call_count == 2
    content = result.content
    assert "aggregated_data" in content
    expected_data = {"prop1": "value1", "prop2": "value2"}
    assert content["aggregated_data"] == expected_data
    assert len(content["rag_results"]) == 2 