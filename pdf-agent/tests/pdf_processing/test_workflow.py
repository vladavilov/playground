import os
import json
from unittest.mock import patch, MagicMock, PropertyMock
from agno.workflow import RunResponse
from agno.models.response import ModelResponse
from src.pdf_processing.workflow import PDFExtractionWorkflow, to_json_template
from src.config import get_settings

# Set env vars for the CHAT model. Embedding model is now local.
os.environ["AZURE_OPENAI_API_KEY"] = "test_key"
os.environ["AZURE_OPENAI_CHAT_ENDPOINT"] = "https://test-chat.openai.azure.com/"
os.environ["AZURE_OPENAI_LLM_MODEL_NAME"] = "test-llm"


@patch("src.pdf_processing.workflow.extract_text_from_pdf")
@patch("src.pdf_processing.workflow.AzureAIFoundry")
def test_pdf_extraction_workflow_agent_integration(
    mock_azure_ai_foundry: MagicMock,
    mock_extract_text: MagicMock,
    mock_chat_model: MagicMock,
):
    """
    Integration test verifying the workflow creates a new agent for each
    property group and passes the correct RAG context.
    This test uses the real SentenceTransformerEmbedder.
    """
    pdf_path = "dummy.pdf"  # Path is now arbitrary since text extraction is mocked
    mock_extract_text.return_value = "The document title is 'Sample Invoice'. The ISIN is US1234567890."
    mock_agent_response_content = {"document_title": "Sample Invoice"}

    settings = get_settings()
    settings.property_groups = [
        {
            "group_name": "document_info",
            "properties": [{
                "name": "document_title",
                "rag_query": "Extract the document title"
            }]
        }
    ]
    settings.TOP_K = 1

    # This mock is for the chat model's response
    type(mock_chat_model.response.return_value).content = PropertyMock(
        return_value=json.dumps(mock_agent_response_content)
    )

    with patch("src.pdf_processing.workflow.get_settings", return_value=settings):
        workflow = PDFExtractionWorkflow()
        
        # Check that the chat model was initialized correctly
        mock_azure_ai_foundry.assert_called_once_with(
            id=settings.AZURE_OPENAI_LLM_MODEL_NAME,
            api_key=settings.AZURE_OPENAI_API_KEY,
            azure_endpoint=settings.AZURE_OPENAI_CHAT_ENDPOINT,
            temperature=0.0,
        )

        # Replace the real chat model with our mock to control its output
        workflow.chat_model = mock_chat_model
        result = workflow.run(pdf_file_path=pdf_path)

    # The agent should be called once for the single property group.
    mock_chat_model.response.assert_called_once()
    
    # The initial text extraction should be called.
    mock_extract_text.assert_called_once_with(pdf_path)

    assert isinstance(result, RunResponse)
    rag_result = result.content["rag_results"][0]
    assert rag_result["extracted_data"] == mock_agent_response_content
    # Assert that the correct chunk was retrieved by the vector search
    assert "The document title is 'Sample Invoice'" in rag_result["relevant_chunks"][0]


@patch("src.pdf_processing.workflow.extract_text_from_pdf")
@patch("src.pdf_processing.workflow.AzureAIFoundry")
def test_workflow_aggregates_results(
    mock_azure_ai_foundry: MagicMock,
    mock_extract_text: MagicMock,
    mock_chat_model: MagicMock,
):
    """
    Tests that the workflow correctly aggregates the extracted data from
    multiple property groups into a single dictionary, using the real embedder.
    """
    pdf_path = "dummy.pdf"
    mock_extract_text.return_value = "The document title is 'Sample Invoice'. The ISIN is US1234567890."
    
    settings = get_settings()
    settings.property_groups = [
        {
            "group_name": "group1", 
            "properties": [
                {"name": "document_title", "rag_query": "What is the title?"},
            ]
        },
        {
            "group_name": "group2", 
            "properties": [{"name": "isin", "rag_query": "What is the ISIN?"}]
        },
    ]
    settings.TOP_K = 1
    
    # Mock the sequence of responses from the chat model
    type(mock_chat_model.response.return_value).content = PropertyMock(
        side_effect=[
            json.dumps({"document_title": "Sample Invoice"}), 
            json.dumps({"isin": "US1234567890"})
        ]
    )

    with patch("src.pdf_processing.workflow.get_settings", return_value=settings):
        workflow = PDFExtractionWorkflow()
        
        # Replace the real chat model with our mock
        workflow.chat_model = mock_chat_model
        result = workflow.run(pdf_file_path=pdf_path)

    assert isinstance(result, RunResponse)
    assert mock_chat_model.response.call_count == 2
    
    content = result.content
    assert "aggregated_data" in content
    expected_data = {"document_title": "Sample Invoice", "isin": "US1234567890"}
    assert content["aggregated_data"] == expected_data
    assert len(content["rag_results"]) == 2


def test_to_json_template():
    properties = [
        {"name": "document_title", "rag_query": "Extract the document title"},
        {"name": "isin", "rag_query": "Extract the ISIN"}
    ]
    expected_template = {
        "document_title": "Extract the document title",
        "isin": "Extract the ISIN"
    }
    assert to_json_template(properties) == expected_template

def test_to_json_template_empty():
    properties = []
    expected_template = {}
    assert to_json_template(properties) == expected_template

def test_to_json_template_single_property():
    properties = [
        {"name": "document_type", "rag_query": "Classify the document type"}
    ]
    expected_template = {
        "document_type": "Classify the document type"
    }
    assert to_json_template(properties) == expected_template 