import json
from unittest.mock import patch, MagicMock, PropertyMock
from agno.workflow import RunResponse
from src.pdf_processing.workflow import PDFExtractionWorkflow, to_json_template
from src.config import get_settings

@patch("src.pdf_processing.workflow.extract_text_from_pdf")
@patch("src.pdf_processing.workflow.clean_text")
@patch("src.pdf_processing.workflow.create_property_extraction_agent")
def test_pdf_extraction_workflow_happy_path(
    mock_create_agent: MagicMock,
    mock_clean_text: MagicMock,
    mock_extract_text: MagicMock,
    mock_chat_model: MagicMock,
):
    """
    Tests the complete PDF extraction workflow with mocked text processing and agent.
    Verifies that the workflow correctly calls its components and returns the
    aggregated result.
    """
    pdf_path = "dummy.pdf"
    raw_text = "The document title is 'Sample Invoice'. The ISIN is US1234567890."
    cleaned_text = "The document title is 'Sample Invoice'. The ISIN is US1234567890."
    
    mock_extract_text.return_value = raw_text
    mock_clean_text.return_value = cleaned_text

    mock_agent = MagicMock()
    mock_create_agent.return_value = mock_agent

    # Mock the sequence of responses from the agent for each property group
    mock_agent.run.side_effect = [
        MagicMock(content=json.dumps({"document_title": "Sample Invoice"})),
        MagicMock(content=json.dumps({"isin": "US1234567890"})),
    ]

    settings = get_settings()
    settings.property_groups = [
        {
            "group_name": "group1", 
            "properties": [{"name": "document_title", "description": "What is the title?"}],
        },
        {
            "group_name": "group2", 
            "properties": [{"name": "isin", "description": "What is the ISIN?"}],
        },
    ]

    with patch("src.pdf_processing.workflow.get_settings", return_value=settings):
        workflow = PDFExtractionWorkflow()
        # We need to manually inject the mocked chat model into the workflow instance
        workflow.chat_model = mock_chat_model
        
        result = workflow.run(pdf_file_path=pdf_path)

    # Verify that the text processing functions were called
    mock_extract_text.assert_called_once_with(pdf_path)
    mock_clean_text.assert_called_once_with(raw_text)
    
    # Verify that an agent was created for each property group
    assert mock_create_agent.call_count == 2
    
    # Verify the agent's run method was called for each group
    assert mock_agent.run.call_count == 2

    # Verify the content of the agent calls
    first_call_args = mock_agent.run.call_args_list[0]
    second_call_args = mock_agent.run.call_args_list[1]
    
    group1_template = to_json_template(settings.property_groups[0]["properties"])
    group2_template = to_json_template(settings.property_groups[1]["properties"])

    expected_message_1 = f"Text: '{cleaned_text}'\n\nJSON Template: {json.dumps(group1_template, indent=2)}\n\n"
    expected_message_2 = f"Text: '{cleaned_text}'\n\nJSON Template: {json.dumps(group2_template, indent=2)}\n\n"
    
    assert first_call_args.kwargs["message"] == expected_message_1
    assert second_call_args.kwargs["message"] == expected_message_2

    # Verify the final aggregated result
    assert isinstance(result, RunResponse)
    
    expected_data = {"document_title": "Sample Invoice", "isin": "US1234567890"}
    assert result.content == expected_data

def test_to_json_template():
    properties = [
        {"name": "document_title", "description": "Extract the document title"},
        {"name": "isin", "description": "Extract the ISIN"}
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
        {"name": "document_type", "description": "Classify the document type"}
    ]
    expected_template = {
        "document_type": "Classify the document type"
    }
    assert to_json_template(properties) == expected_template 