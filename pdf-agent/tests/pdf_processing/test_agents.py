from unittest.mock import MagicMock
import json
from agno.agent import Agent
from src.pdf_processing.agents import create_property_extraction_agent

def test_create_property_extraction_agent(mock_chat_model: MagicMock):
    """Tests if the factory function creates and configures the agent correctly."""
    agent = create_property_extraction_agent(model=mock_chat_model)
    assert isinstance(agent, Agent)
    assert agent.model is mock_chat_model
    
    assert "You are a hyper-specialized financial data extraction engine." in agent.instructions
    assert "**Critical Directives:**" in agent.instructions

def test_property_extraction_agent_run(mock_chat_model: MagicMock):
    """
    Tests the run method of the created agent, ensuring it calls the model
    with the correctly formatted message.
    """
    cleaned_text = "Some financial text containing property1 and property2."
    json_template = {"prop1": "description1", "prop2": "description2"}
    
    # Expected message format that the agent will receive
    expected_message = (
        f"Text: '{cleaned_text}'\n\n"
        f"JSON Template: {json.dumps(json_template, indent=2)}\n\n"
    )

    # Mock the response from the agent's run
    mock_chat_model.response.return_value.content = '{"prop1": "value1", "prop2": "value2"}'

    agent = create_property_extraction_agent(model=mock_chat_model)

    run_response = agent.run(message=expected_message)

    # Verify the agent's model was called with the correct message
    mock_chat_model.response.assert_called_once()
    called_messages = mock_chat_model.response.call_args.kwargs['messages']
    # The user message should be the last one in the list
    assert called_messages[-1].content == expected_message
    
    assert run_response.content == '{"prop1": "value1", "prop2": "value2"}' 