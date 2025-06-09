from unittest.mock import MagicMock
from agno.agent import Agent
from src.pdf_processing.agents import create_property_extraction_agent

def test_create_property_extraction_agent(mock_chat_model: MagicMock):
    """Tests if the factory function creates and configures the agent correctly."""
    agent = create_property_extraction_agent(model=mock_chat_model)
    assert isinstance(agent, Agent)
    assert agent.model is mock_chat_model
    
    # The instructions are a tuple, so join them for the check
    instruction_string = "".join(agent.instructions)
    assert "You are an expert at extracting information" in instruction_string
    assert "--- EXAMPLE START ---" in instruction_string

def test_property_extraction_agent_run_with_context(mock_chat_model: MagicMock):
    """
    Tests the run method of the created agent, ensuring it calls model.response()
    with the correctly formatted messages.
    """
    rag_context = "Some financial text containing property1 and property2."
    json_template = {"prop1": "Extract property 1", "prop2": "Extract property 2"}
    user_prompt = "Extract data based on the provided context and properties."

    # Mock the response from the agent's run
    mock_chat_model.response.return_value.content = '{"prop1": "value1", "prop2": "value2"}'

    agent = create_property_extraction_agent(
        model=mock_chat_model,
        session_state={
            "rag_context": rag_context,
            "json_template": json_template,
        },
    )

    # Verify the session state was set correctly
    assert agent.session_state["rag_context"] == rag_context
    assert agent.session_state["json_template"] == json_template

    run_response = agent.run(message=user_prompt)

    # Verify the agent was called and returned the mocked response
    mock_chat_model.response.assert_called_once()
    assert run_response.content == '{"prop1": "value1", "prop2": "value2"}' 