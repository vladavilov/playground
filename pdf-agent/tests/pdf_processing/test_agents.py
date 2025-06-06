from unittest.mock import MagicMock
from agno.agent import Agent
from src.pdf_processing.agents import create_property_extraction_agent

def test_create_property_extraction_agent(mock_chat_model: MagicMock):
    """Tests if the factory function creates and configures the agent correctly."""
    agent = create_property_extraction_agent(model=mock_chat_model)
    assert isinstance(agent, Agent)
    assert agent.model is mock_chat_model
    assert "You are an expert financial data extractor" in agent.instructions

def test_property_extraction_agent_run_with_context(mock_chat_model: MagicMock):
    """
    Tests the run method of the created agent, ensuring it calls model.response()
    with the correctly formatted messages.
    """
    rag_context = "Some financial text containing property1 and property2."
    properties = ["property1", "property2"]
    user_prompt = "Extract data."
    
    agent = create_property_extraction_agent(
        model=mock_chat_model,
        session_state={
            "rag_context": rag_context,
            "properties": properties,
        },
    )
    
    run_response = agent.run(message=user_prompt)
    
    mock_chat_model.response.assert_called_once()
    call_kwargs = mock_chat_model.response.call_args.kwargs
    
    messages_list = call_kwargs['messages']
    # The rendered instructions become the system prompt (the first message).
    system_prompt_content = messages_list[0].content

    # The user's direct message is the last message.
    user_prompt_content = messages_list[-1].content

    assert rag_context in system_prompt_content
    assert str(properties) in system_prompt_content
    assert user_prompt_content == user_prompt
    assert run_response.content == '{"property1": "value1", "property2": "value2"}' 