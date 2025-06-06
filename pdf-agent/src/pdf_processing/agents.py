from agno.agent import Agent
from agno.models.openai import OpenAIChat
from typing import Optional, Dict, Any


def create_property_extraction_agent(
    model: OpenAIChat, session_state: Optional[Dict[str, Any]] = None
) -> Agent:
    """
    Creates and configures an Agno Agent for property extraction.

    This agent is designed to extract a specific list of properties from a
    given text context (RAG context) and return them in a JSON format.

    Args:
        model: An instance of an AGNO-compatible model, e.g., OpenAIChat.
        session_state: Optional initial session state for the agent.

    Returns:
        A configured instance of agno.agent.Agent.
    """
    instructions = (
        "You are an expert financial data extractor. "
        "Your task is to extract specific properties from the provided text context. "
        "You must return the extracted data in a valid JSON format. "
        "Do not include any explanatory text or markdown formatting around the JSON.\n\n"
        "Please extract the values for these properties: {properties}\n\n"
        "Context:\n---\n{rag_context}\n---"
    )

    agent = Agent(
        model=model,
        instructions=instructions,
        session_state=session_state if session_state is not None else {},
        add_state_in_messages=True,
    )
    return agent 