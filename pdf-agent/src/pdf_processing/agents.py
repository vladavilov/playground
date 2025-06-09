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
        session_state: Optional initial session state for the agent, which must
                       contain 'rag_context' and 'json_template'.

    Returns:
        A configured instance of agno.agent.Agent.
    """
    instructions = (
        "You are an expert at extracting information from text and filling in a JSON template. "
        "Given a text and a JSON template, you must extract the required information from the text "
        "and fill in the corresponding values in the JSON template. You must return only a valid JSON object, no explanations, no yapping.\n\n"
        "--- EXAMPLE START ---\n"
        "**Example input:**",
        "Text: 'The ISIN for the new bond is US1234567890. The title of the document is `Prospectus`.`\n\n"
        "JSON Template: {'isin': '<Extract the ISIN>', 'document_title': '<Extract the document title>'}\n\n"
        "**Example output:**",
        "{\n"
        "  \"isin\": \"US1234567890\",\n"
        "  \"document_title\": \"Prospectus\"\n"
        "}\n"
        "--- EXAMPLE END ---\n\n"
        "--- TASK START ---\n"
        "Text: '{rag_context}'\n\n"
        "JSON Template: {json_template}\n\n"
    )

    agent = Agent(
        model=model,
        instructions=instructions,
        session_state=session_state if session_state is not None else {},
        add_state_in_messages=True,
    )
    return agent 