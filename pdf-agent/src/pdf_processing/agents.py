from agno.agent import Agent
from agno.models.azure.ai_foundry import AzureAIFoundry
import textwrap


def create_property_extraction_agent(model: AzureAIFoundry) -> Agent:
    """
    Creates and configures an Agno Agent for property extraction.

    This agent is designed to extract a specific list of properties from a
    given text context and return them in a JSON format.

    Args:
        model: An instance of an AGNO-compatible model, e.g., AzureAIFoundry.

    Returns:
        A configured instance of agno.agent.Agent.
    """
    instructions = textwrap.dedent(
        """
        You are a hyper-specialized financial data extraction engine. Your sole function is to populate a JSON template using information from the provided financial document text.

        **Critical Directives:**
        1.  **Exactitude:** Extract values VERBATIM from the text. Do not infer, calculate, or create information.
        2.  **Strict JSON Output:** Your entire response MUST be a single, valid JSON object. Omit all other text, including markdown fences (```json) or commentary.
        3.  **Handle Missing Information:** If a value for a key cannot be found in the text, the value for that key MUST be `null`. Do not omit keys.
        4.  **Template Adherence:** The structure of your JSON output must exactly match the provided template.

        ---

        **Example 1: Full Data**

        *Input:*
        Text: 'The new bond offering from Quantum Corp, identified by ISIN US1234567890, has been classified as a Senior Unsecured Note. The document title is "Official Prospectus - Series A Bonds".'

        JSON Template: {
          "document_title": "The title of the document",
          "isin": "The ISIN of the financial product",
          "instrument_type": "The type of financial instrument"
        }

        *Output:*
        {"document_title": "Official Prospectus - Series A Bonds", "isin": "US1234567890", "instrument_type": "Senior Unsecured Note"}

        ---

        **Example 2: Missing Data**

        *Input:*
        Text: 'This Key Information Document outlines the primary risks for the equity fund. The product is managed by Global Investments Ltd. The main risks include market volatility and liquidity risk.'

        JSON Template: {
          "document_type": "The classification of the document",
          "isin": "The ISIN of the financial product",
          "manager": "The name of the investment manager"
        }

        *Output:*
        {"document_type": "Key Information Document", "isin": null, "manager": "Global Investments Ltd."}
        """
    )

    agent = Agent(model=model, instructions=instructions)
    return agent 