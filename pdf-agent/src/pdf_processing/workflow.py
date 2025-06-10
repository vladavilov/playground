"""
Defines the core PDF extraction workflow using the AGNO framework.
"""
import json
import re
from agno.workflow import Workflow, RunResponse
from agno.models.azure.ai_foundry import AzureAIFoundry
from src.config import get_settings
from .extractor import extract_text_from_pdf
from .cleaner import clean_text
from .agents import create_property_extraction_agent

def to_json_template(properties: list[dict]) -> dict:
    """
    Converts a list of property dictionaries to a JSON template for extraction.
    """
    return {prop["name"]: prop["description"] for prop in properties}

class PDFExtractionWorkflow(Workflow):
    """
    Orchestrates the entire data extraction process from a PDF file.

    This workflow is responsible for taking a PDF file, extracting its text,
    cleaning the text, and then using an AI agent to extract structured
    data from the full text content.

    The `run` method executes the main logic of the workflow and returns a
    single JSON object containing the aggregated extracted data.

    This workflow manages the process of receiving a PDF file path, invoking
    text processing, and coordinating with an AGNO Agent to extract structured data.
    """
    def __init__(self):
        super().__init__()
        
        settings = get_settings()
        # Initialize the chat model. The agent will be created on-the-fly for each run.
        self.chat_model = AzureAIFoundry(
            id=settings.AZURE_OPENAI_LLM_MODEL_NAME,
            api_key=settings.AZURE_OPENAI_API_KEY,
            azure_endpoint=settings.AZURE_OPENAI_CHAT_ENDPOINT,
            temperature=0.0,
        )
        self.property_groups = settings.property_groups
        self.max_retries = 3

    def run(self, pdf_file_path: str) -> RunResponse:
        """
        Executes the main logic of the workflow for a given PDF file.

        Returns:
            A RunResponse containing the aggregated extracted data as a dictionary.
        """
        raw_text = extract_text_from_pdf(pdf_file_path)
        cleaned_text = clean_text(raw_text)
        aggregated_data = {}

        if cleaned_text:
            for group in self.property_groups:
                properties_to_extract = group.get("properties", [])
                if not properties_to_extract:
                    continue

                json_template = to_json_template(properties_to_extract)

                agent = create_property_extraction_agent(model=self.chat_model)

                message = (
                    f"Text: '{cleaned_text}'\n\n"
                    f"JSON Template: {json.dumps(json_template, indent=2)}\n\n"
                )

                agent_response = agent.run(message=message)
                
                extracted_data = {}
                if agent_response.content:
                    for attempt in range(self.max_retries):
                        parsed_data, validation_errors = self._validate_agent_output(
                            agent_response.content, properties_to_extract
                        )

                        if not validation_errors:
                            extracted_data = parsed_data
                            break
                        
                        if attempt < self.max_retries - 1:
                            # Construct a refinement prompt with specific errors
                            error_list = "; ".join(validation_errors)
                            refinement_message = (
                                f"The previous JSON output was invalid. Please correct it. "
                                f"Errors: {error_list}"
                            )
                            agent_response = agent.run(message=refinement_message)
                        else:
                            error_msg = f"Invalid JSON after multiple retries: {validation_errors}"
                            extracted_data = {"error": error_msg}

                if "error" not in extracted_data:
                    aggregated_data.update(extracted_data)
                    
        return RunResponse(content=aggregated_data)

    def _validate_agent_output(
        self, response_content: str, properties_to_extract: list[dict]
    ) -> tuple[dict | None, list[str]]:
        """
        Validates the agent's JSON output against a set of rules.

        This method checks for:
        1.  Valid JSON structure.
        2.  Presence of all required properties.
        3.  Absence of unexpected properties.
        4.  Compliance with 'enum' and 'regex' validation rules.

        Args:
            response_content: The string output from the agent, expected to be JSON.
            properties_to_extract: The list of property definitions, including
                                   their validation rules.

        Returns:
            A tuple containing:
            - The parsed data as a dictionary if JSON is valid, otherwise None.
            - A list of string descriptions of any validation errors found.
              An empty list indicates successful validation.
        """
        errors = []
        try:
            data = json.loads(response_content)
        except json.JSONDecodeError:
            return None, ["Invalid JSON structure."]

        expected_prop_names = {prop["name"] for prop in properties_to_extract}
        provided_prop_names = set(data.keys())

        # Check for missing required properties
        for prop in properties_to_extract:
            prop_name = prop["name"]
            is_required = prop.get("validation", {}).get("required", False)
            if is_required and prop_name not in provided_prop_names:
                errors.append(f"Missing required property: '{prop_name}'")

        # Check for unexpected properties
        for prop_name in provided_prop_names:
            if prop_name not in expected_prop_names:
                errors.append(f"Unexpected property: '{prop_name}'")

        # Perform per-property validation
        for prop in properties_to_extract:
            prop_name = prop["name"]
            if prop_name not in data:
                continue

            value = data[prop_name]
            validation_rules = prop.get("validation", {})

            # Enum validation
            if "enum" in validation_rules and value not in validation_rules["enum"]:
                errors.append(
                    f"Value '{value}' for property '{prop_name}' is not in the allowed enum: {validation_rules['enum']}"
                )

            # Regex validation
            if "regex" in validation_rules:
                if not re.match(validation_rules["regex"], str(value)):
                    errors.append(
                        f"Value '{value}' for property '{prop_name}' does not match regex pattern: '{validation_rules['regex']}'"
                    )
        
        return data, errors