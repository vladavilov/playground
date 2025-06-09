"""
Defines the core PDF extraction workflow using the AGNO framework.
"""
import json
import re
from agno.workflow import Workflow, RunResponse
from agno.embedder.sentence_transformer import SentenceTransformerEmbedder
from agno.models.azure.ai_foundry import AzureAIFoundry
import faiss
import numpy as np
from src.config import get_settings
from .extractor import extract_text_from_pdf
from .cleaner import clean_text, chunk_text
from typing import TypedDict
from .agents import create_property_extraction_agent

class PropertyGroupResult(TypedDict):
    """
    Represents the result of processing a single property group.
    """
    group_name: str
    rag_query: str
    relevant_chunks: list[str]
    extracted_data: dict

class PDFExtractionWorkflowResult(TypedDict):
    """
    Represents the structured result of the PDFExtractionWorkflow.
    """
    extracted_chunks: list[str]
    rag_results: list[PropertyGroupResult]
    aggregated_data: dict

def to_json_template(properties: list[dict]) -> dict:
    """
    Converts a list of property dictionaries to a JSON template for extraction.
    """
    return {prop["name"]: prop["rag_query"] for prop in properties}

class PDFExtractionWorkflow(Workflow):
    """
    Orchestrates the entire data extraction process from a PDF file.

    This workflow is responsible for taking a PDF file, extracting its text,
    cleaning and chunking the text, generating vector embeddings, and storing
    them in a FAISS in-memory vector store.

    The workflow is designed to be run as part of a larger data processing
    pipeline.

    The `run` method executes the main logic of the workflow, and the class
    is initialized with the path to the PDF file. The `SentenceTransformerEmbedder`

    is used to generate the embeddings, and the configuration for the
    embedder is automatically loaded from the environment variables.

    This workflow manages the process of receiving a PDF file path, invoking
    text processing and chunking, managing a RAG pipeline, and coordinating
    with an AGNO Agent to extract structured data.
    """
    def __init__(self):
        """
        Initializes the workflow and the SentenceTransformerEmbedder.
        """
        super().__init__()
        self.vector_store = None
        
        settings = get_settings()
        self.embedder = SentenceTransformerEmbedder(
            id=settings.EMBEDDING_MODEL_PATH
        )
        # Initialize the chat model. The agent will be created on-the-fly for each run.
        self.chat_model = AzureAIFoundry(
            id=settings.AZURE_OPENAI_LLM_MODEL_NAME,
            api_key=settings.AZURE_OPENAI_API_KEY,
            azure_endpoint=settings.AZURE_OPENAI_CHAT_ENDPOINT,
            temperature=0.0,
        )
        self.property_groups = settings.property_groups
        self.top_k = settings.TOP_K
        self.max_retries = 3

    def run(self, pdf_file_path: str) -> RunResponse:
        """
        Executes the main logic of the workflow for a given PDF file.
        """
        raw_text = extract_text_from_pdf(pdf_file_path)
        cleaned_text = clean_text(raw_text)
        chunks = chunk_text(cleaned_text)
        rag_results = []
        aggregated_data = {}

        if chunks:
            chunk_embeddings = [self.embedder.get_embedding(chunk) for chunk in chunks]
            self.vector_store = self._create_vector_store(chunk_embeddings)

            if self.vector_store:
                for group in self.property_groups:
                    properties_to_extract = group.get("properties", [])
                    if not properties_to_extract:
                        continue

                    all_relevant_indices = set()
                    for prop in properties_to_extract:
                        rag_query = prop.get("rag_query")
                        if rag_query:
                            query_embedding = self.embedder.get_embedding(rag_query)
                            effective_top_k = min(self.top_k, len(chunks))
                            relevant_indices = self._search_vector_store(
                                np.array([query_embedding]).astype("float32"), top_k=effective_top_k
                            )
                            all_relevant_indices.update(relevant_indices)
                    
                    if not all_relevant_indices:
                        continue

                    relevant_chunks = [chunks[i] for i in sorted(list(all_relevant_indices))]
                    
                    rag_context = "\\n\\n".join(relevant_chunks)
                    json_template = to_json_template(properties_to_extract)

                    agent = create_property_extraction_agent(
                        model=self.chat_model,
                        session_state={
                            "rag_context": rag_context,
                            "json_template": json_template,
                        },
                    )

                    agent_response = agent.run(
                        message="Extract the data based on the provided context and properties."
                    )
                    
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

                    rag_results.append(
                        PropertyGroupResult(
                            group_name=group.get("group_name"),
                            rag_query=rag_query,
                            relevant_chunks=relevant_chunks,
                            extracted_data=extracted_data,
                        )
                    )

            # Aggregate all successful extractions
            for result in rag_results:
                if "error" not in result["extracted_data"]:
                    aggregated_data.update(result["extracted_data"])
                    
        workflow_result = PDFExtractionWorkflowResult(
            extracted_chunks=chunks, 
            rag_results=rag_results,
            aggregated_data=aggregated_data,
        )

        return RunResponse(content=workflow_result)

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

    def _create_vector_store(self, embeddings: list[list[float]]):
        """
        Creates a FAISS vector store from the given embeddings.
        """
        if not embeddings:
            return None

        embeddings_np = np.array(embeddings).astype("float32")
        dimension = embeddings_np.shape[1]
        index = faiss.IndexFlatL2(dimension)
        index.add(embeddings_np)
        return index

    def _search_vector_store(
        self, query_embedding: np.ndarray, top_k: int
    ) -> list[int]:
        """
        Searches the vector store for the top_k most similar embeddings.
        """
        if self.vector_store is None:
            return []

        distances, indices = self.vector_store.search(query_embedding, top_k)
        
        # Filter out invalid indices (-1) which faiss can return 
        # if there are fewer vectors than top_k
        return [i for i in indices[0] if i != -1]