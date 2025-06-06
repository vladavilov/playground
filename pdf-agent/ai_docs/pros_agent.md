# Technical Requirements Document: PDF Structured Product Data Extraction Microservice

**Version:** 0.7
**Date:** 2024-07-28

## 1. Introduction

This document outlines the technical requirements for a microservice that parses PDF documents containing structured financial product information. The microservice shall accept a PDF file via an HTTP request, extract a predefined list of over 300 properties (organized into logical groups), and respond with a JSON object containing the extracted data.
The system shall be implemented using the AGNO framework, with an AGNO `Workflow` orchestrating an AGNO `Agent` for the core data extraction task, including Retrieval Augmented Generation (RAG). The microservice shall be deployable via Docker.

## 2. System Overview

The microservice shall expose a REST API endpoint. Upon receiving a PDF file, the following internal process, orchestrated by an AGNO `Workflow`, shall occur:

1.  **File Ingestion & Temporary Storage:** The API layer shall receive the PDF file, save it to a temporary location, and log this event.
2.  **Core Data Extraction (AGNO Workflow & Agent):** The AGNO `Workflow` shall be invoked with the temporary file path.
    a.  The Workflow shall manage PDF text extraction, preprocessing, and chunking (see Section 3.1).
    b.  The Workflow shall iterate through predefined logical groups of properties, managing the RAG process (embedding, retrieval) for each group to gather relevant context.
    c.  The Workflow shall invoke an AGNO `Agent` with the RAG context to extract properties for each group into a JSON structure.
    d.  The Workflow shall validate the agent's JSON output. If validation fails for any property, the Workflow shall re-invoke the agent with feedback for refinement (max 3 attempts per group extraction).
    e.  The Workflow shall aggregate the validated JSON data from all groups.
3.  **Response & Cleanup:** The aggregated JSON data from the AGNO Workflow shall be returned by the API layer as the HTTP response. The API layer shall then delete the temporary PDF file and log these actions.

## 3. Core Components & Technical Specifications

### 3.0. REST API Layer

*   **Description:** Provides a REST API for PDF ingestion and JSON data retrieval.
*   **Framework:** A Python web framework (FastAPI) shall be used.
*   **Endpoint:** A `POST` endpoint (`/extract_data`) shall be implemented.
    *   **Request:** Shall accept `multipart/form-data` containing one PDF file.
    *   **Response (Success - HTTP 200):** Shall return `application/json` with the aggregated extracted properties.
    *   **Response (Error - HTTP 4xx/5xx):** Shall return `application/json` with an error message.
*   **Responsibilities:**
    1.  Receive the PDF file via HTTP POST.
    2.  Save the uploaded PDF to a temporary file path.
    3.  Invoke the AGNO `PDFExtractionWorkflow` (see Section 3.2) with the temporary file path.
    4.  Receive the aggregated JSON result from the workflow.
    5.  Return the JSON result as the HTTP response.
    6.  Ensure the temporary PDF file is deleted after processing (successful or failed).
    7.  Log file reception, processing initiation, completion (or failure), and deletion.

### 3.1. PDF Processing & Text Preprocessing/Chunking (Functions/Modules)

*   **Description:** This component, invoked by the AGNO Workflow, transforms a PDF file (from a given temporary path) into cleaned, chunked text suitable for RAG.
*   **Input:** A PDF file path.
*   **Output:** A list of cleaned text chunks.
*   **Technical Implementation (Python functions/modules):**
    *   Libraries: `PyMuPDF` (Fitz), `ftfy`, `nltk`, `langchain_text_splitters.RecursiveCharacterTextSplitter`.
    *   **Text Cleaning Requirements:**
        *   **Remove Excessive Whitespace:**
            *   Multiple consecutive space characters shall be replaced with a single space.
                *   *Python Example:* `re.sub(r'  +', ' ', text)`
            *   Tab characters (`\\t`) shall be replaced with a single space.
                *   *Python Example:* `text.replace('\\t', ' ')`
            *   Sequences of three or more newline characters (`\\n`) shall be replaced with exactly two newlines.
                *   *Python Example:* `re.sub(r'\\n{3,}', '\\n\\n', text)`
            *   Leading/trailing whitespace (spaces, tabs, newlines) shall be removed from the beginning and end of the entire extracted text, and from each individual line after initial newline normalization.
                *   *Python Example (for entire text):* `text.strip()`
        *   **Normalize Unicode Characters:** Unicode characters shall be normalized using `NFKC` normalization to ensure consistency.
            *   *Python Example (`unicodedata`):* `unicodedata.normalize('NFKC', text)`
        *   **Handle Hyphenation:** Words broken across lines by a hyphen shall be rejoined.
            *   **Approach:** The system shall use regular expressions to identify and rejoin words hyphenated at the end of a line.
            *   *Python Example:* `re.sub(r'([a-zA-Z]+)-\\n([a-zA-Z]+)', r'\\1\\2', text)` (This pattern can be refined for more complex cases).
        *   **Fix Text Inconsistencies:** The `ftfy` library should be used to address common text issues like Mojibake and other Unicode inconsistencies.
            *   *Python Example:* `ftfy.fix_text(text)`
        *   **Advanced Cleaning (using `nltk`):**
            *   **Sentence Tokenization:** The system shall use NLTK's `sent_tokenize` to split extracted text into individual sentences prior to chunking.
                *   **Implementation Note:** After initial text extraction and basic cleaning, pass the text to `nltk.sent_tokenize()`.
            *   **Reasoning:** Sentence tokenization aims to improve the semantic coherence of text chunks for RAG.
    *   **Text Chunking (for RAG):**
        *   **Method:** The system shall use `RecursiveCharacterTextSplitter` from `LangChain`.
        *   **Configuration:**
            *   `chunk_size`: To be determined (e.g., 500-1000 characters/tokens), need to keep LLM context small enough, but rich enough to be useful.
            *   `chunk_overlap`: To be determined (e.g., 50-100 characters/tokens), need to keep context chunks joined for sequential processing.
            *   `separators`: Configured with common structural separators (`["\\n\\n", "\\n", ". ", " ", ""]`).
        *   **Reasoning:** `RecursiveCharacterTextSplitter` attempts to split text along semantic boundaries, which is beneficial for RAG.
    *   **Error Handling:** Functions shall raise exceptions or return error indicators for PDFs that cannot be parsed, to be handled by the AGNO Workflow.

### 3.2. AGNO-Based AI Data Extraction Workflow and Agent

*   **Description:** An AGNO `Workflow` (`agno.workflow.Workflow`) shall orchestrate the entire data extraction process, utilizing one AGNO `Agent` (`agno.agent.Agent`) instance for LLM interactions.
*   **AGNO Workflow Definition (`PDFExtractionWorkflow(agno.workflow.Workflow)`):**
    *   **Core Responsibilities:**
        1.  Receive the temporary PDF file path from the API layer.
        2.  Invoke PDF processing and chunking functions (as defined in Section 3.1) to obtain text chunks.
        3.  Manage the creation of vector embeddings for all text chunks (e.g., using a model like `text-embedding-ada-002` via AGNO's model integration, such as `agno.models.openai.OpenAIEmbedder`) and set up an in-memory vector store (using directly managed `FAISS`).
        4.  Iterate through predefined logical groups of properties (defined in a configuration accessible to the workflow).
        5.  For each property group:
            a.  Retrieve the hardcoded RAG query string for the group (from configuration).
            b.  Embed the query and use the vector store to retrieve relevant text chunks (Top-K).
            c.  Invoke the `PropertyExtractionAgent` with RAG context and the list of properties for the group.
            d.  Receive JSON output from the `PropertyExtractionAgent`.
            e.  **Validate Agent Output:** The workflow shall perform a multi-step validation of the agent's output. First, it validates that the output is a structurally correct JSON. If so, it then validates the content against rules defined in the configuration, including:
                *   **Schema Adherence:** Checks that all properties marked as `required` are present and that no unexpected properties are included.
                *   **Per-Property Rules:** Enforces specific rules for each property's value, such as matching a `regex` pattern or belonging to a predefined `enum` list.
            f.  If validation fails, initiate a refinement loop (max 3 attempts total for the group): construct a detailed refinement prompt that includes the specific validation errors (e.g., "Missing required property: 'isin_code'", "Value 'XYZ' does not match regex") to guide the `PropertyExtractionAgent`, re-invoke the agent, and re-validate.
            g.  Store the validated (or best-effort refined) JSON for the group.
        6.  Aggregate JSON results from all groups.
        7.  Return the final aggregated JSON data to the API layer.
    *   **AGNO Components:** The implementation shall utilize `agno.workflow.Workflow` for orchestration and `agno.agent.Agent` for LLM interactions, configured with an appropriate AGNO model integration (`agno.models.openai.OpenAIChat` for Azure OpenAI).

*   **AGNO Agent Definition (`PropertyExtractionAgent`):**
    *   An AGNO `Agent` shall be defined (e.g., as part of the Workflow).
    *   **Core Task:** Given a text context and specific instructions (e.g., list of properties to extract, or properties to refine with feedback), the Agent shall interact with the configured LLM to produce a JSON string output.
    *   **Instructions:** The Agent's base instructions shall define its role (e.g., financial data extractor) and JSON output requirement. Specific task details (context, property lists, refinement feedback) shall be passed dynamically in the `run` call from the Workflow.

### 3.3. Orchestration

*   **Description:** The REST API endpoint handler (Section 3.0) shall instantiate and execute the AGNO `PDFExtractionWorkflow` upon receiving an API request.

## 4. Non-Functional Requirements

*   **Accuracy:** The system must achieve high precision in extracting data.
*   **Configuration:** The following shall be externalized (in Python config files or environment variables):
    *   Azure OpenAI settings (API key, endpoint, model deployment names for LLM and embeddings).
    *   Property group definitions (including RAG queries and property lists) **should be in separate .yaml file**.
    *   API service settings (e.g., port).
    *   Temporary file directory path.
*   **Logging:** Comprehensive logging shall be implemented for all major steps, including API requests, file handling, PDF processing, RAG operations, AGNO Agent invocations (input prompts/context, raw LLM responses), validation outcomes, refinement attempts, and errors. AGNO's logging utilities (e.g., `agno.utils.log.logger`) should be used where appropriate.
*   **Error Handling:** The system (API layer and AGNO Workflow) shall implement robust error handling and return appropriate HTTP error responses or log failures clearly.
*   **Security:**
    *   Temporary file storage shall be secure, and files shall be deleted promptly after processing.
    *   Input validation for uploaded files (e.g., file size limits) shall be considered.
*   **Scalability (Basic):** The service shall be deployable with an ASGI server (e.g., Uvicorn with workers) to handle concurrent requests.
*   **Dockerization:**
    *   A `Dockerfile` shall be created to package the microservice, including Python environment, AGNO framework, dependencies, and application code.
    *   The Docker image shall be configurable via environment variables.
    *   The `Dockerfile` shall define how to run the service (e.g., `CMD ["uvicorn", "main:app"]`).

## 5. Data Requirements (To Be Provided by Stakeholders)

*   **Definitive List of All 300+ Fields:** A complete, precise list.
*   **Property Grouping Definition & RAG Queries:** For each logical group:
    *   A list of the specific properties it contains.
    *   A hardcoded, specific RAG query string for context retrieval.
    (This structure shall be maintained in a configuration source).
*   **Sample PDFs:** 5-10 representative PDFs for development/testing.
*   **Validation Rules:** Specific criteria for each property (e.g., data type, `required` flag, `regex` pattern for format validation, a list of allowed values for an `enum`).
*   **Gold Standard Data (Highly Recommended):** Manually extracted, validated JSON for sample PDFs.

## 6. Assumptions

*   The system is a microservice accessed via REST API.
*   The system processes one PDF file per API request; concurrent requests result in parallel, independent processing instances (each creating its own temporary file).
*   The AGNO framework (compatible with `agno.workflow.Workflow`, `agno.agent.Agent`) is the primary tool for AI agent definition and workflow orchestration.
*   All input PDFs contain selectable text.
*   Azure OpenAI Service access is configured and available.
*   Field lists, groupings, RAG queries, and validation rules are managed via externalized configuration.

---