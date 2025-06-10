# Technical Requirements Document: PDF Structured Product Data Extraction Microservice

**Version:** 0.8
**Date:** 2024-07-28

## 1. Introduction

This document outlines the technical requirements for a microservice that parses PDF documents containing structured financial product information. The microservice shall accept a PDF file via an HTTP request, extract a predefined list of over 300 properties (organized into logical groups), and respond with a JSON object containing the extracted data.
The system shall be implemented using the AGNO framework, with an AGNO `Workflow` orchestrating an AGNO `Agent` for the core data extraction task. The microservice shall be deployable via Docker.

## 2. System Overview

The microservice shall expose a REST API endpoint. Upon receiving a PDF file, the following internal process, orchestrated by an AGNO `Workflow`, shall occur:

1.  **File Ingestion & Temporary Storage:** The API layer shall receive the PDF file, save it to a temporary location, and log this event.
2.  **Core Data Extraction (AGNO Workflow & Agent):** The AGNO `Workflow` shall be invoked with the temporary file path.
    a.  The Workflow shall manage PDF text extraction and cleaning (see Section 3.1).
    b.  The Workflow shall iterate through predefined logical groups of properties.
    c.  For each group, the Workflow shall invoke an AGNO `Agent` with the full, cleaned text from the PDF to extract all properties for that group into a JSON structure.
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

### 3.1. PDF Processing & Text Preprocessing (Functions/Modules)

*   **Description:** This component, invoked by the AGNO Workflow, transforms a PDF file (from a given temporary path) into cleaned text.
*   **Input:** A PDF file path.
*   **Output:** A single string of cleaned text.
*   **Technical Implementation (Python functions/modules):**
    *   Libraries: `PyMuPDF` (Fitz), `ftfy`.
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
    *   **Error Handling:** Functions shall raise exceptions or return error indicators for PDFs that cannot be parsed, to be handled by the AGNO Workflow.

### 3.2. AGNO-Based AI Data Extraction Workflow and Agent

*   **Description:** An AGNO `Workflow` (`agno.workflow.Workflow`) shall orchestrate the entire data extraction process, utilizing one AGNO `Agent` (`agno.agent.Agent`) instance for LLM interactions.
*   **AGNO Workflow Definition (`PDFExtractionWorkflow(agno.workflow.Workflow)`):**
    *   **Core Responsibilities:**
        1.  Receive the temporary PDF file path from the API layer.
        2.  Invoke PDF processing and cleaning functions (as defined in Section 3.1) to obtain a single cleaned text string.
        3.  Iterate through predefined logical groups of properties (defined in a configuration accessible to the workflow).
        4.  For each property group:
            a.  Construct a JSON template for the properties within the group.
            b.  Invoke the `PropertyExtractionAgent` with the full cleaned text and the JSON template for the group.
            c.  Receive JSON output from the `PropertyExtractionAgent`.
            d.  **Validate Agent Output:** The workflow shall perform a multi-step validation of the agent's output. First, it validates that the output is a structurally correct JSON. If so, it then validates the content against rules defined in the configuration, including:
                *   **Schema Adherence:** Checks that all properties marked as `required` are present and that no unexpected properties are included.
                *   **Per-Property Rules:** Enforces specific rules for each property's value, such as matching a `regex` pattern or belonging to a predefined `enum` list.
            e.  If validation fails, initiate a refinement loop (max 3 attempts total for the group): construct a detailed refinement prompt that includes the specific validation errors (e.g., "Missing required property: 'isin_code'", "Value 'XYZ' does not match regex") to guide the `PropertyExtractionAgent`, re-invoke the agent, and re-validate.
            f.  Store the validated (or best-effort refined) JSON for the group.
        5.  Aggregate JSON results from all groups.
        6.  Return the final aggregated JSON data to the API layer.
    *   **AGNO Components:** The implementation shall utilize `agno.workflow.Workflow` for orchestration and `agno.agent.Agent` for LLM interactions, configured with an appropriate AGNO model integration (`agno.models.openai.OpenAIChat` for Azure OpenAI).

*   **AGNO Agent Definition (`PropertyExtractionAgent`):**
    *   An AGNO `Agent` shall be defined (e.g., as part of the Workflow).
    *   **Core Task:** Given a text context and specific instructions, the Agent shall interact with the configured LLM to produce a JSON string output.
    *   **Instructions:** The Agent's base instructions shall define its role as an expert in filling a JSON template based on provided text. Specific task details (the full text and the JSON template) shall be passed dynamically in the `run` call from the Workflow.

### 3.3. Orchestration

*   **Description:** The REST API endpoint handler (Section 3.0) shall instantiate and execute the AGNO `PDFExtractionWorkflow` upon receiving an API request.

## 4. Non-Functional Requirements

*   **Accuracy:** The system must achieve high precision in extracting data.
*   **Configuration:** The following shall be externalized (in Python config files or environment variables):
    *   Azure OpenAI settings for the chat model (API key, endpoint, model deployment name).
    *   Property group definitions (including property names and descriptions) **should be in separate .yaml file**.
    *   API service settings (e.g., port).
    *   Temporary file directory path.
*   **Logging:** Comprehensive logging shall be implemented for all major steps, including API requests, file handling, PDF processing, AGNO Agent invocations (input prompts/context, raw LLM responses), validation outcomes, refinement attempts, and errors. AGNO's logging utilities (e.g., `agno.utils.log.logger`) should be used where appropriate.
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
*   **Property Grouping Definition & Descriptions:** For each logical group:
    *   A list of the specific properties it contains.
    *   Each property must have a `description` string to guide the LLM.
*   **Sample PDFs:** 5-10 representative PDFs for development/testing.
*   **Validation Rules:** Specific criteria for each property (e.g., data type, `required` flag, `regex` pattern for format validation, a list of allowed values for an `enum`).
*   **Gold Standard Data (Highly Recommended):** Manually extracted, validated JSON for sample PDFs.

## 6. Assumptions

*   The system is a microservice accessed via REST API.
*   The system processes one PDF file per API request; concurrent requests result in parallel, independent processing instances (each creating its own temporary file).
*   The AGNO framework (compatible with `agno.workflow.Workflow`, `agno.agent.Agent`) is the primary tool for AI agent definition and workflow orchestration.
*   All input PDFs contain selectable text.
*   Azure OpenAI Service access for the chat model is configured and available.
*   The embedding model is loaded from a local path.
*   Field lists, groupings, property descriptions, and validation rules are managed via externalized configuration.

---