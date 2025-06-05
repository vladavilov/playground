## Implementation Task Breakdown: PDF Structured Product Data Extraction Microservice

### Phase 1: Project Setup & Core Infrastructure

1.  [x] **Task 1.1: Environment Setup & Dependency Management**
    *   Description: Set up the Python development environment. Initialize a `requirements.txt` (or `pyproject.toml` with Poetry/PDM) with core dependencies (Python, FastAPI, Uvicorn, AGNO, PyMuPDF, ftfy, nltk, Langchain).
    *   TRD Ref: Section 3.1 (Technical Implementation details specific libraries), Section 4 (Dockerization implies Python environment).
    *   Considerations: Version pinning for reproducibility.
    *   NFRs Addressed: Part of basic project setup.

2.  [x] **Task 1.2: Basic FastAPI Application Shell**
    *   Description: Create the initial FastAPI application structure (e.g., `main.py`). Implement a basic health check endpoint.
    *   TRD Ref: Section 3.0 (REST API Layer, FastAPI framework).
    *   Considerations: Standard project layout for FastAPI.
    *   NFRs Addressed: Scalability (foundation for Uvicorn).

3.  [x] **Task 1.3: Configuration Management Setup**
    *   Description: Implement the mechanism for loading external configurations (e.g., Python config files, environment variables, YAML for property groups). Define initial placeholders for Azure OpenAI settings, API port, temporary file directory.
    *   TRD Ref: Section 4 (Configuration - specific items like Azure OpenAI settings, property group definitions YAML, API service settings, temporary file directory path).
    *   Considerations: Secure handling of secrets (e.g., API keys using environment variables or a secrets management tool).
    *   NFRs Addressed: Configuration.

4.  [x] **Task 1.4: Logging Setup**
    *   Description: Integrate a logging framework (e.g., Python's `logging` module, potentially augmented by AGNO's logging utilities as mentioned in Section 4). Configure basic logging for different levels (INFO, DEBUG, ERROR).
    *   TRD Ref: Section 4 (Logging - comprehensive logging, AGNO's logging utilities).
    *   Considerations: Consistent log format, structured logging if beneficial for later analysis.
    *   NFRs Addressed: Logging.

5.  [x] **Task 1.5: Initial Dockerfile Creation**
    *   Description: Create a basic `Dockerfile` to package the FastAPI application. Ensure it can build and run the application shell.
    *   TRD Ref: Section 4 (Dockerization - `Dockerfile` creation, configurable via environment variables, CMD definition).
    *   Considerations: Use an official Python base image, manage dependencies.
    *   NFRs Addressed: Dockerization.

### Phase 2: PDF Processing & Text Preprocessing

1.  [x] **Task 2.1: PDF Text Extraction Module**
    *   Description: Develop functions to extract raw text from PDF files using `PyMuPDF` (Fitz).
    *   TRD Ref: Section 3.1 (PDF Processing & Text Preprocessing/Chunking, specifically `PyMuPDF`).
    *   Input: PDF file path.
    *   Output: Raw extracted text.
    *   Error Handling: Implement robust error handling for unparseable or problematic PDFs.
    *   NFRs Addressed: Accuracy (initial step), Error Handling.

2.  **Task 2.2: Text Cleaning Module - Basic Cleaning**
    *   Description: Implement functions for basic text cleaning as per Section 3.1:
        *   Remove excessive whitespace (multiple spaces, tabs).
        *   Normalize newlines (3+ to 2).
        *   Strip leading/trailing whitespace (entire text and per line).
        *   Normalize Unicode characters (NFKC).
        *   Handle hyphenation (rejoin hyphenated words).
        *   Fix text inconsistencies using `ftfy`.
    *   TRD Ref: Section 3.1 (Text Cleaning Requirements - detailed list).
    *   Considerations: Test with diverse PDF text outputs.
    *   NFRs Addressed: Accuracy.

3.  **Task 2.3: Text Cleaning Module - Advanced Cleaning (NLTK)**
    *   Description: Implement sentence tokenization using NLTK's `sent_tokenize` after initial cleaning.
    *   TRD Ref: Section 3.1 (Advanced Cleaning - Sentence Tokenization using NLTK).
    *   Considerations: Download NLTK resources (`punkt`) if necessary (document this in setup).
    *   NFRs Addressed: Accuracy (improving semantic coherence).

4.  **Task 2.4: Text Chunking Module (LangChain)**
    *   Description: Implement text chunking using `langchain_text_splitters.RecursiveCharacterTextSplitter`.
    *   TRD Ref: Section 3.1 (Text Chunking - `RecursiveCharacterTextSplitter`, configuration details).
    *   Configuration: Make `chunk_size`, `chunk_overlap`, and `separators` configurable as per Section 3.1.
    *   Input: Cleaned text (potentially list of sentences).
    *   Output: List of text chunks.
    *   NFRs Addressed: Accuracy (RAG effectiveness).

### Phase 3: AGNO Agent & Workflow Development

1.  **Task 3.1: `PropertyExtractionAgent` Definition**
    *   Description: Define the `PropertyExtractionAgent` (`agno.agent.Agent`) as outlined in Section 3.2.
    *   TRD Ref: Section 3.2 (AGNO Agent Definition (`PropertyExtractionAgent`)).
    *   Configuration: Configure with an AGNO model integration for Azure OpenAI (e.g., `agno.models.openai.OpenAIChat`).
    *   Core Task: Design the base instructions for its role (financial data extractor, JSON output). The agent will receive dynamic context and property lists per `run` call.
    *   NFRs Addressed: Accuracy (core extraction logic).

2.  **Task 3.2: `PDFExtractionWorkflow` - Core Orchestration Setup**
    *   Description: Define the `PDFExtractionWorkflow` class inheriting from `agno.workflow.Workflow`. Implement the initial structure to receive a PDF file path.
    *   TRD Ref: Section 3.2 (AGNO Workflow Definition (`PDFExtractionWorkflow(agno.workflow.Workflow)`)).
    *   NFRs Addressed: Foundation for core logic.

3.  **Task 3.3: `PDFExtractionWorkflow` - PDF Processing Integration**
    *   Description: Integrate the PDF processing and chunking modules (from Phase 2) into the workflow. The workflow should call these modules to get text chunks from the input PDF path.
    *   TRD Ref: Section 3.2 (Core Responsibilities item 2: Invoke PDF processing and chunking functions from Section 3.1).
    *   Error Handling: Propagate errors from PDF processing.
    *   NFRs Addressed: Orchestration as per Section 3.3.

4.  **Task 3.4: `PDFExtractionWorkflow` - Vector Embedding & Storage**
    *   Description: Implement logic within the workflow to:
        *   Create vector embeddings for text chunks (e.g., using `agno.models.openai.OpenAIEmbedder` with `text-embedding-ada-002`).
        *   Set up and manage an in-memory vector store (e.g., `FAISS`).
    *   TRD Ref: Section 3.2 (Core Responsibilities item 3: Manage vector embeddings and in-memory vector store).
    *   Configuration: Azure OpenAI embedding model details.
    *   NFRs Addressed: Accuracy (RAG).

5.  **Task 3.5: `PDFExtractionWorkflow` - Property Group Iteration & RAG**
    *   Description: Implement the loop to iterate through predefined logical groups of properties (this will require a placeholder or sample configuration for property groups from Section 5). For each group:
        *   Retrieve the hardcoded RAG query string.
        *   Embed the query.
        *   Use the vector store to retrieve relevant text chunks (Top-K).
    *   TRD Ref: Section 3.2 (Core Responsibilities item 4, 5a, 5b: Iterate groups, retrieve RAG query, embed query, retrieve chunks). Also relevant: Section 5 (Data Requirements - Property Grouping Definition & RAG Queries).
    *   Configuration: Property group definitions (YAML file structure from Section 4).
    *   NFRs Addressed: Accuracy (RAG).

6.  **Task 3.6: `PDFExtractionWorkflow` - Agent Invocation**
    *   Description: Within the property group loop, invoke the `PropertyExtractionAgent` with the RAG context and the list of properties for the current group.
    *   TRD Ref: Section 3.2 (Core Responsibilities item 5c: Invoke the `PropertyExtractionAgent`).
    *   NFRs Addressed: Accuracy (extraction).

7.  **Task 3.7: `PDFExtractionWorkflow` - JSON Validation Stub**
    *   Description: Implement a placeholder for JSON validation against predefined rules (Section 5). Initially, this might just check for valid JSON structure.
    *   TRD Ref: Section 3.2 (Core Responsibilities item 5e: Validate the JSON against predefined rules). Also relevant: Section 5 (Data Requirements - Validation Rules).
    *   Considerations: The actual validation rules will come from stakeholders.
    *   NFRs Addressed: Accuracy.

8.  **Task 3.8: `PDFExtractionWorkflow` - Refinement Loop Stub**
    *   Description: Implement a placeholder for the refinement loop (max 3 attempts). This will involve constructing a refinement prompt and re-invoking the agent.
    *   TRD Ref: Section 3.2 (Core Responsibilities item 5f: Initiate a refinement loop if validation fails).
    *   Considerations: The logic for constructing effective refinement prompts will be complex.
    *   NFRs Addressed: Accuracy.

9.  **Task 3.9: `PDFExtractionWorkflow` - Aggregation & Return**
    *   Description: Implement logic to aggregate JSON results from all groups and return the final aggregated JSON.
    *   TRD Ref: Section 3.2 (Core Responsibilities item 6 & 7: Aggregate JSON results and return final data).
    *   NFRs Addressed: Accuracy.

### Phase 4: REST API Layer Implementation

1.  **Task 4.1: `/extract_data` Endpoint Implementation**
    *   Description: Implement the `POST` endpoint `/extract_data` in the FastAPI application.
        *   Accept `multipart/form-data` with one PDF file.
        *   Save the uploaded PDF to a temporary, secure file path (configurable).
    *   TRD Ref: Section 3.0 (Endpoint: `POST /extract_data`, Request: `multipart/form-data`, Responsibilities item 1 & 2).
    *   NFRs Addressed: Security, Configuration.

2.  **Task 4.2: Workflow Invocation from API**
    *   Description: In the `/extract_data` endpoint handler, instantiate and execute the `PDFExtractionWorkflow` with the temporary file path.
    *   TRD Ref: Section 3.0 (Responsibilities item 3: Invoke the AGNO `PDFExtractionWorkflow`), Section 3.3 (Orchestration).
    *   NFRs Addressed: Orchestration as per Section 3.3.

3.  **Task 4.3: API Response Handling**
    *   Description: Receive the aggregated JSON from the workflow and return it as `application/json` with HTTP 200 on success. Implement error response handling (HTTP 4xx/5xx with JSON error messages).
    *   TRD Ref: Section 3.0 (Response (Success - HTTP 200), Response (Error - HTTP 4xx/5xx), Responsibilities item 4 & 5).
    *   NFRs Addressed: Error Handling.

4.  **Task 4.4: Temporary File Cleanup**
    *   Description: Ensure the temporary PDF file is deleted after processing (whether successful or failed) in a `finally` block or equivalent.
    *   TRD Ref: Section 3.0 (Responsibilities item 6: Ensure temporary PDF file is deleted). Also relevant: Section 4 (Security: Temporary file storage shall be secure, and files shall be deleted promptly).
    *   NFRs Addressed: Security, Logging.

5.  **Task 4.5: API Layer Logging**
    *   Description: Implement specific logging for the API layer: file reception, processing initiation, completion/failure, and deletion.
    *   TRD Ref: Section 3.0 (Responsibilities item 7: Log file reception, processing initiation, completion (or failure), and deletion). Also relevant: Section 4 (Logging).
    *   NFRs Addressed: Logging.

### Phase 5: Full Integration, Testing & NFR Fulfillment

1.  **Task 5.1: Detailed JSON Validation Implementation**
    *   Description: Once validation rules for each property are provided (Section 5), implement the detailed JSON validation logic within the `PDFExtractionWorkflow`.
    *   TRD Ref: Section 3.2 (Core Responsibilities item 5e), Section 5 (Data Requirements - Validation Rules).
    *   Considerations: Use a robust JSON schema validation library if appropriate.
    *   NFRs Addressed: Accuracy.

2.  **Task 5.2: Refinement Loop Implementation (Full)**
    *   Description: Fully implement the refinement loop in the `PDFExtractionWorkflow`, including constructing effective feedback prompts for the `PropertyExtractionAgent`.
    *   TRD Ref: Section 3.2 (Core Responsibilities item 5f).
    *   Considerations: This will likely require iterative testing and prompt engineering.
    *   NFRs Addressed: Accuracy.

3.  **Task 5.3: End-to-End Testing with Sample PDFs**
    *   Description: Conduct thorough end-to-end testing using the sample PDFs provided by stakeholders. Verify extraction accuracy against gold standard data (if available).
    *   TRD Ref: Section 4 (Accuracy NFR), Section 5 (Data Requirements - Sample PDFs, Gold Standard Data).
    *   NFRs Addressed: Accuracy.

4.  **Task 5.4: Security Hardening**
    *   Description: Review and implement security best practices:
        *   Validate uploaded files (e.g., file size limits, potentially basic type checking).
        *   Ensure secure temporary file handling.
        *   Review dependencies for vulnerabilities.
    *   TRD Ref: Section 4 (Security - Temporary file storage, input validation for uploaded files).
    *   NFRs Addressed: Security.

5.  **Task 5.5: Scalability Configuration (Uvicorn)**
    *   Description: Configure Uvicorn with an appropriate number of workers for concurrent request handling. Test basic concurrency.
    *   TRD Ref: Section 4 (Scalability (Basic) - deployable with an ASGI server like Uvicorn with workers).
    *   NFRs Addressed: Scalability.

6.  **Task 5.6: Dockerfile Finalization & Optimization**
    *   Description: Finalize the `Dockerfile`. Ensure it's optimized for size and build speed. Include the `CMD` to run the service with Uvicorn. Make it configurable via environment variables.
    *   TRD Ref: Section 4 (Dockerization - all sub-points).
    *   NFRs Addressed: Dockerization.

7.  **Task 5.7: Comprehensive Logging Review**
    *   Description: Review all logging statements for clarity, completeness, and appropriateness. Ensure AGNO logging is effectively used.
    *   TRD Ref: Section 4 (Logging).
    *   NFRs Addressed: Logging.

8.  **Task 5.8: Error Handling Review & Refinement**
    *   Description: Test various error scenarios (invalid PDF, API errors, workflow failures) and ensure robust error handling and reporting.
    *   TRD Ref: Section 4 (Error Handling - implement robust error handling, return appropriate HTTP error responses).
    *   NFRs Addressed: Error Handling.

### Phase 6: Documentation & Deployment Preparation

1.  **Task 6.1: Technical Documentation**
    *   Description: Document the setup, configuration, API usage, and internal architecture of the microservice.
    *   TRD Ref: General good practice, implied by the level of detail in the TRD itself.
    *   Considerations: README, API documentation (FastAPI can auto-generate some).

2.  **Task 6.2: Deployment Scripts/Guides**
    *   Description: Prepare scripts or a guide for deploying the Dockerized microservice.
    *   TRD Ref: Related to Section 4 (Dockerization).
    *   NFRs Addressed: Deployment.

--- 