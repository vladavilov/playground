# PDF Structured Product Data Extraction Microservice

## Overview

This project is a FastAPI-based microservice designed to extract structured data from PDF documents. It leverages Azure's AI services for text extraction and analysis, using a RAG (Retrieval-Augmented Generation) pipeline to identify specific properties within the document's content.

The service is built to be scalable and is containerized using Docker for easy deployment and consistent environments.

## Features

-   **PDF Parsing**: Extracts raw text from uploaded PDF files.
-   **Text Cleaning**: Normalizes and cleans extracted text to improve accuracy.
-   **AI-Powered Extraction**: Uses an AI agent (powered by Azure OpenAI) with a RAG pipeline to intelligently extract data based on configurable property groups.
-   **Local Embeddings**: Generates vector embeddings using a self-hosted model (`BAAI/bge-small-en-v1.5`) for semantic search, ensuring data privacy and removing dependency on external embedding services.
-   **Dynamic Property Groups**: Extraction logic is defined in `config/property_groups.yaml`, allowing for easy customization without code changes.
-   **Data Validation**: Includes a robust, rule-based validation system for the extracted JSON data.
-   **RESTful API**: Provides a simple, secure endpoint for uploading PDFs and receiving structured JSON data.
-   **Dockerized**: Fully containerized for portability and scalable deployment.

## Property Group Configuration (`property_groups.yaml`)

The core of the extraction logic is driven by the `config/property_groups.yaml` file. This file allows you to define what data to look for without changing any Python code.

Each group consists of:
- `group_name`: A logical name for the category of data.
- `properties`: A list of the specific data points to extract for this group. Each property must contain its own `rag_query`.

### RAG (Retrieval-Augmented Generation) Logic
The service performs a RAG search for each individual property defined in a group using its specific `rag_query`. The text chunks retrieved from all searches within the group are then merged into a single context, which is passed to the AI agent to extract all properties for that group at once.

### Property Validation

For each property, you can specify a `validation` block to ensure data quality:

- `required` (boolean): If `true`, the extraction will fail if this property is not found.
- `enum` (list): Provides a list of allowed values for the property.
- `regex` (string): A regular expression pattern that the extracted value must match.

Example:
```yaml
- group_name: "General Information"
  properties:
    - name: "document_title"
      rag_query: "Extract the title of the document."
      validation:
        required: true
    - name: "isin"
      rag_query: "Extract the ISIN (International Securities Identification Number)."
      validation:
        required: true
        regex: "^[A-Z]{2}[A-Z0-9]{9}[0-9]{1}$"
```

## Environment Setup

### Prerequisites

-   Python 3.11+
-   Docker

### Local Development

1.  **Clone the Repository**

2.  **Create a Virtual Environment**
    ```bash
    python -m venv .venv
    source .venv/bin/activate  # On Windows, use `.venv\Scripts\activate`
    ```

3.  **Install Dependencies**
    ```bash
    pip install -r requirements.txt
    ```

4.  **Download NLTK Data**
    The service uses the NLTK library for sentence tokenization. This command needs to be run once.
    ```bash
    python scripts/download_nltk_data.py
    ```

5.  **Download Embedding Model**
    The service uses a local sentence transformer model. This command downloads the model to the `embedding_model` directory and needs to be run once.
    ```bash
    python scripts/download_model.py
    ```

6.  **Configure Environment Variables**
    Create a `.env` file in the project's root directory. The service requires Azure OpenAI credentials for the chat model to function. The embedding model runs locally and requires no keys.
    ```env
    # Azure OpenAI Settings
    AZURE_OPENAI_API_KEY="your_azure_api_key"
    
    # Chat Model (AzureAIFoundry)
    AZURE_OPENAI_CHAT_ENDPOINT="https://your-resource.openai.azure.com/"
    AZURE_OPENAI_LLM_MODEL_NAME="your_chat_deployment_name" # e.g., gpt-4o

    # Optional Overrides
    API_PORT=8000
    LOG_LEVEL="INFO"
    MAX_FILE_SIZE_MB=10
    ```

## Running the Service

### Locally

To run the application for local development with live reloading:
```bash
uvicorn src.main:app --reload
```
The API will be available at `http://localhost:8000`.

### With Docker

1.  **Build the Docker Image**
    ```bash
    docker build -t pdf-extractor-service .
    ```

2.  **Run the Docker Container**
    ```bash
    docker run -p 8000:8000 --env-file .env pdf-extractor-service
    ```
    To override the number of workers for better performance:
    ```bash
    docker run -p 8000:8000 --env-file .env -e WORKERS=4 pdf-extractor-service
    ```

- Notes: 
1.  `/scripts/rebuild_docker_image.sh` is a script that rebuilds and starts the docker image with dummy environment variables on localhost:8000.
2.  `/scripts/send_pdf.bat` is a script that sends a sample PDF file from 'scripts/tmp/sample.pdf' to the docker container on localhost:8000.

## API Usage

### `/extract_data`

-   **Method**: `POST`
-   **Description**: Upload a PDF file to extract structured data.
-   **Request**: `multipart/form-data`
    -   `file`: The PDF file to be processed.
-   **Responses**:
    -   `200 OK`: Successful extraction. The body contains the aggregated JSON data.
    -   `400 Bad Request`: Invalid file type (not `application/pdf`).
    -   `413 Request Entity Too Large`: File size exceeds the configured `MAX_FILE_SIZE_MB`.
    -   `500 Internal Server Error`: An unexpected error occurred during processing.

### Auto-Generated Documentation

Once the service is running, interactive API documentation (provided by Swagger UI and ReDoc) is available at:
-   **Swagger UI**: `http://localhost:8000/docs`
-   **ReDoc**: `http://localhost:8000/redoc`

## Running Tests

To run the full test suite:
```bash
pytest
```
