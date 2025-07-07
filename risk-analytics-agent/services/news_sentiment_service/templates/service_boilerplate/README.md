# [Service Name] Microservice

---

## 1. Overview

This project is a boilerplate for creating new microservices within the **News Sentiment Service**. Its primary responsibility is to provide a standardized starting point for connecting to external data vendors, fetching raw data, and exposing it through a consistent REST API contract.

## 2. Architectural Role

[TODO: Add architectural diagram]

```mermaid
```

## 3. API Contract

[TODO: Add API contract]

## 4. Local Development

### 4.1. Installation

The project uses standard Python packaging.

1.  **Create a Virtual Environment**: From this service's root directory, run:
    ```bash
    python -m venv venv
    ```

2.  **Activate the Environment**:
    -   On Windows: `.\\venv\\Scripts\\activate`
    -   On macOS/Linux: `source venv/bin/activate`

3.  **Install Dependencies**: Once activated, install the required packages:
    ```bash
    pip install -e .[dev]
    ```

### 4.2. Configuration

The service uses a layered approach for configuration: `.env` file > environment variables > Azure App Configuration.

Create a `.env` file in the service's root directory:
```env
# .env

# The port the local FastAPI server will run on
API_PORT=8000

# (Optional) For connecting to a cloud-based dev environment
# AZURE_APPCONFIG_ENDPOINT="https://<your-app-config-name>.azconfig.io"
```

### 4.3. Running the Service

Ensure your virtual environment is activated. To start the local FastAPI server:
```bash
uvicorn src.main:app --host 0.0.0.0 --port 8000 --reload
```

### 4.4. Running Tests

Ensure your virtual environment is activated. To run the unit tests:
```bash
pytest
```
The tests are configured to automatically use the `src` directory for the python path. 