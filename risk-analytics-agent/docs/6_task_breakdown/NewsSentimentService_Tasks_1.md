## [x] **Epic 1: Core Infrastructure & Shared Components**

**Objective:** Establish the foundational, reusable elements required by all other services. This epic is the prerequisite for all subsequent development.

*   **[x] Task 1.1: Implement `common` Shared Library Pydantic Models**
    *   **Description:** In the `playground/risk-analytics-agent/src/news_sentiment_service/common/models/models.py` file, define the core Pydantic data models that will serve as the data contracts between services. These models must enforce data types and validation.
    *   **Implementation Details:**
        *   **`RawNewsArticle`**: Create a Pydantic model with fields as defined in the requirements: `article_text: str`, `source_name: str`, `publication_time: datetime`, `title: str`, `url: str`, `article_hash: str`.
        *   **`EnrichedNewsEvent`**: Create a complex Pydantic model with nested models for `entities` and `sentiment`, matching the schema exactly: `id: str`, `source: str`, `published_at: datetime`, `ingested_at: datetime`, `event_type: str`, `entities: object`, `sentiment: object`, etc.
        *   **`RealTimeSentimentResponse` & `HistoricalSentimentResponse`**: Create Pydantic models for the two API output schemas.
    *   **Architecture Reference:** [4.1 Raw News Article Model](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#raw-news-article-model-api-contract), [4.4 Database Schema](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#database-schema)
    *   **Requirements Reference:** [2.1 Raw News Article](playground/risk-analytics-agent/ai_docs/2_data_services/NewsSentimentService.md#21-input-model-raw-news-article), [2.2 Enriched News Event](playground/risk-analytics-agent/ai_docs/2_data_services/NewsSentimentService.md#22-core-data-model-enriched-news-event), [2.3 API Output Models](playground/risk-analytics-agent/ai_docs/2_data_services/NewsSentimentService.md#23-api-output-models)
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/common/models/`

*   **[x] Task 1.2: Implement `common` Shared Library Azure Clients**
    *   **Description:** In the `common` library, create reusable client classes for interacting with Azure services. These clients must be configured to use `DefaultAzureCredential` for seamless authentication both locally (via Azure CLI) and in Azure (via Managed Identity).
    *   **Implementation Details:**
        *   **`CosmosDBClient`** (`common/cosmos_db/cosmos_db_client.py`): A class to encapsulate interactions with Cosmos DB. It should be initialized with the database name and container name. Implement methods like `upsert_item(item: dict)` and `query_items(query: str) -> list[dict]`.
        *   **`ServiceBusClient`** (`common/service_bus/service_bus_client.py`): A class to handle Service Bus communication. It should be initialized with the queue name. Implement methods like `send_message(message: str)` and a listener/receiver method `receive_messages(callback_function)`.
    *   **Architecture Reference:** [3.4.2 The `DefaultAzureCredential` Flow](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#342-the-defaultazurecredential-flow)
    *   **Requirements Reference:** N/A
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/common/cosmos_db/`, `playground/risk-analytics-agent/src/news_sentiment_service/common/service_bus/`

*   **[x] Task 1.3: Write Unit Tests for `common` Library (TDD)**
    *   **Description:** Create comprehensive unit tests for the shared library components to ensure their correctness and stability.
    *   **Implementation Details:**
        *   **Models (`common/tests/test_models.py`):** Write `pytest` tests to validate the Pydantic models. Test successful creation, type validation, and failure on missing or incorrect data.
        *   **Clients (`common/tests/test_cosmos_db_client.py`, etc.):** Write `pytest` tests for the Azure client classes. Use `unittest.mock` to patch the underlying Azure SDK clients (e.g., `CosmosClient`, `ServiceBusClient`) to test the logic of your wrapper classes without making actual Azure calls.
    *   **Architecture Reference:** N/A
    *   **Requirements Reference:** N/A
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/common/tests/`

*   **[x] Task 1.4: Set Up Local Development with Docker Compose**
    *   **Description:** Create the root `docker-compose.yml` file to orchestrate the multi-service local development environment, enabling easy startup and service-to-service communication.
    *   **Implementation Details:**
        *   Define services for `sentiment-api-service`, `news-processor-service`, and `data-ingestion-orchestrator-service`, `data-ingestion-bazinga-service`.
        *   Use the `build: context:` key to point to the respective service directories.
        *   Use the `environment:` key to inject the `AZURE_APPCONFIG_ENDPOINT` and the service URL overrides (e.g., `- SENTIMENT_API_URL=http://sentiment-api-service:8000`) as specified in the architecture.
    *   **Architecture Reference:** [3.4 Local Development Strategy](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#34-local-development-strategy)
    *   **Requirements Reference:** N/A
    *   **Structure Reference:** `docker-compose.yml`

*   **[x] Task 1.5: Define Base Infrastructure as Code (IaC)**
    *   **Description:** Create Bicep files in the `infrastructure/modules/` directory to define the core, shared Azure resources.
    *   **Implementation Details:**
        *   `app-config.bicep`: Defines an Azure App Configuration store.
        *   `key-vault.bicep`: Defines an Azure Key Vault and configures its access policies to later grant access to service Managed Identities.
        *   `service-bus.bicep`: Defines a Service Bus namespace and a queue named `articles-to-process`.
        *   `cosmos-db.bicep`: Defines a Cosmos DB account (NoSQL API) with a database and the `enriched-news-events` container. Configure the partition key to `/published_at`.
    *   **Architecture Reference:** [3.3 Infrastructure, Configuration, and Secrets Management](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#33-infrastructure-configuration-and-secrets-management)
    *   **Requirements Reference:** N/A
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/infrastructure/`

*   **[x] Task 1.6: Implement Main IaC Orchestration Files**
    *   **Description:** Create the top-level Bicep files that orchestrate the deployment of the individual resource modules for different environments.
    *   **Implementation Details:**
        *   `infrastructure/main.bicep`: Create the main Bicep file that references the modules in `infrastructure/modules/` to compose the full environment.
        *   `infrastructure/environments/`: Create parameter files (`dev.bicepparam`, `prod.bicepparam`) to provide environment-specific configurations (e.g., naming prefixes, SKUs).
    *   **Architecture Reference:** [3.3 Infrastructure, Configuration, and Secrets Management](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#33-infrastructure-configuration-and-secrets-management)
    *   **Requirements Reference:** N/A
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/infrastructure/`

---
## [ ] **Epic 2A: News Adapter Framework**

**Objective:** Build the extensible framework for ingesting news from various providers. This is a prerequisite for the ingestion orchestrator.

*   **[x] Task 2A.1: Define and Create the News Adapter Template**
    *   **Description:** In the `playground/risk-analytics-agent/src/news_sentiment_service/templates/data_ingestion_service_boilerplate` directory, create a reusable template for a news adapter microservice.
    *   **Implementation Details:**
        *   Take as the basis `playground/risk-analytics-agent/src/news_sentiment_service/templates/service_boilerplate`.
        *   Define a `/news` GET endpoint that returns a list of `RawNewsArticle` Pydantic models. This serves as the standard contract for all adapters.
        *   Include a `Dockerfile.template` and a base `requirements.txt`.
        *   Create a `Dockerfile` for the adapter service.
    *   **Architecture Reference:** [4.2 Data Ingestion Service (DIS)](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#42-data-ingestion-service-dis)
    *   **Requirements Reference:** N/A (Architectural pattern)
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/templates/service_boilerplate/`

*   **[x] Task 2A.2: Implement a Concrete News Adapter (e.g., Benzinga)**
    *   **Description:** Create the first specific news adapter by copying the `playground/risk-analytics-agent/src/news_sentiment_service/templates/data_ingestion_service_boilerplate` to a new `benzinga_adapter` directory and implementing the data fetching logic.
    *   **Implementation Details:**
        *   Copy the contents of `playground/risk-analytics-agent/src/news_sentiment_service/templates/data_ingestion_service_boilerplate` to `playground/risk-analytics-agent/src/news_sentiment_service/data_ingestion/adapters/benzinga_adapter/`.
        *   Implement the logic within the `/news` endpoint to connect to the "Benzinga" news feed API, transform its response into the standard `RawNewsArticle` model, and return it.
        *   Update the `Dockerfile` and `requirements.txt` as needed.
    *   **Architecture Reference:** [4.2 Data Ingestion Service (DIS)](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#42-data-ingestion-service-dis)
    *   **Requirements Reference:** N/A (Example implementation)
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/data_ingestion/adapters/benzinga_adapter/`

---
## [x] **Epic 2B: Data Ingestion Orchestration**

**Objective:** Build the components responsible for fetching raw news articles *from the adapters* and queuing them for enrichment.

*   **[x] Task 2B.1: Implement the Data Ingestion Service (DIS) Orchestrator**
    *   **Description:** Create the main application logic for the DIS as a Python script run on a schedule. This service orchestrates the entire ingestion workflow by calling the various news adapters and queuing articles for processing.
    *   **Implementation Details:**
        *   Integrate with Azure App Configuration and environment variables to load all required settings, including a list of news adapter service URLs.
        *   Loop through each adapter URL and make an HTTP GET request to its `/news` endpoint using `httpx`.
        *   For each `RawNewsArticle` returned, use the `ServiceBusClient` to enqueue it into the `articles-to-process` queue for downstream processing.
        *   Implement robust logging and error handling for failed adapter calls or queueing operations.
        *   **Note:** Duplicate checking is handled downstream by the News Processor Service to maintain clean separation of concerns.
    *   **Architecture Reference:** [4.2 Data Ingestion Service (DIS)](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#42-data-ingestion-service-dis)
    *   **Requirements Reference:** [4.4 Real-Time News Ingestion Service](playground/risk-analytics-agent/ai_docs/2_data_services/NewsSentimentService.md#44-component-4-real-time-news-ingestion-service) (`RTN-FR-01`, `RTN-FR-01a`, `RTN-FR-01b`)
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/data_ingestion/orchestrator_service/`
    *   **Template Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/templates/service_boilerplate/`

*   **[x] Task 2B.2: Write Unit Tests for DIS Orchestrator (TDD)**
    *   **Description:** Test the core logic of the DIS orchestrator.
    *   **Implementation Details:**
        *   Use `pytest` and `unittest.mock`.
        *   Mock the `httpx` client to simulate responses from news adapters (both success and failure cases).
        *   Mock the `ServiceBusClient` to verify that `send_message` is called with the correct article payload.
        *   **Note:** No need to test duplicate checking as this responsibility is moved to the News Processor Service.
    *   **Architecture Reference:** N/A
    *   **Requirements Reference:** N/A
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/data_ingestion/orchestrator_service/tests/`

---
## [ ] **Epic 3: News Processing & Enrichment**

**Objective:** Develop the core AI service that enriches raw articles with structured analysis and sentiment scores.

*   **[ ] Task 3.1: Implement ArticleEnricher Component in News Processor Service**
    *   **Description:** Create a dedicated enrichment component within the News Processor Service to handle all article enrichment logic with Azure OpenAI integration.
    *   **Implementation Details:**
        *   Create an `ArticleEnricher` class in `news_processor_service/enrichment/`.
        *   The constructor should accept an Azure OpenAI client configuration.
        *   Implement a method `enrich_article(article: RawNewsArticle) -> EnrichedNewsEvent`.
        *   This method will contain the two-step prompt logic:
            1.  Build and send Prompt 1 (Entity Extraction).
            2.  If relevant, build and send Prompt 2 (Scoring & Classification).
            3.  Parse both JSON responses and assemble the final `EnrichedNewsEvent` object.
        *   Include robust error handling and logging for AI service failures.
    *   **Architecture Reference:** [4.3 News Processor Service (NPS)](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#43-news-processor-service-nps)
    *   **Requirements Reference:** [4.1 News Scoring Service](playground/risk-analytics-agent/ai_docs/2_data_services/NewsSentimentService.md#41-component-1-news-scoring-service) (`NSS-FR-01` to `NSS-FR-07`)
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/news_processor_service/enrichment/`

*   **[ ] Task 3.2: Implement the News Processor Service (NPS)**
    *   **Description:** Build the NPS as a comprehensive message-driven service that handles the complete processing pipeline: listening to Service Bus, duplicate checking, enrichment, and storage.
    *   **Implementation Details:**
        *   Create a FastAPI application. Use a background task (`fastapi.BackgroundTasks`) or a startup event to start a listener loop.
        *   Integrate with Azure App Configuration and environment variables to load all required settings (e.g., OpenAI endpoint, Cosmos DB connection details).
        *   The listener will use the `ServiceBusClient`'s `receive_messages` method, passing a callback function.
        *   The callback function will:
            1.  Receive a `RawNewsArticle` from the Service Bus queue.
            2.  Check for duplicates by querying Cosmos DB using the `article_hash`.
            3.  If the article is new, instantiate and use the `ArticleEnricher` to get an `EnrichedNewsEvent`.
            4.  Use the `CosmosDBClient` to `upsert_item` the enriched event into the database.
            5.  Handle errors gracefully with appropriate logging and dead letter queue management.
        *   Implement health check endpoints for monitoring.
    *   **Architecture Reference:** [4.3 News Processor Service (NPS)](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#43-news-processor-service-nps)
    *   **Requirements Reference:** [4.1 News Scoring Service](playground/risk-analytics-agent/ai_docs/2_data_services/NewsSentimentService.md#41-component-1-news-scoring-service) (`NSS-FR-01` to `NSS-FR-07`)
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/news_processor_service/`

*   **[ ] Task 3.3: Write Unit Tests for NPS and ArticleEnricher (TDD)**
    *   **Description:** Test the enrichment component and the service's complete processing pipeline logic.
    *   **Implementation Details:**
        *   **`ArticleEnricher` Tests:** Mock the OpenAI client to return predictable JSON for both prompts. Provide a sample `RawNewsArticle` and assert that the `enrich_article` method returns the expected `EnrichedNewsEvent`.
        *   **NPS Tests:** 
            - Mock the `ServiceBusClient` to simulate receiving a message.
            - Mock the `CosmosDBClient` for both duplicate checking queries and `upsert_item` operations.
            - Mock the `ArticleEnricher` to test the service's orchestration logic.
            - Test both new article processing and duplicate article handling scenarios.
    *   **Architecture Reference:** N/A
    *   **Requirements Reference:** N/A
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/news_processor_service/tests/`

---
## [ ] **Epic 4: API & Data Serving**

**Objective:** Expose the aggregated sentiment scores to consumers via a public REST API.

*   **[ ] Task 4.1: Implement the Sentiment Score API Service (NSSS)**
    *   **Description:** Create the public-facing `Sentiment Score API` using FastAPI. This service handles API requests, queries the database, and performs the final score aggregation.
    *   **Implementation Details:**
        *   Define endpoints `GET /sentiment/realtime` and `GET /sentiment/historical` with the specified query parameters and Pydantic response models.
        *   Integrate with Azure App Configuration and environment variables to load all required settings (e.g., Cosmos DB connection details).
        *   In each endpoint, use the `CosmosDBClient` to build and execute a query to fetch relevant `EnrichedNewsEvent`s.
        *   Create a private helper function `_calculate_ass(events: list[EnrichedNewsEvent], mode: str)` to implement the Aggregated Sentiment Score (ASS) formula. This function must correctly apply the weights (`W_event`, `W_source`, `W_time`) as defined in the business rules.
    *   **Architecture Reference:** [4.5 News Sentiment Score Service (NSSS)](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#45-news-sentiment-score-service-nsss)
    *   **Requirements Reference:** [4.2 On-Demand Sentiment Service](playground/risk-analytics-agent/ai_docs/2_data_services/NewsSentimentService.md#42-component-2-on-demand-sentiment-service-public-api), [3.1 Formula: ASS](playground/risk-analytics-agent/ai_docs/2_data_services/NewsSentimentService.md#31-formula-aggregated-sentiment-score-ass)
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/sentiment_api_service/`

*   **[ ] Task 4.2: Write Unit and Integration Tests for NSSS (TDD)**
    *   **Description:** Ensure the API endpoints and calculation logic are correct.
    *   **Implementation Details:**
        *   **Unit Tests:** Create `pytest` tests specifically for the `_calculate_ass` helper function. Provide it with a static list of `EnrichedNewsEvent` objects and assert that the returned score is arithmetically correct for both `REALTIME` and `HISTORICAL` modes.
        *   **Integration Tests:** Use FastAPI's `TestClient`. For each endpoint, mock the `CosmosDBClient` to return a predefined list of events. Make a request via the `TestClient` and assert that the HTTP response code is 200 and the JSON body matches the expected, calculated output.
    *   **Architecture Reference:** N/A
    *   **Requirements Reference:** N/A
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/sentiment_api_service/tests/`

---
## [ ] **Epic 5: Historical Data Processing**

**Objective:** Create a utility to process and backfill the system with historical news data.

*   **[ ] Task 5.1: Create the Historical Processing Script**
    *   **Description:** Develop a standalone Python script for batch processing a large, local archive of historical news. This script will directly integrate the enrichment logic and follow the same processing pipeline as the News Processor Service.
    *   **Implementation Details:**
        *   The script should accept a command-line argument for the path to the news archive.
        *   Integrate with Azure App Configuration and environment variables to load all required settings.
        *   It will walk the directory, reading each news file and converting to `RawNewsArticle` objects.
        *   For each article, it will:
            1.  Check for duplicates by querying Cosmos DB using the `article_hash`.
            2.  If the article is new, directly instantiate the `ArticleEnricher` component (same logic as News Processor Service).
            3.  Use the `CosmosDBClient` to store the enriched event.
        *   Include progress indicators, batch processing capabilities, and robust error handling.
        *   **Note:** This script replicates the News Processor Service logic directly rather than going through Service Bus to optimize for batch processing performance.
    *   **Architecture Reference:** N/A (utility script)
    *   **Requirements Reference:** [4.3 Historical Processing Service](playground/risk-analytics-agent/ai_docs/2_data_services/NewsSentimentService.md#43-component-3-historical-processing-service) (`HPS-FR-01` to `HPS-FR-05`)
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/scripts/`

---
## [ ] **Epic 6: Deployment & Configuration**

**Objective:** Create a robust, automated CI/CD pipeline to deploy infrastructure, configure environments, and deploy the applications.

*   **[ ] Task 6.1: Implement Configuration Loading from YAML**
    *   **Description:** In the `Sentiment Score API` service, implement logic to load the `event_weights.yml` and `source_weights.yml` files at startup. These values should be used in the Aggregated Sentiment Score (ASS) calculation.
    *   **Architecture Reference:** [4.5 News Sentiment Score Service (NSSS)](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#45-news-sentiment-score-service-nsss)
    *   **Structure Reference:** `playground/risk-analytics-agent/src/news_sentiment_service/sentiment_api_service/config/`

*   **[ ] Task 6.2: Create CI/CD Script for Configuration Management**
    *   **Description:** Develop a script (e.g., PowerShell, Bash) that will be run as part of the CI/CD pipeline. This script will be responsible for populating Azure App Configuration and Azure Key Vault.
    *   **Implementation Details:**
        *   The script should accept IaC deployment outputs (like resource names) as parameters.
        *   It will set non-sensitive values (e.g., service URLs) in App Configuration using `az appconfig kv set`.
        *   It will retrieve sensitive values (e.g., Cosmos DB keys) using the Azure CLI.
        *   It will securely store secrets in Key Vault using `az keyvault secret set`.
        *   It will create Key Vault references in App Configuration using `az appconfig kv set-keyvault`.
    *   **Architecture Reference:** [3.3 Infrastructure, Configuration, and Secrets Management](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#33-infrastructure-configuration-and-secrets-management)

*   **[ ] Task 6.3: Implement CI/CD Pipeline for IaC Deployment**
    *   **Description:** Create the main CI/CD pipeline (e.g., in GitHub Actions, Azure DevOps) that orchestrates the entire deployment.
    *   **Implementation Details:**
        *   The pipeline will have a stage to deploy the Bicep templates using `az deployment group create`.
        *   A subsequent step must capture the JSON output from the Bicep deployment.
        *   The pipeline will then trigger the configuration management script (from Task 6.2), passing the captured outputs to it.
    *   **Architecture Reference:** [3.3 Infrastructure, Configuration, and Secrets Management](playground/risk-analytics-agent/ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md#33-infrastructure-configuration-and-secrets-management)
    *   **Requirements Reference:** N/A
    *   **Structure Reference:** N/A 