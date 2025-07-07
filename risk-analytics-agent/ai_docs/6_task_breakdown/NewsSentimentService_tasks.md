# News Sentiment Service - Task Breakdown (Developer Focused)

---

## **Epic 1: Core Service Foundation**
**Goal:** Establish the foundational Python code required by all microservices, allowing for local development and testing.

#### **Task 1.1:** Initialize Monorepo and Shared Scaffolding
-   **User Story**: As a Developer, I want a standardized monorepo structure so that I can easily navigate between services and shared components.
-   **Definition of Done**:
    -   The top-level directory structure (according to ai_docs\5_service_code_structure\NewsSentimentService_Structure.md) is created.
    -   A shared `common/models.py` file is created. It must contain the Pydantic models for `RawNewsArticle` and `EnrichedNewsEvent`, matching the schemas in Section 2.1 and 2.2 of the requirements document.

#### **Task 1.2:** Implement Common Infrastructure Clients
-   **User Story**: As a Developer, I need reusable clients for core services so that I can interact with Azure infrastructure consistently, with the ability to mock them for local development.
-   **Definition of Done**:
    -   A `common/cosmos_db/` directory is created containing the abstracted client for Cosmos DB operations.
    -   A `common/service_bus/` directory is created containing the abstracted client for Service Bus operations.
    -   The CosmosDB client provides methods for fetching timestamps, checking `article_hash` existence, and inserting `EnrichedNewsEvent` documents.
    -   The Service Bus client provides methods to send and receive messages from the queue.
    -   (Note: Initial implementation can use in-memory mocks or local containers, with cloud connectivity to be configured later).

#### **Task 1.3:** Define Standard Service Boilerplate
-   **User Story**: As a Developer, I want a single, clear definition for a standard microservice boilerplate so that all new services are created consistently.
-   **Definition of Done**:
    -   A standard service boilerplate is defined and will be used for all subsequent service creation tasks. It must include:
        -   A dedicated service directory inside `/src`.
        -   A `main.py` file serving as the entrypoint for the service or worker.
        -   An empty `requirements.txt` file.
        -   A dedicated `docker/Dockerfile.[service-name]` file, configured to package the specific service into a container image.

---

## **Epic 2: Data Ingestion Service (e.g., DIS-Bazinga)**
**Goal:** Develop the core logic for a provider-specific, cron-driven service that fetches news, deduplicates it, and queues it for analysis.

#### **Task 2.1:** Initialize the DIS Project Boilerplate for a Provider
-   **User Story**: As a Developer, I want to initialize the project structure for a specific Data Ingestion Service so that I can begin implementing its logic.
-   **Definition of Done**:
    -   A directory, e.g., `data_ingestion_bazinga/`, is created and populated using the standard service boilerplate defined in **Task 1.3**.
    -   An `orchestrator.py` file is created inside the service directory.

#### **Task 2.2:** Develop the Ingestion Orchestration Logic for a Provider
-   **User Story**: As a Data Engineer, I want the service to automatically fetch, filter, and queue new articles from a specific provider's adapter.
-   **Definition of Done**:
    -   The logic in `data_ingestion_bazinga/orchestrator.py` implements watermarking by fetching the last run timestamp from Cosmos DB (**`RTN-FR-01b`**).
    -   It implements deduplication by checking the `article_hash` against Cosmos DB (**`RTN-FR-01a`**, **`HPS-FR-01a`**).
    -   New, unique articles are queued to Service Bus for processing (**`RTN-FR-01`**).

---

## **Epic 3: News Processor Service (NPS) - Core Logic**
**Goal:** Implement the core logic for the worker service that performs the core AI analysis and enrichment.

#### **Task 3.1:** Initialize the NPS Project Boilerplate
-   **User Story**: As a Developer, I want to initialize the project structure for the News Processor Service.
-   **Definition of Done**:
    -   A directory `news_processor/` is created and populated using the standard service boilerplate defined in **Task 1.3**.
    -   A `worker.py` file and an `analysis/` subdirectory are created inside the service directory.

#### **Task 3.2:** Develop the AI Analysis and Enrichment Logic
-   **User Story**: As a Risk Analyst, I want raw articles to be automatically analyzed and enriched with structured data.
-   **Definition of Done**:
    -   The logic is implemented in the `news_processor/analysis/` directory.
    -   The first-step AI call determines relevance and extracts entities (`issuer_name`, `cusips`, `sector`, `state`), fulfilling **`NSS-FR-02`**.
    -   The second-step AI call generates sentiment (`score`, `magnitude`), `event_type`, and `summary_excerpt`, fulfilling **`NSS-FR-03`**, **`NSS-FR-04`**, **`NSS-FR-05`**.
    -   The service assigns the `source_credibility_tier`, fulfilling **`NSS-FR-06`**. The entire process produces a valid `EnrichedNewsEvent` object (**`NSS-FR-01`**, **`NSS-FR-07`**).

#### **Task 3.3:** Implement the Queue Worker
-   **User Story**: As a Developer, I want a worker that listens for articles, processes them, and stores the results.
-   **Definition of Done**:
    -   The `news_processor/worker.py` module polls the Service Bus queue.
    -   It invokes the analysis logic for each message (**`RTN-FR-02`**, **`HPS-FR-02`**).
    -   It persists the `EnrichedNewsEvent` to Cosmos DB, routing events based on the `sector` as per **`RTN-FR-03`**, **`HPS-FR-03`**, and **`HPS-FR-03a`**.

---

## **Epic 4: Sentiment Score API Service (SSAS) - Core Logic**
**Goal:** Build the core logic for the public API that serves aggregated sentiment scores.

#### **Task 4.1:** Initialize the SSAS Project Boilerplate
-   **User Story**: As a Developer, I want to initialize the project structure for the Sentiment Score API Service.
-   **Definition of Done**:
    -   A directory `sentiment_api/` is created and populated using the standard service boilerplate defined in **Task 1.3**.
    -   An `endpoints/` subdirectory and an `aggregation.py` file are created inside the service directory.

#### **Task 4.2:** Develop the Score Aggregation Logic
-   **User Story**: As a Quant Analyst, I need a reliable implementation of the Aggregated Sentiment Score formula.
-   **Definition of Done**:
    -   The logic in `sentiment_api/aggregation.py` implements the **Aggregated Sentiment Score (ASS)** formula from Section 3.1 of the requirements (**`SCS-FR-04`**).
    -   The function correctly applies all weights (`W_event`, `W_source`, `W_time`).
    -   The time-decay logic handles `REALTIME` and `HISTORICAL` modes as specified in rules **`BR-01`** and **`BR-02`** (**`SCS-FR-05`**).

#### **Task 4.3:** Implement the API Endpoints
-   **User Story**: As a consumer, I want performant API endpoints to request sentiment scores.
-   **Definition of Done**:
    -   The `/sentiment/realtime` and `/sentiment/historical` endpoints are implemented in `sentiment_api/endpoints/sentiment.py` (**`SCS-FR-01`**, **`SCS-FR-02`**).
    -   The endpoints retrieve relevant events from Cosmos DB based on request parameters (**`SCS-FR-03`**).
    -   The API response matches the schemas in Sections 2.3.1 and 2.3.2.

---

## **Epic 5: DevOps, Infrastructure & Deployment**
**Goal:** Provision cloud infrastructure, configure Kubernetes deployments, and establish CI/CD pipelines.

#### **Task 5.1:** Define Shared Infrastructure as Code (IaC) with Azure ARM Templates
-   **User Story**: As a DevOps Engineer, I want the core Azure infrastructure defined as code so I can provision and manage the environment reliably and repeatably.
-   **Definition of Done**:
    -   **Azure ARM Templates** scripts are created in the `/scripts/iac/` directory.
    -   The scripts must provision: an Azure Cosmos DB instance, an Azure Service Bus, and an Azure Container Registry.
    -   A `README.md` is included in the `iac` directory with explicit instructions on how to configure and apply the Azure ARM Templates plan.
