# Task Breakdown: News Sentiment Component

This document breaks down the development work for the News Sentiment component into actionable epics and tasks. Each task is mapped back to the core requirements and system architecture documents to ensure alignment.

-   **Requirements Document:** `ai_docs/2_data_services/NewsSentimentService.md`
-   **Architecture Document:** `ai_docs/1_architecture/NewsSentimentService_SystemArchitecture.md`

---

### Epic 1: Data Ingestion Service (DIS)
-   **Description**: The AKS CronJob responsible for fetching, deduplicating, and queuing news articles.
-   **Architecture Reference**: `4.2 Data Ingestion Service (DIS)`
-   **Requirements Reference**: `4.4 Component 4: Real-Time News Ingestion Service`

| Task ID | Description                                       |
| :------ | :------------------------------------------------ |
| **1.1** | Initialize the Python project for the DIS.        |
| **1.2** | Implement a client for Cosmos DB.                 |
| **1.3** | Implement a client for Azure Service Bus.         |
| **1.4** | Develop the core ingestion orchestration logic.   |
| **1.5** | Create a `Dockerfile`.                            |

---

#### **Task 1.1:** Initialize the Python project for the DIS
-   **User Story**: As a Developer, I want a standardized Python project structure for the Data Ingestion Service so that I can easily locate files and add new code consistently.
-   **Definition of Done**:
    -   [ ] A directory for the service is created at `services/data-ingestion-service/`.
    -   [ ] It contains an `app/` source directory, a `tests/` directory, a `requirements.txt`, and a `Dockerfile`.
    -   [ ] Basic `__init__.py` files are in place to form a package.

#### **Task 1.2:** Implement a client for Cosmos DB
-   **User Story**: As the System, I need to persist and retrieve the last ingestion timestamp and check for existing article hashes so that I can prevent processing duplicate news articles and only fetch the latest content.
-   **Definition of Done**:
    -   [ ] A `clients/cosmos_client.py` module exists.
    -   [ ] It contains functions to get/set timestamps and check for article hash existence.
    -   [ ] Functions are covered by unit tests with the Cosmos DB SDK mocked.
    -   **Requirements Covered**: `RTN-FR-01a`, `RTN-FR-01b`

#### **Task 1.3:** Implement a client for Azure Service Bus
-   **User Story**: As the System, I need to publish raw news articles to a central message queue so that they can be processed asynchronously by the News Processor Service.
-   **Definition of Done**:
    -   [ ] A `clients/service_bus_client.py` module exists.
    -   [ ] It contains a function to send a message (the raw article data) to the configured queue.
    -   [ ] The function is covered by unit tests with the Service Bus SDK mocked.

#### **Task 1.4:** Develop the core ingestion orchestration logic
-   **User Story**: As a Data Engineer, I want the service to automatically fetch new articles, filter duplicates, and queue them for processing so that the system is continuously updated with fresh, relevant news.
-   **Definition of Done**:
    -   [ ] The `main.py` and `core/logic.py` files contain the orchestration logic.
    -   [ ] The logic correctly uses the Cosmos DB and Service Bus clients to perform the end-to-end flow.
    -   [ ] The entire flow is covered by an integration test.
    -   **Requirements Covered**: `RTN-FR-01`, `RTN-FR-02`

#### **Task 1.5:** Create a `Dockerfile`
-   **User Story**: As a DevOps Engineer, I need a Dockerfile for the Data Ingestion Service so that I can build a container image and deploy it consistently across different environments.
-   **Definition of Done**:
    -   [ ] A `Dockerfile` is present in the service's root directory.
    -   [ ] The `docker build` command successfully creates a runnable image.

---

### Epic 2: News Processor Service (NPS)
-   **Description**: The FastAPI service that consumes articles from the queue and performs AI enrichment.
-   **Architecture Reference**: `4.3 News Processor Service (NPS)`
-   **Requirements Reference**: `4.1 Component 1: News Scoring Service`

| Task ID | Description                                       |
| :------ | :------------------------------------------------ |
| **2.1** | Initialize the FastAPI project for the NPS.       |
| **2.2** | Implement the Service Bus listener.               |
| **2.3** | Develop the two-step AI processing logic.         |
| **2.4** | Implement persistence to Cosmos DB.               |
| **2.5** | Add error handling.                               |
| **2.6** | Create a `Dockerfile`.                            |

---

#### **Task 2.1:** Initialize the FastAPI project for the NPS
-   **User Story**: As a Developer, I want a standardized FastAPI project structure for the News Processor Service so that I can easily build and scale the enrichment service.
-   **Definition of Done**:
    -   [ ] A directory for the service is created at `services/news-processor-service/`.
    -   [ ] It contains an `app/` source directory, a `tests/` directory, a `requirements.txt`, and a `Dockerfile`, all configured for a basic FastAPI app.

#### **Task 2.2:** Implement the Service Bus listener
-   **User Story**: As the System, I need to continuously listen for and receive new articles from the Azure Service Bus queue so that I can process them in near real-time.
-   **Definition of Done**:
    -   [ ] A `services/service_bus_listener.py` module is implemented to run in the background of the FastAPI app's lifecycle.
    -   [ ] It successfully connects to the queue, retrieves messages, and deserializes them into a Pydantic model.

#### **Task 2.3:** Develop the two-step AI processing logic
-   **User Story**: As a Risk Analyst, I want raw news articles to be automatically analyzed and enriched with structured data (entities, sentiment, event type) so that I can use this data for risk calculations.
-   **Definition of Done**:
    -   [ ] The `core/processing.py` module contains functions that call the Azure OpenAI service with the two specified prompts.
    -   [ ] It correctly parses the JSON responses and handles potential errors from the AI model.
    -   [ ] Logic is covered by unit tests using mocks for the OpenAI API.
    -   **Requirements Covered**: `NSS-FR-02` to `NSS-FR-06`, `NFR-MQ-01`

#### **Task 2.4:** Implement persistence to Cosmos DB
-   **User Story**: As the System, I need to store the fully enriched news event data in a permanent database so that it can be queried by the Sentiment Score API.
-   **Definition of Done**:
    -   [ ] The service correctly uses a Cosmos DB client to save the `EnrichedNewsEvent` model to the database.
    -   [ ] Logic to route events to the "Enriched News Store" vs "Audit News Store" is implemented.
    -   **Requirements Covered**: `NSS-FR-07`, `RTN-FR-03`

#### **Task 2.5:** Add robust error handling
-   **User Story**: As a System Operator, I want failed processing attempts to be handled gracefully with retries, and permanently failing messages moved to a dead-letter queue so that I can investigate and prevent data loss.
-   **Definition of Done**:
    -   [ ] The Service Bus listener includes error handling.
    -   [ ] A message that fails processing is retried a configurable number of times before being moved to the dead-letter queue.
    -   [ ] This behavior is verified by tests.

#### **Task 2.6:** Create a `Dockerfile`
-   **User Story**: As a DevOps Engineer, I need a Dockerfile for the News Processor Service so I can build and deploy it as a container.
-   **Definition of Done**:
    -   [ ] A `Dockerfile` is present in the service's root directory.
    -   [ ] The `docker build` command successfully creates a runnable image.

---

### Epic 3: Sentiment Score API (NSSS)
-   **Description**: The public-facing FastAPI for calculating and serving aggregated sentiment scores.
-   **Architecture Reference**: `4.5 News Sentiment Score Service (NSSS)`
-   **Requirements Reference**: `4.2 Component 2: On-Demand Sentiment Service (Public API)`

| Task ID | Description                                       |
| :------ | :------------------------------------------------ |
| **3.1** | Initialize the FastAPI project for the NSSS.                |
| **3.2** | Implement the API endpoints.                                |
| **3.3** | Implement the data access layer.                            |
| **3.4** | Implement the Aggregated Sentiment Score (ASS) formula.     |
| **3.5** | Create a `Dockerfile`.                                      |

---

#### **Task 3.1:** Initialize the FastAPI project for the NSSS
-   **User Story**: As a Developer, I want a standardized FastAPI project for the Sentiment Score API so that I can build and expose the sentiment data efficiently.
-   **Definition of Done**:
    -   [ ] A directory for the service is created at `services/sentiment-score-api/`.
    -   [ ] It is set up as a basic FastAPI application.

#### **Task 3.2:** Implement the API endpoints
-   **User Story**: As a client application, I want to be able to request a sentiment score for a financial entity, either in real-time or for a historical date, so that I can incorporate this score into my risk analysis.
-   **Definition of Done**:
    -   [ ] `GET /sentiment/realtime` and `GET /sentiment/historical` are implemented.
    -   [ ] They accept the specified query parameters and return the correct response models.
    -   [ ] Endpoints are automatically documented in the OpenAPI specification.
    -   **Requirements Covered**: `SCS-FR-01`, `SCS-FR-02`

#### **Task 3.3:** Implement the data access layer
-   **User Story**: As the System, I need to efficiently query the Enriched News Store for all relevant articles based on parameters like CUSIP, sector, or issuer name, so that I can calculate an aggregated score.
-   **Definition of Done**:
    -   [ ] A `services/cosmos_db.py` module has functions to query the database based on the API parameters.
    -   [ ] Queries are performant and use appropriate indexes.
    -   **Requirements Covered**: `SCS-FR-03`

#### **Task 3.4:** Implement the Aggregated Sentiment Score (ASS) formula
-   **User Story**: As a Risk Analyst, I want the API to calculate a single, aggregated sentiment score from multiple news articles using the approved weighted formula so that I get a reliable and consistent risk indicator.
-   **Definition of Done**:
    -   [ ] The `core/calculation.py` module contains a complete implementation of the ASS formula.
    -   [ ] It correctly applies time-decay, event, and source weights.
    -   [ ] The calculation is verified by unit tests with various inputs.
    -   **Requirements Covered**: `SCS-FR-04`, `SCS-FR-05`, `NFR-DC-01`

#### **Task 3.5:** Create a `Dockerfile`
-   **User Story**: As a DevOps Engineer, I need a Dockerfile for the Sentiment Score API so I can build and deploy it as a container.
-   **Definition of Done**:
    -   [ ] A `Dockerfile` is present in the service's root directory.
    -   [ ] The `docker build` command successfully creates a runnable image.

---

### Epic 4: Cross-Cutting Concerns & Deployment
-   **Description**: Shared code, infrastructure, and deployment pipelines for the entire component.

| Task ID | Description                                       |
| :------ | :------------------------------------------------ |
| **4.1** | Create a shared library for Pydantic models.      |
| **4.2** | Define Infrastructure-as-Code (IaC).              |
| **4.3** | Set up GitLab CI/CD pipelines.                    |
| **4.4** | Configure centralized logging and monitoring.     |

---

#### **Task 4.1:** Create a shared library for Pydantic models
-   **User Story**: As a Developer, I want a single source of truth for all data models so that I can ensure data consistency between microservices and avoid code duplication.
-   **Definition of Done**:
    -   [ ] A `shared/models` directory exists and is installable as a Python package.
    -   [ ] All services use this shared package for their data contracts.
    -   **Requirements Covered**: `2.1`, `2.2`, `2.3`

#### **Task 4.2:** Define Infrastructure-as-Code (IaC)
-   **User Story**: As a DevOps Engineer, I want the entire Azure infrastructure to be defined as code so that I can create, update, or replicate the environment automatically and reliably.
-   **Definition of Done**:
    -   [ ] Bicep or Terraform scripts are created in the repository.
    -   [ ] Running these scripts provisions all necessary Azure resources (AKS, Cosmos DB, Service Bus, etc.).

#### **Task 4.3:** Set up GitLab CI/CD pipelines
-   **User Story**: As a Developer, I want my code changes to be automatically tested and deployed to the correct environment upon merging so that I can deliver features quickly and safely.
-   **Definition of Done**:
    -   [ ] A `.gitlab-ci.yml` file is configured.
    -   [ ] Pushing to a feature branch runs tests.
    -   [ ] Merging to `main` triggers a build and deployment to the AKS cluster.

#### **Task 4.4:** Configure centralized logging and monitoring
-   **User Story**: As a System Operator, I want all logs and performance metrics from all services to be collected in a central location so that I can easily monitor system health and diagnose issues.
-   **Definition of Done**:
    -   [ ] All services are configured to send logs and telemetry to Azure Application Insights.
    -   [ ] A basic dashboard is created to visualize key metrics (e.g., API latency, error rates). 