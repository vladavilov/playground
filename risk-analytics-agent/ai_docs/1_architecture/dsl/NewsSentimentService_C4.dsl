workspace "News Sentiment Service" "C4 Architecture Model for News Sentiment Component" {

    model {
        # External Systems
        bloomberg = softwareSystem "Bloomberg" "Financial news and data provider" "External System"
        tradingeconomics = softwareSystem "TradingEconomics" "Economic data and news provider" "External System"
        reuters = softwareSystem "Reuters" "Global news provider" "External System"
        openai = softwareSystem "OpenAI GPT-4" "AI language model for text analysis" "External System"
        
        # Users
        riskAnalyst = person "Risk Analyst" "Uses risk analytics system for trading decisions"
        orchestratorService = softwareSystem "Orchestrator Service" "Central coordinator of risk analytics workflow" "Internal System"
        
        # The News Sentiment System
        newsSentimentSystem = softwareSystem "News Sentiment Service" "Ingests, processes, and serves aggregated news sentiment scores for fixed-income instruments" {
            
            # Containers (applications/services)
            newsAdapterLayer = container "News Adapter Layer" "Provides unified interface to multiple news sources" "Python" {
                bloombergAdapter = component "Bloomberg Adapter" "Fetches articles from Bloomberg API" "Python Class"
                teAdapter = component "TradingEconomics Adapter" "Fetches articles from TE API" "Python Class"
                reutersAdapter = component "Reuters Adapter" "Fetches articles from Reuters API" "Python Class"
                adapterFactory = component "Adapter Factory" "Creates appropriate adapter based on source type" "Python Class"
            }
            
            dataIngestionService = container "Data Ingestion Service" "Orchestrates periodic news collection" "Python/AKS CronJob" {
                scheduler = component "Scheduler" "Manages cron-based execution" "APScheduler"
                ingestionOrchestrator = component "Ingestion Orchestrator" "Coordinates news fetching from all sources" "Python Class"
                circuitBreaker = component "Circuit Breaker" "Handles source failures gracefully" "Python Class"
                deduplicator = component "Deduplicator" "Prevents duplicate article processing" "Python Class"
            }
            
            newsProcessorService = container "News Processor Service" "Enriches raw news with AI analysis" "Python/FastAPI" {
                gptClient = component "GPT Client" "Interfaces with OpenAI API" "OpenAI SDK"
                promptEngine = component "Prompt Engine" "Manages prompts for GPT-4" "Python Class"
                entityExtractor = component "Entity Extractor" "Extracts issuer, sector, CUSIP" "Python Class"
                sentimentAnalyzer = component "Sentiment Analyzer" "Calculates sentiment scores" "Python Class"
                eventClassifier = component "Event Classifier" "Classifies news event types" "Python Class"
            }
            
            cosmosDB = container "Azure Cosmos DB" "Stores enriched news events" "NoSQL Database" {
                tags "Database"
            }
            
            redis = container "Redis Cache" "Caches sentiment calculations" "In-Memory Cache" {
                tags "Database"
            }
            
            sentimentScoreAPI = container "Sentiment Score API" "Calculates and serves aggregated sentiment scores" "Python/FastAPI" {
                realtimeEndpoint = component "Realtime Endpoint" "Calculates current sentiment" "REST API"
                historicalEndpoint = component "Historical Endpoint" "Calculates past sentiment" "REST API"
                sentimentCalculator = component "Sentiment Calculator" "Core aggregation logic" "Python Class"
                cacheManager = component "Cache Manager" "Manages Redis caching" "Python Class"
            }
            
            messageQueue = container "Azure Service Bus" "Message queue for async processing" "Message Queue" {
                tags "Queue"
            }
        }
        
        # Relationships - External
        bloomberg -> newsAdapterLayer "Provides news articles"
        tradingeconomics -> newsAdapterLayer "Provides news articles"
        reuters -> newsAdapterLayer "Provides news articles"
        newsProcessorService -> openai "Sends articles for analysis"
        
        # Relationships - Users
        riskAnalyst -> orchestratorService "Requests risk analysis"
        orchestratorService -> sentimentScoreAPI "Requests sentiment scores"
        
        # Relationships - Internal Components
        dataIngestionService -> newsAdapterLayer "Fetches articles"
        dataIngestionService -> messageQueue "Queues articles for processing"
        messageQueue -> newsProcessorService "Delivers articles"
        newsProcessorService -> cosmosDB "Stores enriched events"
        sentimentScoreAPI -> cosmosDB "Queries enriched events"
        sentimentScoreAPI -> redis "Caches results"
        
        # Component-level relationships
        bloombergAdapter -> bloomberg "API calls"
        teAdapter -> tradingeconomics "API calls"
        reutersAdapter -> reuters "API calls"
        adapterFactory -> bloombergAdapter "Creates"
        adapterFactory -> teAdapter "Creates"
        adapterFactory -> reutersAdapter "Creates"
        
        ingestionOrchestrator -> adapterFactory "Uses"
        ingestionOrchestrator -> circuitBreaker "Protected by"
        ingestionOrchestrator -> deduplicator "Filters through"
        scheduler -> ingestionOrchestrator "Triggers"
        
        gptClient -> openai "API calls"
        promptEngine -> gptClient "Sends prompts"
        entityExtractor -> promptEngine "Uses"
        sentimentAnalyzer -> promptEngine "Uses"
        eventClassifier -> promptEngine "Uses"
        
        realtimeEndpoint -> sentimentCalculator "Uses"
        historicalEndpoint -> sentimentCalculator "Uses"
        sentimentCalculator -> cacheManager "Uses"
        cacheManager -> redis "Read/Write"
        sentimentCalculator -> cosmosDB "Queries"
    }

    views {
        # Level 1: System Context
        systemContext newsSentimentSystem "SystemContext" {
            include *
            autoLayout
        }
        
        # Level 2: Container
        container newsSentimentSystem "Containers" {
            include *
            autoLayout
        }
        
        # Level 3: Components - News Adapter Layer
        component newsAdapterLayer "NewsAdapterComponents" {
            include *
            autoLayout
        }
        
        # Level 3: Components - Data Ingestion Service
        component dataIngestionService "DataIngestionComponents" {
            include *
            autoLayout
        }
        
        # Level 3: Components - News Processor Service
        component newsProcessorService "NewsProcessorComponents" {
            include *
            autoLayout
        }
        
        # Level 3: Components - Sentiment Score API
        component sentimentScoreAPI "SentimentAPIComponents" {
            include *
            autoLayout
        }
        
        # Deployment view
        deployment newsSentimentSystem "Production" "ProductionDeployment" {
            deploymentNode "Azure Cloud" {
                deploymentNode "Azure Kubernetes Service" {
                    deploymentNode "News Processing Cluster" {
                        containerInstance dataIngestionService
                        containerInstance newsProcessorService
                        containerInstance sentimentScoreAPI
                    }
                }
                
                deploymentNode "Azure PaaS Services" {
                    deploymentNode "Azure Cosmos DB" {
                        containerInstance cosmosDB
                    }
                    deploymentNode "Azure Cache for Redis" {
                        containerInstance redis
                    }
                    deploymentNode "Azure Service Bus" {
                        containerInstance messageQueue
                    }
                }
            }
        }
        
        # Styling
        styles {
            element "External System" {
                background #999999
                color #ffffff
            }
            element "Internal System" {
                background #1168bd
                color #ffffff
            }
            element "Person" {
                shape person
                background #08427b
                color #ffffff
            }
            element "Container" {
                background #438dd5
                color #ffffff
            }
            element "Component" {
                background #85bbf0
                color #000000
            }
            element "Database" {
                shape cylinder
            }
            element "Queue" {
                shape pipe
            }
        }
    }
} 