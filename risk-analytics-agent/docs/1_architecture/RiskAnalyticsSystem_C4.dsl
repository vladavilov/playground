workspace "Risk Analytics System" "A C4 model for the Risk Analytics System, focusing on the Orchestrator Service." {

    model {
        dataScientist = person "Data Scientist" "A data scientist who needs to generate historical datasets for ML model training." {
            tags "Internal"
        }

        clientApp = softwareSystem "Client Application" "The web or desktop application used by the End User to interact with the system." {
            tags "External"
        }
        storageSystem = softwareSystem "Artifact Storage" "A long-term storage system, like an S3 bucket or network share, for storing generated training data." {
            tags "External, Database"
        }

        riskAnalyticsSystem = softwareSystem "Risk Analytics System" "The complete microservices-based platform for performing financial risk analysis." {

            orchestrator = container "Orchestrator Service" "Acts as the central coordinator for the entire risk analytics workflow." {
                tags "Microservice"
                
                group "Orchestrator Service Internals" {
                    apiController = component "API Controller" "Exposes REST endpoints for on-demand analysis and training data jobs." {
                        tags "Component"
                    }
                    onDemandManager = component "OnDemand Workflow Manager" "Manages the sequence of service calls for the on-demand analysis workflow." {
                        tags "Component"
                    }
                    trainingManager = component "TrainingData Workflow Manager" "Manages the sequence of service calls for training data preparation workflows." {
                        tags "Component"
                    }
                    serviceClient = component "Service Client" "A generic client for making gRPC calls to other microservices." {
                        tags "Component"
                    }
                }
            }

            finCalculations = container "FinCalculations Service" "Provides core Financial Data Objects for instruments." "Python" {
                tags "Microservice, Data Source"
            }
            newsSentiment = container "NewsSentiment Service" "Provides news sentiment analysis for a given instrument." "Python" {
                tags "Microservice, Data Source"
            }
            repoData = container "RepoData Service" "Provides cost of carry (repo) data for instruments." "Python" {
                tags "Microservice, Data Source"
            }
            ownershipData = container "OwnershipData Service" "Provides ownership concentration data for instruments." "Python" {
                tags "Microservice, Data Source"
            }
            marketDataFeed = container "MarketDataFeed" "Provides general and historical market indicators." {
                tags "Microservice, Data Source"
            }
            economicCalendar = container "EconomicCalendar Service" "Provides a matrix of upcoming economic events." "Python" {
                tags "Microservice, Data Source"
            }

            marketRegime = container "MarketRegime Service" "An ML model that determines the current market regime based on indicators." "Python, ML" {
                tags "Microservice, ML Model"
            }
            riskPrediction = container "RiskPrediction Model" "An ML model that predicts risk for an instrument." "Python, ML" {
                tags "Microservice, ML Model"
            }
            
            riskSynthesis = container "RiskSynthesis Service" "Assembles all data points into a final, structured JSON output with narrative." "Python" {
                tags "Microservice, Synthesis"
            }
        }

        clientApp -> riskAnalyticsSystem "Requests on-demand instrument analysis" "JSON/HTTPS"
        riskAnalyticsSystem -> clientApp "Returns comprehensive analysis" "JSON/HTTPS"
        dataScientist -> riskAnalyticsSystem "Triggers historical data preparation job" "JSON/HTTPS"
        riskAnalyticsSystem -> storageSystem "Stores generated training data" "File I/O"
        storageSystem -> dataScientist "Consumes training data for model development" "File I/O"

        clientApp -> apiController "GET /analyze/instrument/{cusip}" "JSON/HTTPS"
        dataScientist -> apiController "POST /jobs/prepare-training-data" "JSON/HTTPS"
        
        onDemandManager -> finCalculations "Gets FinancialDataObject" "gRPC"
        onDemandManager -> newsSentiment "Gets news_sentiment" "gRPC"
        onDemandManager -> repoData "Gets cost_of_carry_bps" "gRPC"
        onDemandManager -> ownershipData "Gets ownership_concentration" "gRPC"
        onDemandManager -> marketDataFeed "Gets market_indicators" "gRPC"
        onDemandManager -> economicCalendar "Gets events_matrix" "gRPC"
        onDemandManager -> marketRegime "Gets market regime" "gRPC"
        onDemandManager -> riskPrediction "Gets risk prediction" "gRPC"
        onDemandManager -> riskSynthesis "Sends consolidated data, gets final analysis" "gRPC"
        
        trainingManager -> storageSystem "Saves training data file" "S3 API"
        trainingManager -> economicCalendar "Gets historical events_matrix" "gRPC"
        trainingManager -> marketDataFeed "Gets historical market data" "gRPC"
        trainingManager -> finCalculations "Gets historical financial data" "gRPC"
        trainingManager -> newsSentiment "Gets historical sentiment data" "gRPC"

        apiController -> onDemandManager "Delegates on-demand analysis request"
        apiController -> trainingManager "Delegates training data request"
        
        trainingManager -> trainingManager "Joins and enriches datasets" "In-memory processing"

        onDemandManager -> serviceClient "Makes calls to downstream services"
        trainingManager -> serviceClient "Makes calls to downstream services"
        
        serviceClient -> finCalculations " " "gRPC"
        serviceClient -> newsSentiment " " "gRPC"
        serviceClient -> repoData " " "gRPC"
        serviceClient -> ownershipData " " "gRPC"
        serviceClient -> marketDataFeed " " "gRPC"
        serviceClient -> economicCalendar " " "gRPC"
        serviceClient -> marketRegime " " "gRPC"
        serviceClient -> riskPrediction " " "gRPC"
        serviceClient -> riskSynthesis " " "gRPC"

    }

    views {
        systemContext riskAnalyticsSystem "SystemContext" "The system context diagram for the Risk Analytics System." {
            include *
            autoLayout lr
            title "System Context: Risk Analytics System"
        }

        container riskAnalyticsSystem "Containers" "The container diagram for the Risk Analytics System." {
            include *
            autoLayout lr
            title "Container Diagram: Risk Analytics System"
        }
        
        component orchestrator "Components" "The component diagram for the Orchestrator Service." {
            include *
            autoLayout lr
            title "Component Diagram: Orchestrator Service"
        }

        dynamic orchestrator "OnDemandAnalysis" "Illustrates the 'On-Demand Instrument Analysis' workflow." {
            clientApp -> apiController "Request analysis for CUSIP"
            apiController -> onDemandManager "Delegate to workflow manager"

            // Initial Fan-Out (Parallel Execution)
            {
                onDemandManager -> finCalculations "Get FinancialDataObject"
                onDemandManager -> newsSentiment "Get news_sentiment"
                onDemandManager -> repoData "Get cost_of_carry_bps"
                onDemandManager -> ownershipData "Get ownership_concentration"
                onDemandManager -> marketDataFeed "Get market_indicators"
                onDemandManager -> economicCalendar "Get events_matrix"
            }
            
            // First Level Synthesis
            onDemandManager -> marketRegime "Get market regime"
            
            // Second Level Synthesis
            onDemandManager -> riskPrediction "Get risk prediction"

            // Final Assembly and Synthesis
            onDemandManager -> riskSynthesis "Send consolidated data"
            riskSynthesis -> onDemandManager "Return final analysis object"

            onDemandManager -> apiController "Return final analysis"
            apiController -> clientApp "Return final analysis"
            
            autoLayout
            title "Dynamic View: On-Demand Instrument Analysis"
        }

        dynamic orchestrator "RiskPredictionTraining" "Illustrates the 'Risk Prediction Training Data' workflow." {
            dataScientist -> apiController "POST request to start job"
            apiController -> trainingManager "Delegate to workflow manager"

            // Fan-out to collect historical data
            {
                 trainingManager -> economicCalendar "Get events_matrix for range"
                 trainingManager -> marketDataFeed "Get historical market data"
                 trainingManager -> finCalculations "Get historical financial data"
                 trainingManager -> newsSentiment "Get historical sentiment data"
            }
            
            trainingManager -> trainingManager "Join all datasets"
            trainingManager -> storageSystem "Save enriched dataset to artifact storage"
            apiController -> dataScientist "Return job ID and artifact location"
            
            autoLayout
            title "Dynamic View: Risk Prediction Training Data Workflow"
        }

        styles {
            element "Person" {
                shape Person
                background #08427b
                color #ffffff
            }
            element "External" {
                background #999999
            }
            element "Software System" {
                background #1168bd
                color #ffffff
            }
            element "Microservice" {
                shape Hexagon
                background #2d882d
                color #ffffff
            }
            element "Data Source" {
                background #8f4f09
            }
            element "ML Model" {
                background #9000ff
            }
            element "Synthesis" {
                background #438dd5
            }
            element "Component" {
                shape Component
                background #85bbf0
                color #000000
            }
            element "Database" {
                shape Cylinder
            }
        }
    }
}