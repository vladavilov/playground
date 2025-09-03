# Community Analysis Pipeline

This document describes the community detection and summarization functionality added to the Neo4j ingestion service.

## Overview

The community analysis pipeline runs automatically after the GraphRAG ingestion completes. It performs two main steps:

1. **Community Detection**: Uses Neo4j Graph Data Science (GDS) library to detect communities in the knowledge graph
2. **Community Summarization**: Generates LLM summaries for each detected community

## Architecture

### Components

- **CommunityDetectionService**: Executes Cypher scripts for community detection
- **CommunitySummarizerService**: Generates LLM summaries for communities
- **Cypher Scripts**: Four scripts that implement the community detection algorithm

### Flow

```
GraphRAG Ingestion → Community Detection → Community Summarization → Updated Graph
```

## Community Detection Algorithm

The pipeline uses the **Leiden algorithm** from Neo4j GDS with the following steps:

### 1. Graph Projection
```cypher
CALL gds.graph.project(
  "communities",
  __Entity__,
  {
    _ALL_: {
      type: "*",
      orientation: "UNDIRECTED",
      properties: {weight: {property: "*", aggregation: "COUNT"}}
    }
  }
)
```

### 2. Community Detection
```cypher
CALL gds.leiden.write(
  "communities",
  {
    writeProperty: "communities",
    includeIntermediateCommunities: True,
    relationshipWeightProperty: "weight"
  }
);
```

### 3. Community Node Creation
Creates hierarchical `__Community__` nodes with `IN_COMMUNITY` relationships.

### 4. Community Retrieval
Retrieves communities and their contents for summarization using APOC procedures.

## Community Summarization

Each community is summarized using the configured LLM with a specialized prompt that:

- Analyzes the community's nodes and relationships
- Identifies the main theme and purpose
- Provides a concise, informative summary
- Updates the community node with the summary

## Configuration

The community analysis uses the same LLM configuration as the GraphRAG pipeline:

- **Model**: Configurable via `OAI_MODEL` environment variable
- **API Completion Key**: Configurable via `OAI_COMPLETION_KEY` environment variable
- **API Embeddings Key**: Configurable via `OAI_EMBEDDIGNS_KEY` environment variable
- **Completion URL**: Configurable via `OAI_COMPLETION_URL` environment variable
- **Embeddings URL**: Configurable via `OAI_EMBEDDINGS_URL` environment variable

## Dependencies

### Neo4j Requirements
- Neo4j 5.x or higher
- Graph Data Science (GDS) library installed
- APOC procedures enabled

### Python Dependencies
- `neo4j` driver
- `neo4j_graphrag` library
- OpenAI-compatible LLM client

## Usage

The community analysis runs automatically after each document ingestion. No additional configuration is required.

### Manual Execution

```python
from ingestion.community_detection import CommunityDetectionService
from ingestion.community_summarizer import CommunitySummarizerService

# Run community detection
detector = CommunityDetectionService(driver)
result = await detector.run_community_detection()

# Run community summarization
communities = detector.get_communities_for_summarization()
summarizer = CommunitySummarizerService(driver)
summary_result = await summarizer.summarize_communities(communities)
```

## Output

### Community Detection Results
- Number of communities created
- Hierarchical community structure
- Entity-community relationships

### Community Summarization Results
- LLM-generated summaries for each community
- Summary stored as `summary` property on community nodes
- Timestamp of summarization stored as `summarized_at`

## Error Handling

- Community detection failures are logged but don't stop the pipeline
- Individual community summarization failures are logged and tracked
- Comprehensive error reporting and metrics

## Performance Considerations

- Community detection scales with graph size
- LLM summarization can be rate-limited based on API constraints
- Consider batching for large numbers of communities

## Monitoring

The pipeline provides detailed logging for:
- Community detection progress
- Summarization success/failure rates
- Performance metrics
- Error details

## Troubleshooting

### Common Issues

1. **GDS Library Not Available**: Ensure Neo4j GDS is installed and enabled
2. **APOC Procedures Missing**: Verify APOC procedures are available
3. **LLM API Errors**: Check API key and endpoint configuration
4. **Memory Issues**: Large graphs may require increased Neo4j memory

### Debug Mode

Enable debug logging to see detailed execution information:

```python
import logging
logging.getLogger('ingestion.community_detection').setLevel(logging.DEBUG)
logging.getLogger('ingestion.community_summarizer').setLevel(logging.DEBUG)
```

## Future Enhancements

- Configurable community detection algorithms
- Custom summarization prompts
- Community quality metrics
- Automated community optimization
- Integration with downstream retrieval services
