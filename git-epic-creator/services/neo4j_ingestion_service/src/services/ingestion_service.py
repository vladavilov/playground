import logging

from utils.neo4j_client import Neo4jClient

class Neo4jIngestionService:
    
    def __init__(self, client: Neo4jClient):
        self.client = client
        self.logger = logging.getLogger(__name__)

    def ingest():
        pass