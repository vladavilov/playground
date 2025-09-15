from typing import Iterator

import httpx
from neo4j import GraphDatabase, Session

from ..config import get_retrieval_settings


def get_oai_client() -> httpx.Client:
    settings = get_retrieval_settings()
    headers = {"Authorization": f"Bearer {settings.OAI_KEY}"}
    return httpx.Client(
        base_url=settings.OAI_BASE_URL.rstrip("/"), headers=headers, timeout=settings.OAI_TIMEOUT_SEC
    )


def get_neo4j_session() -> Session:
    settings = get_retrieval_settings()
    driver = GraphDatabase.driver(
        settings.NEO4J_URI, auth=(settings.NEO4J_USERNAME, settings.NEO4J_PASSWORD)
    )
    return driver.session(database=settings.NEO4J_DATABASE)


