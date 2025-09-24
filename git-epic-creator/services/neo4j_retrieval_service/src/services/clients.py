from typing import Iterator

from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from neo4j import GraphDatabase, Session

from ..config import get_retrieval_settings


def get_llm(temperature: float | None = None) -> ChatOpenAI:
    settings = get_retrieval_settings()
    kwargs: dict = {
        "model": settings.llm.OAI_MODEL,
        "timeout": settings.OAI_TIMEOUT_SEC,
        "default_query": {"api-version": settings.llm.OAI_API_VERSION},
        "base_url": settings.llm.OAI_BASE_URL,
        "api_key": settings.llm.OAI_KEY,
        "temperature": (
            float(temperature) if temperature is not None else float(settings.LLM_TEMPERATURE)
        ),
    }
    return ChatOpenAI(**kwargs)


def get_embedder() -> OpenAIEmbeddings:
    settings = get_retrieval_settings()
    kwargs: dict = {
        "model": settings.llm.OAI_EMBED_MODEL,
        "base_url": settings.llm.OAI_BASE_URL,
        "api_key": settings.llm.OAI_KEY,
    }
    return OpenAIEmbeddings(**kwargs)


def get_neo4j_session() -> Session:
    settings = get_retrieval_settings()
    driver = GraphDatabase.driver(
        settings.neo4j.NEO4J_URI, auth=(settings.neo4j.NEO4J_USERNAME, settings.neo4j.NEO4J_PASSWORD)
    )
    return driver.session(database=settings.neo4j.NEO4J_DATABASE)


