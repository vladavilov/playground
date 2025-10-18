from langchain_openai import AzureChatOpenAI, AzureOpenAIEmbeddings
from neo4j import GraphDatabase, Session

from config import get_retrieval_settings


def get_llm(temperature: float | None = None) -> AzureChatOpenAI:
    settings = get_retrieval_settings()
    return AzureChatOpenAI(
        azure_endpoint=settings.llm.OAI_BASE_URL,
        deployment_name=settings.llm.OAI_MODEL,
        api_key=settings.llm.OAI_KEY,
        api_version=settings.llm.OAI_API_VERSION,
        timeout=settings.OAI_TIMEOUT_SEC,
        temperature=float(temperature) if temperature is not None else float(settings.LLM_TEMPERATURE),
    )


def get_embedder() -> AzureOpenAIEmbeddings:
    settings = get_retrieval_settings()
    return AzureOpenAIEmbeddings(
        azure_endpoint=settings.llm.OAI_BASE_URL,
        azure_deployment=settings.llm.embedding_deployment_name,
        api_key=settings.llm.OAI_KEY,
        api_version=settings.llm.OAI_API_VERSION,
        model=settings.llm.OAI_EMBED_MODEL_NAME,
    )


def get_neo4j_session() -> Session:
    settings = get_retrieval_settings()
    driver = GraphDatabase.driver(
        settings.neo4j.NEO4J_URI, auth=(settings.neo4j.NEO4J_USERNAME, settings.neo4j.NEO4J_PASSWORD)
    )
    return driver.session(database=settings.neo4j.NEO4J_DATABASE)


