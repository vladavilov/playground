from langchain_openai import AzureChatOpenAI, AzureOpenAIEmbeddings
from neo4j import GraphDatabase, Session

from config import get_retrieval_settings


def get_llm(use_fast_model: bool = True) -> AzureChatOpenAI:
    """Get LLM client configured for retrieval tasks.
    
    Args:
        use_fast_model: If True, uses OAI_MODEL_FAST (default for retrieval tasks)
    
    Returns:
        Configured AzureChatOpenAI instance
    """
    settings = get_retrieval_settings()
    model = settings.llm.OAI_MODEL_FAST if use_fast_model else settings.llm.OAI_MODEL
    return AzureChatOpenAI(
        azure_endpoint=settings.llm.OAI_BASE_URL,
        deployment_name=model,
        api_key=settings.llm.OAI_KEY,
        api_version=settings.llm.OAI_API_VERSION,
        timeout=settings.llm.LLM_TIMEOUT_SEC,
        temperature=settings.llm.LLM_TEMPERATURE,
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


