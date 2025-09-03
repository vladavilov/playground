import asyncio
import logging
import os
import json
from pathlib import Path

from neo4j import Driver

from neo4j_graphrag.embeddings import OpenAIEmbeddings
from neo4j_graphrag.llm import OpenAILLM, AzureOpenAILLM
from neo4j_graphrag.experimental.pipeline.kg_builder import SimpleKGPipeline
from neo4j_graphrag.indexes import create_vector_index

from configuration.graphrag_config import get_graphrag_settings
from .community_detection import CommunityDetectionService
from .community_summarizer import CommunitySummarizerService

_settings = get_graphrag_settings()
DEFAULT_EMBEDDING_MODEL = _settings.OAI_EMBED_MODEL
DEFAULT_CHAT_MODEL = _settings.OAI_MODEL
OPENAI_API_KEY = _settings.OAI_KEY
OPENAI_BASE_URL = _settings.OAI_BASE_URL
AZURE_API_VERSION = _settings.OAI_API_VERSION

VECTOR_INDEX_NAME = "graphrag_index"
VECTOR_LABEL = "Chunk"
VECTOR_PROPERTY = "embedding"
VECTOR_DIMENSIONS = 1536


DEFAULT_FINANCE_SCHEMA: dict[str, object] = {
	"node_types": [
		# Core
		"PROJECT",
		"DOCUMENT",
		"FILE",
		"REQUIREMENT",
		"ENTITY",
		# Trading/Markets
		"ORDER",
		"TRADE",
		"INSTRUMENT",
		"EQUITY",
		"BOND",
		"DERIVATIVE",
		"OPTION",
		"FUTURE",
		"SWAP",
		"FX_PAIR",
		"PORTFOLIO",
		"STRATEGY",
		"ALPHA_MODEL",
		"RISK_MODEL",
		"BENCHMARK",
		"INDEX",
		"ETF",
		"FUND",
		"BROKER",
		"EXCHANGE",
		"VENUE",
		"DARK_POOL",
		"ORDER_BOOK",
		"QUOTE",
		"TICKER",
		"BAR",
		"DEPTH",
		"AUCTION",
		"CCP",
		"CSD",
		"CLEARING_MEMBER",
		"MARGIN_CALL",
		"SETTLEMENT_INSTRUCTION",
		"CASH_MOVEMENT",
		"CORPORATE_ACTION",
		"DIVIDEND",
		"SPLIT",
		"MERGER",
		"RATING",
		"RATING_AGENCY",
		"ANALYST_REPORT",
		"PROSPECTUS",
		"TERM_SHEET",
		"REFERENCE_DATA",
		"FIGI",
		"ISIN",
		"CUSIP",
		"RIC",
		"BLOOMBERG_FIELD",
		"REUTERS_FIELD",
		"CALENDAR",
		"HOLIDAY",
		"CURRENCY",
		"COUNTRY",
		"REGION",
		# Wealth/Client
		"CLIENT",
		"ACCOUNT",
		"RELATIONSHIP",
		"ADVISOR",
		"MANDATE",
		"MODEL_PORTFOLIO",
		"REBALANCE",
		"FEE_SCHEDULE",
		"GOAL",
		"SUITABILITY_PROFILE",
		"RISK_TOLERANCE",
		"KYC_PROFILE",
		"AML_ALERT",
		# Risk/Reg/Reporting
		"VaR_REPORT",
		"STRESS_TEST",
		"LIMIT",
		"BREACH",
		"CONTROL",
		"CONTROL_TEST",
		"POLICY",
		"PROCEDURE",
		"MI_FID_REPORT",
		"FINRA_REPORT",
		"AUDIT",
		"FINDING",
		"REMEDIATION",
		"ISO20022_MESSAGE",
		"SWIFT_MESSAGE",
		"PAYMENT",
		"LEDGER_ENTRY",
		"GL_ACCOUNT",
		# Data/Platform/IT
		"SERVICE",
		"API",
		"MICROSERVICE",
		"EVENT",
		"TOPIC",
		"QUEUE",
		"ETL_JOB",
		"BATCH",
		"DATA_PIPELINE",
		"DAG",
		"DATA_CATALOG",
		"DATA_LAKE",
		"DATA_WAREHOUSE",
		"TABLE",
		"COLUMN",
		"SCHEMA_VERSION",
		"NOTEBOOK",
		"DASHBOARD",
		"REPORT",
		"KPI",
		"ALERT",
		"INCIDENT",
		"TICKET",
		"CHANGE_REQUEST",
		"DEPLOYMENT",
		"RELEASE",
		"FEATURE_FLAG",
		"SECRET",
		"VAULT",
		"KEY",
		"CERTIFICATE",
		"IAM_ROLE",
		"USER",
		"GROUP",
		"TENANT",
		"ENVIRONMENT",
		"NAMESPACE",
		"CLUSTER",
		"NODE_HOST",
		"POD",
		"CONTAINER",
		"IMAGE",
		"REPOSITORY",
		"COMMIT",
		"CI_JOB",
		"CD_PIPELINE",
		"TEST_SUITE",
		"TEST_CASE",
		"COVERAGE",
		"LOG",
		"METRIC",
		"TRACE",
		"CONFIG",
		"FEATURE_TOGGLE",
		"MONITOR",
		"SLO",
		"SLA",
		"SYNTHETIC_CHECK",
		"CONNECTOR",
		"INGESTION_JOB",
		"TRANSFORM_JOB",
		"LOAD_JOB",
		# Organizations
		"COMPANY",
		"SUBSIDIARY",
		"COUNTERPARTY",
		"CUSTODIAN",
		"ADMINISTRATOR",
		"PRIME_BROKER",
		"MARKET_DATA_FEED",
		"NEWS_FEED",
	],
	"relationship_types": [
		# Core chain
		"PROJECT_HAS_DOCUMENT",
		"PROJECT_HAS_FILE",
		"DOCUMENT_HAS_REQUIREMENT",
		"FILE_INCLUDES_REQUIREMENT",
		"REQUIREMENT_IS_INCLUDED_IN_FILE",
		"REQUIREMENT_MENTIONS_ENTITY",
		"ENTITY_RELATES_TO_ENTITY",
		"ENTITY_SUPPORTS_REQUIREMENT",
		# Trading flows
		"TRADES_ON",
		"ROUTES_VIA",
		"EXECUTED_AT",
		"CLEARED_BY",
		"SETTLED_AT",
		"CUSTODIED_BY",
		"ALLOCATED_TO",
		"HEDGES",
		"OWNS",
		"BENCHMARKED_BY",
		"FOLLOWS_STRATEGY",
		"RUNS_MODEL",
		"CALCULATED_BY",
		"VALUED_BY",
		# Market data
		"HAS_TICKER",
		"HAS_QUOTE",
		"HAS_BAR",
		"HAS_ORDER_BOOK",
		"PUBLISHES",
		"SUBSCRIBES",
		"INGESTS",
		# Wealth
		"MANAGED_BY",
		"ADVISED_BY",
		"BELONGS_TO",
		"HAS_MANDATE",
		"USES_MODEL_PORTFOLIO",
		"USED_BY",
		"REBALANCED_BY",
		"FEES_DETERMINED_BY",
		"SUITABILITY_ASSESSED_BY",
		# Risk/Reg
		"COMPLIES_WITH",
		"VIOLATES",
		"REPORTS_TO",
		"FILES_WITH",
		"LIMIT_OF",
		"BREACHED_BY",
		"TESTED_BY",
		"CONTROLS",
		"FINDING_OF",
		"REMEDIATES",
		# Data/IT
		"DEPENDS_ON",
		"CALLS",
		"EMITS",
		"CONSUMES",
		"PRODUCES",
		"RUNS_ON",
		"DEPLOYED_TO",
		"MONITORED_BY",
		"ALERTS_ON",
		"LOGS_TO",
		"TRACES_TO",
		"CONFIGURES",
		"PARAMETER_OF",
		"VERSION_OF",
		"SUPERSEDES",
		"ENRICHED_BY",
		"SOURCED_FROM",
		"MAPS_TO",
		"TRANSFORMS_TO",
		"LOADS_TO",
		"SECURED_BY",
		"ENCRYPTED_BY",
		"AUTHENTICATED_BY",
		"AUTHORIZED_BY",
		# Finance ops
		"RESULTS_IN",
		"CORPORATE_ACTION_OF",
		"PAYS",
		"RECEIVES",
		"POSTED_TO",
		"NETTED_BY",
	],
	"patterns": [
		("PROJECT", "PROJECT_HAS_DOCUMENT", "DOCUMENT"),
		("PROJECT", "PROJECT_HAS_FILE", "FILE"),
		("DOCUMENT", "DOCUMENT_HAS_REQUIREMENT", "REQUIREMENT"),
		("FILE", "FILE_INCLUDES_REQUIREMENT", "REQUIREMENT"),
		("REQUIREMENT", "REQUIREMENT_IS_INCLUDED_IN_FILE", "FILE"),
		("REQUIREMENT", "REQUIREMENT_MENTIONS_ENTITY", "ENTITY"),
		("ENTITY", "ENTITY_RELATES_TO_ENTITY", "ENTITY"),
		# Representative finance/IT links
		("ORDER", "TRADES_ON", "VENUE"),
		("TRADE", "EXECUTED_AT", "EXCHANGE"),
		("TRADE", "CLEARED_BY", "CCP"),
		("TRADE", "SETTLED_AT", "CSD"),
		("PORTFOLIO", "BENCHMARKED_BY", "BENCHMARK"),
		("CLIENT", "MANAGED_BY", "ADVISOR"),
		("ACCOUNT", "BELONGS_TO", "CLIENT"),
		("MODEL_PORTFOLIO", "USED_BY", "ACCOUNT"),
		("SERVICE", "DEPENDS_ON", "SERVICE"),
		("MICROSERVICE", "CALLS", "API"),
		("ETL_JOB", "PRODUCES", "TABLE"),
		("CONNECTOR", "INGESTS", "REFERENCE_DATA"),
	],
	# Keep labels/relations/patterns unlimited to allow LLM extraction beyond the seed schema
	"additional_node_types": True,
	"additional_relationship_types": True,
	"additional_patterns": True,
	"additional_properties": True,
}


def _normalize_base_url(url: str) -> str:
	value = url.strip()
	if not value:
		return value
	value = value.rstrip("/")
	if not value.endswith("/v1"):
		value = f"{value}/v1"
	return value


def _apply_base_url_env() -> None:
	if OPENAI_BASE_URL:
		normalized = _normalize_base_url(OPENAI_BASE_URL)
		os.environ["OPENAI_BASE_URL"] = normalized


def _build_embedder() -> OpenAIEmbeddings:
	# Export env for clients/wrappers that read base URL from environment
	_apply_base_url_env()
	kwargs: dict[str, object] = {"model": DEFAULT_EMBEDDING_MODEL, "api_key": OPENAI_API_KEY}
	if OPENAI_BASE_URL:
		kwargs["base_url"] = os.environ.get("OPENAI_BASE_URL", OPENAI_BASE_URL)
	try:
		return OpenAIEmbeddings(**kwargs)  # type: ignore[arg-type]
	except TypeError:
		# Older versions may not accept base_url; fall back to env-only usage
		return OpenAIEmbeddings(model=DEFAULT_EMBEDDING_MODEL, api_key=OPENAI_API_KEY)


def _build_llm() -> OpenAILLM:
	# Export env for clients/wrappers that read base URL from environment
	_apply_base_url_env()
	# If an Azure API version is provided, treat OAI_BASE_URL as the Azure endpoint
	if AZURE_API_VERSION and OPENAI_BASE_URL:
		endpoint = OPENAI_BASE_URL.rstrip("/") + "/"
		version = AZURE_API_VERSION or "2024-06-01"
		return AzureOpenAILLM(
			model_name=DEFAULT_CHAT_MODEL,
			azure_endpoint=endpoint,
			api_version=version,
			api_key=OPENAI_API_KEY,
			model_params={"temperature": 0},
		)
	kwargs: dict[str, object] = {
		"model_name": DEFAULT_CHAT_MODEL,
		"model_params": {"temperature": 0},
		"api_key": OPENAI_API_KEY,
	}
	if OPENAI_BASE_URL:
		kwargs["base_url"] = os.environ.get("OPENAI_BASE_URL", OPENAI_BASE_URL)
	try:
		return OpenAILLM(**kwargs)  # type: ignore[arg-type]
	except TypeError:
		return OpenAILLM(model_name=DEFAULT_CHAT_MODEL, model_params={"temperature": 0}, api_key=OPENAI_API_KEY)


def _prepare_document_for_ingestion(json_content: str, file_path: str, project_id: str) -> dict:
	"""
	Prepare document data for GraphRAG pipeline ingestion.
	
	Args:
		json_content: JSON string content
		file_path: Path to the JSON file
		project_id: Project identifier
		
	Returns:
		Dictionary with document data and metadata for ingestion
	"""
	try:
		payload = json.loads(json_content)
		if isinstance(payload, dict):
			# Extract metadata if available
			metadata = payload.get("metadata", {})
			
			# Prepare document with rich metadata
			document = {
				"text": payload.get("text", ""),
				"path": file_path,
				"title": payload.get("title", Path(file_path).name),
				"project_id": project_id,
				"file_name": metadata.get("file_name", Path(file_path).name),
				"file_type": metadata.get("file_type", "unknown"),
				"content_type": metadata.get("content_type", "application/octet-stream"),
				"creation_date": metadata.get("creation_date"),
				"modification_date": metadata.get("modification_date")
			}
			
			# Add any additional metadata fields
			if "author" in metadata:
				document["author"] = metadata["author"]
			if "category" in metadata:
				document["category"] = metadata["category"]
			if "version" in metadata:
				document["version"] = metadata["version"]
			if "tags" in metadata:
				document["tags"] = metadata["tags"]
				
			return document
	except (json.JSONDecodeError, KeyError):
		pass
	
	# Fallback to basic document info
	return {
		"text": "",
		"path": file_path,
		"title": Path(file_path).name,
		"project_id": project_id,
		"file_name": Path(file_path).name,
		"file_type": "unknown",
		"content_type": "application/octet-stream"
	}





def ensure_vector_index(driver: Driver) -> None:
	create_vector_index(
		driver,
		VECTOR_INDEX_NAME,
		label=VECTOR_LABEL,
		embedding_property=VECTOR_PROPERTY,
		dimensions=VECTOR_DIMENSIONS,
		similarity_fn="cosine",
		fail_if_exists=False,
	)


async def run_documents(driver: Driver, documents: list[dict], schema: dict[str, object] | None = None) -> None:
	"""
	Run GraphRAG pipeline with a list of prepared documents.
	
	Args:
		driver: Neo4j driver instance
		documents: List of document dictionaries with metadata
		schema: Optional schema configuration
	"""
	if not documents or not isinstance(documents, list):
		raise ValueError("documents must be a non-empty list")
	
	if not all(isinstance(doc, dict) and "text" in doc for doc in documents):
		raise ValueError("all documents must be dictionaries with 'text' field")

	ensure_vector_index(driver)
	schema_to_use = schema or DEFAULT_FINANCE_SCHEMA
	
	kg_builder = SimpleKGPipeline(
		llm=_build_llm(),
		driver=driver,
		embedder=_build_embedder(),
		schema=schema_to_use,
		from_pdf=False,
	)
	
	documents_json = json.dumps(documents, ensure_ascii=False, indent=2)
	await kg_builder.run_async(text=documents_json)


async def run_community_analysis(driver: Driver) -> dict[str, object]:
	"""
	Run community detection and summarization pipeline.
	
	Args:
		driver: Neo4j driver instance
		
	Returns:
		Dictionary containing community analysis results and metrics
	"""
	logger = logging.getLogger(__name__)
	logger.info("Starting community analysis pipeline")
	
	try:
		# Step 1: Community Detection
		community_detector = CommunityDetectionService(driver)
		detection_result = await community_detector.run_community_detection()
		
		if not detection_result.get("success"):
			logger.error("Community detection failed")
			return detection_result
		
		logger.info(f"Community detection completed. Found {detection_result.get('communities_created', 0)} communities.")
		
		# Step 2: Community Summarization
		ran_levels = detection_result.get("ran_levels", 0)
		communities = community_detector.get_communities_for_summarization(max_levels=ran_levels)
		
		if not communities:
			logger.warning("No communities found for summarization")
			return {
				"success": True,
				"communities_detected": 0,
				"communities_summarized": 0,
				"ran_levels": ran_levels,
				"message": "No communities found to summarize"
			}
		
		community_summarizer = CommunitySummarizerService(driver)
		summarization_result = await community_summarizer.summarize_communities(communities)
		
		if not summarization_result.get("success"):
			logger.error("Community summarization failed")
			return summarization_result
		
		logger.info(f"Community analysis completed successfully. {summarization_result.get('successful_summaries', 0)} communities summarized.")
		
		return {
			"success": True,
			"communities_detected": detection_result.get("communities_created", 0),
			"communities_summarized": summarization_result.get("successful_summaries", 0),
			"ran_levels": detection_result.get("ran_levels", 0),
			"detection_result": detection_result,
			"summarization_result": summarization_result
		}
		
	except Exception as e:
		logger.error(f"Community analysis pipeline failed: {e}")
		return {
			"success": False,
			"error": str(e)
		}


def prepare_document_from_json(file_path: str, project_id: str) -> dict:
	"""
	Prepare a single document from JSON file for ingestion.
	
	Args:
		file_path: Path to the JSON file
		project_id: Project identifier
		
	Returns:
		Dictionary with prepared document data and metadata
	"""
	if not isinstance(file_path, str) or not file_path:
		raise ValueError("file_path must be a non-empty string")

	with open(file_path, "r", encoding="utf-8") as f:
		content = f.read()

	# Prepare document with metadata and project ID
	document = _prepare_document_for_ingestion(content, file_path, project_id)
	
	# Inject project_id into the text content if it's JSON
	try:
		payload = json.loads(content)
		if isinstance(payload, dict):
			payload["project_id"] = project_id
			document["text"] = json.dumps(payload, ensure_ascii=False)
	except (json.JSONDecodeError, KeyError):
		# If not JSON, just add project_id to the document metadata
		pass
	
	return document
