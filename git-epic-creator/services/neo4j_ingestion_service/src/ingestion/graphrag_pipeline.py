import asyncio
import logging
import os
import json
from pathlib import Path

from neo4j import Driver

from neo4j_graphrag.experimental.pipeline.config.runner import PipelineRunner
from neo4j_graphrag.indexes import create_vector_index
from .community_detection import CommunityDetectionService
from .community_summarizer import CommunitySummarizerService
import yaml

def _load_yaml_config(config_path: str) -> dict | None:
	"""
	Load YAML configuration from a file path.
	Returns a dictionary or None if parsing fails.
	"""
	try:
		with open(config_path, "r", encoding="utf-8") as f:
			return yaml.safe_load(f)
	except Exception:
		return None


def _resolve_vector_index_config(config_data: dict | None) -> dict:
	"""
	Resolve vector index configuration from YAML config data with safe defaults.
	"""
	defaults = {
		"name": "chunk_embeddings",
		"label": "Chunk",
		"property": "embedding",
		"dimensions": 1536,
		"similarity": "cosine",
	}
	if not isinstance(config_data, dict):
		return defaults
	index_cfg = config_data.get("vector_index", {}) or {}
	return {
		"name": index_cfg.get("name", defaults["name"]),
		"label": index_cfg.get("label", defaults["label"]),
		"property": index_cfg.get("property", defaults["property"]),
		"dimensions": int(index_cfg.get("dimensions", defaults["dimensions"])),
		"similarity": index_cfg.get("similarity", defaults["similarity"]),
	}

def _resolve_community_index_config(config_data: dict | None) -> dict:
	"""
	Resolve community summary vector index configuration with safe defaults.
	Defaults: name 'community_summary_idx', label '__Community__', property 'summary_embedding'.
	"""
	defaults = {
		"name": "community_summary_idx",
		"label": "__Community__",
		"property": "summary_embedding",
		"dimensions": 1536,
		"similarity": "cosine",
	}
	if not isinstance(config_data, dict):
		return defaults
	index_cfg = (config_data.get("community_vector_index") or {})
	return {
		"name": index_cfg.get("name", defaults["name"]),
		"label": index_cfg.get("label", defaults["label"]),
		"property": index_cfg.get("property", defaults["property"]),
		"dimensions": int(index_cfg.get("dimensions", defaults["dimensions"])),
		"similarity": index_cfg.get("similarity", defaults["similarity"]),
	}

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

def ensure_vector_index(driver: Driver, index_cfg: dict) -> None:
	"""
	Ensure the Neo4j vector index exists using the provided configuration.
	"""
	create_vector_index(
		driver,
		index_cfg["name"],
		label=index_cfg["label"],
		embedding_property=index_cfg["property"],
		dimensions=index_cfg["dimensions"],
		similarity_fn=index_cfg["similarity"],
		fail_if_exists=False,
	)

def ensure_community_summary_index(driver: Driver, index_cfg: dict) -> None:
	"""
	Ensure the community summary vector index exists using the provided configuration.
	"""
	create_vector_index(
		driver,
		index_cfg["name"],
		label=index_cfg["label"],
		embedding_property=index_cfg["property"],
		dimensions=index_cfg["dimensions"],
		similarity_fn=index_cfg["similarity"],
		fail_if_exists=False,
	)


async def run_documents(
	driver: Driver,
	documents: list[dict],
	*,
	passes: int = 3,
	config_path: str,
) -> None:
	"""
	Run the YAML-configured GraphRAG KG builder pipeline with the provided documents.
	
	Args:
		driver: Neo4j driver instance
		documents: List of document dictionaries with metadata (each must contain a 'text' field)
		passes: Number of iterations to run the pipeline over the documents
		config_path: Path to YAML config file for the SimpleKGPipeline template
	"""
	if not documents or not isinstance(documents, list):
		raise ValueError("documents must be a non-empty list")
	
	if not all(isinstance(doc, dict) and "text" in doc for doc in documents):
		raise ValueError("all documents must be dictionaries with 'text' field")

	if passes <= 0:
		raise ValueError("passes must be a positive integer")

	config_data = _load_yaml_config(config_path)
	index_cfg = _resolve_vector_index_config(config_data)
	community_idx_cfg = _resolve_community_index_config(config_data)

	ensure_vector_index(driver, index_cfg)
	ensure_community_summary_index(driver, community_idx_cfg)
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
