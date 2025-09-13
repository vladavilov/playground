import json
from pathlib import Path

from utils.neo4j_client import get_neo4j_client
from neo4j_graphrag.experimental.pipeline import Pipeline
from .components import (
	KGIngestionComponent,
	EntityRelationshipDescriptionBackfillComponent,
	CommunityDetectionComponent,
	CommunityRetrievalComponent,
	CommunitySummarizationComponent,
)

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
			# Assign a stable id derived from filename (without .json); guarantees id/text presence
			document["id"] = Path(file_path).stem
			if not isinstance(document.get("text"), str):
				document["text"] = ""
			
			return document
	except (json.JSONDecodeError, KeyError):
		pass
	
	# Fallback to basic document info
	return {
		"id": Path(file_path).stem,
		"text": "",
		"path": file_path,
		"title": Path(file_path).name,
		"project_id": project_id,
		"file_name": Path(file_path).name,
		"file_type": "unknown",
		"content_type": "application/octet-stream"
	}

async def run_end_to_end(documents: list[dict]) -> dict[str, object]:
	"""
	Run the single end-to-end pipeline (ingestion + community analysis).
	"""
	# Basic shape check; detailed validation happens inside KGIngestionComponent
	if not isinstance(documents, list) or len(documents) == 0:
		raise ValueError("documents must be a non-empty list")

	config_dir = Path(__file__).resolve().parent / "config"
	kg_builder_yaml = str(config_dir / "kg_builder_config.yaml")

	client = get_neo4j_client()
	driver = client.driver

	pipe = Pipeline()
	pipe.add_component(KGIngestionComponent(driver=driver), "ingest")
	pipe.add_component(EntityRelationshipDescriptionBackfillComponent(driver=driver), "backfill")
	pipe.add_component(CommunityDetectionComponent(driver=driver), "detect")
	pipe.add_component(CommunityRetrievalComponent(driver=driver), "retrieve")
	pipe.add_component(CommunitySummarizationComponent(driver=driver), "summarize")

	# Wire: subsequent steps consume the accumulated dict from previous
	pipe.connect("ingest", "backfill", input_config={"data": "ingest.data"})
	pipe.connect("backfill", "detect", input_config={"data": "backfill.data"})
	pipe.connect("detect", "retrieve", input_config={"data": "detect.data"})
	pipe.connect("retrieve", "summarize", input_config={"data": "retrieve.data"})

	# Run with initial inputs for ingestion stage
	result = await pipe.run({
		"ingest": {
			"documents": documents,
			"kg_config_path": kg_builder_yaml,
		}
	})

	# Final accumulated dict is under summarize.data
	final_payload = result.get("summarize", {}).get("data", {}) if isinstance(result, dict) else {}
	return final_payload


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
