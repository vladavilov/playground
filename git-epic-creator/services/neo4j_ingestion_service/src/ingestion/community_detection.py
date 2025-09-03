"""
Community Detection Service

Executes Cypher scripts to detect communities in the Neo4j graph using GDS algorithms.
"""

import logging
from pathlib import Path
from typing import Any, Dict, List
from neo4j import Driver

logger = logging.getLogger(__name__)


class CommunityDetectionService:
	"""Service for detecting communities in the Neo4j graph."""

	def __init__(self, driver: Driver):
		self.driver = driver
		self.cypher_scripts_dir = Path(__file__).parent / "cypher_scripts"

	def _drop_existing_projection(self, graph_name: str = "communities") -> None:
		"""Drop existing GDS in-memory graph projection if it exists."""
		logger.info(f"Checking for existing GDS graph projection '{graph_name}' to drop")
		query = (
			"CALL gds.graph.exists($graph_name) YIELD exists "
			"CALL apoc.do.when("
			"  exists,"
			"  'CALL gds.graph.drop($graph_name) YIELD graphName RETURN graphName',"
			"  'RETURN null AS graphName',"
			"  {graph_name: $graph_name}"
			") YIELD value "
			"RETURN value.graphName AS droppedGraph"
		)
		try:
			result = self._execute_cypher(query, {"graph_name": graph_name})
			dropped = result[0].get("droppedGraph") if result else None
			if dropped:
				logger.info(f"Dropped existing GDS graph projection '{dropped}'")
			else:
				logger.info("No existing GDS graph projection to drop")
		except Exception as e:
			logger.error(f"Failed while attempting to drop existing GDS graph '{graph_name}': {e}")
			raise

	def _delete_all_community_nodes(self) -> None:
		"""Delete all __Community__ nodes and their relationships."""
		logger.info("Deleting all existing __Community__ nodes before recreation")
		query = "MATCH (c:`__Community__`) DETACH DELETE c"
		try:
			self._execute_cypher(query)
			logger.info("Deleted existing __Community__ nodes (if any)")
		except Exception as e:
			logger.error(f"Failed to delete __Community__ nodes: {e}")
			raise

	def _load_cypher_script(self, script_name: str) -> str:
		"""Load a Cypher script from the cypher_scripts directory."""
		script_path = self.cypher_scripts_dir / script_name
		try:
			with open(script_path, 'r', encoding='utf-8') as f:
				return f.read().strip()
		except FileNotFoundError:
			raise FileNotFoundError(f"Cypher script not found: {script_path}")

	def _execute_cypher(self, query: str, parameters: Dict[str, Any] | None = None) -> List[Dict[str, Any]]:
		"""Execute a Cypher query and return results."""
		try:
			with self.driver.session() as session:
				result = session.run(query, parameters or {})
				return [dict(record) for record in result]
		except Exception as e:
			logger.error(f"Failed to execute Cypher query: {e}")
			logger.error(f"Query: {query}")
			raise

	async def run_community_detection(self) -> Dict[str, Any]:
		"""
		Run the complete community detection pipeline.

		Returns:
			Dict containing results and metrics from community detection
		"""
		logger.info("Starting community detection pipeline")

		try:
			# Pre-step A: Drop existing GDS projection if present
			self._drop_existing_projection("communities")

			# Pre-step B: Delete existing community nodes to avoid stale data
			self._delete_all_community_nodes()

			# Step 1: Project the graph
			logger.info("Step 1: Projecting graph for community detection")
			project_script = self._load_cypher_script("project_graph.cypher")
			project_result = self._execute_cypher(project_script)
			logger.info("Graph projection completed")

			# Step 2: Detect communities using Leiden algorithm
			logger.info("Step 2: Running Leiden community detection")
			detect_script = self._load_cypher_script("detect_communities.cypher")
			detect_result = self._execute_cypher(detect_script)
			logger.info("Community detection completed")

			# Extract ranLevels from the detection result
			ran_levels = 0
			if detect_result and len(detect_result) > 0:
				ran_levels = detect_result[0].get("ranLevels", 0)
				logger.info(f"Leiden algorithm ran {ran_levels} levels")

			# Step 3: Create community nodes
			logger.info("Step 3: Creating community nodes")
			create_script = self._load_cypher_script("create_community_nodes.cypher")
			create_result = self._execute_cypher(create_script)
			logger.info("Community nodes created")

			# Get community count
			community_count = self._get_community_count()

			logger.info(f"Community detection pipeline completed successfully. Found {community_count} communities across {ran_levels} levels.")

			return {
				"success": True,
				"communities_created": community_count,
				"ran_levels": ran_levels,
				"projection_result": project_result,
				"detection_result": detect_result,
				"creation_result": create_result,
			}

		except Exception as e:
			logger.error(f"Community detection pipeline failed: {e}")
			return {
				"success": False,
				"error": str(e),
			}

	def _get_community_count(self) -> int:
		"""Get the total count of communities created."""
		query = "MATCH (c:__Community__) RETURN count(c) as count"
		result = self._execute_cypher(query)
		return result[0]["count"] if result else 0

	def get_communities_for_summarization(self, max_levels: int = 0) -> List[Dict[str, Any]]:
		"""
		Retrieve communities and their contents for summarization.

		Args:
			max_levels: Maximum number of community levels to retrieve (0-based)

		Returns:
			List of communities with their nodes and relationships
		"""
		logger.info(f"Retrieving communities for summarization up to level {max_levels}")

		try:
			retrieve_script = self._load_cypher_script("retrieve_communities.cypher")
			communities = self._execute_cypher(retrieve_script, {"maxLevels": max_levels})

			logger.info(f"Retrieved {len(communities)} communities for summarization")
			return communities

		except Exception as e:
			logger.error(f"Failed to retrieve communities for summarization: {e}")
			raise
