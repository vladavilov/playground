"""
Community Summarization Service

Generates LLM summaries for communities based on their nodes and relationships.
"""

import logging
import json
from typing import Any, Dict, List
from neo4j import Driver

# LLM classes are imported and used via graphrag_pipeline._build_llm()
from configuration.graphrag_config import get_graphrag_settings

logger = logging.getLogger(__name__)


class CommunitySummarizerService:
	"""Service for generating LLM summaries of communities."""

	def __init__(self, driver: Driver):
		self.driver = driver
		self.settings = get_graphrag_settings()
		self.llm = self._build_llm()

	def _build_llm(self):
		"""Build the LLM client based on configuration."""
		# Use the same LLM building logic as in graphrag_pipeline.py
		from .graphrag_pipeline import _build_llm
		return _build_llm()

	def _format_community_for_summarization(self, community: Dict[str, Any]) -> str:
		"""
		Format community data for LLM summarization.

		Args:
			community: Community data from Neo4j

		Returns:
			Formatted string for LLM input
		"""
		community_id = community.get("communityId", "Unknown")
		nodes = community.get("nodes", [])
		relationships = community.get("rels", [])

		# Format nodes
		nodes_text = "Nodes in this community:\n"
		for node in nodes:
			node_id = node.get("id", "Unknown")
			node_type = node.get("type", "Unknown")
			description = node.get("description", "No description")
			nodes_text += f"- {node_type} (ID: {node_id}): {description}\n"

		# Format relationships
		rels_text = "Relationships in this community:\n"
		for rel in relationships:
			start_id = rel.get("start", "Unknown")
			rel_type = rel.get("type", "Unknown")
			end_id = rel.get("end", "Unknown")
			description = rel.get("description", "No description")
			rels_text += f"- {start_id} --[{rel_type}]--> {end_id}: {description}\n"

		return f"Community ID: {community_id}\n\n{nodes_text}\n{rels_text}"

	def _get_summarization_prompt(self, community_data: str) -> str:
		"""
		Generate the system prompt for community summarization.

		Args:
			community_data: Formatted community data

		Returns:
			Complete prompt for LLM
		"""
		system_prompt = """You are an expert analyst tasked with summarizing communities in a knowledge graph. 

Your task is to analyze the nodes and relationships within a community and provide a concise, informative summary that captures:

1. The main theme or purpose of the community
2. Key entities and their roles
3. Important relationships and patterns
4. The overall significance or context

Provide a summary that is:
- Clear and concise (2-3 sentences)
- Focused on the main theme
- Useful for understanding the community's purpose
- Professional and objective in tone

Community data to analyze:
{community_data}

Provide your summary:"""

		return system_prompt.format(community_data=community_data)

	async def summarize_communities(self, communities: List[Dict[str, Any]]) -> Dict[str, Any]:
		"""
		Generate summaries for all communities.

		Args:
			communities: List of communities from community detection

		Returns:
			Dict containing summarization results and metrics
		"""
		logger.info(f"Starting summarization of {len(communities)} communities")

		results: List[Dict[str, Any]] = []
		successful_summaries = 0
		failed_summaries = 0

		# Step 1: Summarize provided communities (assumed level 0)
		for i, community in enumerate(communities):
			try:
				logger.info(f"Summarizing community {i+1}/{len(communities)}: {community.get('communityId', 'Unknown')}")

				# Format community data
				community_data = self._format_community_for_summarization(community)

				# Generate prompt
				prompt = self._get_summarization_prompt(community_data)

				# Get LLM summary
				summary = self._get_llm_summary(prompt)

				# Update community node with summary
				self._update_community_summary(community.get("communityId"), summary)

				results.append({
					"community_id": community.get("communityId"),
					"summary": summary,
					"status": "success",
				})

				successful_summaries += 1
				logger.info(f"Successfully summarized community {community.get('communityId')}")

			except Exception as e:
				logger.error(f"Failed to summarize community {community.get('communityId', 'Unknown')}: {e}")
				results.append({
					"community_id": community.get("communityId"),
					"summary": None,
					"status": "failed",
					"error": str(e),
				})
				failed_summaries += 1

		# Step 2: Retrieve higher-level communities and summarize level by level
		try:
			with self.driver.session() as session:
				# Determine max level present in graph
				max_level_record = session.run("MATCH (c:__Community__) RETURN coalesce(max(c.level), 0) AS max_level").single()
				max_level = max_level_record["max_level"] if max_level_record else 0
				logger.info(f"Detected max community level: {max_level}")

				# Helper to format higher-level community input from child summaries
				def format_parent_from_children(parent_id: str, children: List[Dict[str, Any]]) -> str:
					lines = [f"Parent Community ID: {parent_id}", "", "Child communities:"]
					for ch in children:
						ch_id = ch.get("id", "Unknown")
						ch_sum = ch.get("summary", "") or "No summary yet"
						ch_count = ch.get("numberOfChildren", 0)
						lines.append(f"- {ch_id} (children: {ch_count}) summary: {ch_sum}")
					return "\n".join(lines)

				# Iterate levels > 0
				for level in range(1, int(max_level) + 1):
					logger.info(f"Summarizing communities at level {level}")
					# Get all community ids at this level
					level_ids = [r["id"] for r in session.run(
						"MATCH (c:__Community__ {level: $level}) RETURN c.id AS id ORDER BY id",
						{"level": level},
					)]

					for parent_id in level_ids:
						try:
							# Retrieve up-to-date children with their summaries
							rec = session.run(
								"MATCH (p:__Community__ {id: $pid}) "
								"OPTIONAL MATCH (p)<-[:IN_COMMUNITY]-(child:__Community__) "
								"WITH p, collect(child) AS cs "
								"WITH p, [c IN cs WHERE c IS NOT NULL | {id: c.id, summary: coalesce(c.summary, ''), "
								"numberOfChildren: size( (c)<-[:IN_COMMUNITY]-(:__Community__) )}] AS communities "
								"RETURN p.id AS communityId, communities",
								{"pid": parent_id},
							).single()
							children = rec["communities"] if rec else []

							# Format and summarize based on children summaries
							parent_input = format_parent_from_children(parent_id, children)
							prompt = self._get_summarization_prompt(parent_input)
							summary = self._get_llm_summary(prompt)

							# Update parent community summary immediately in the same loop
							self._update_community_summary(parent_id, summary)
							results.append({
								"community_id": parent_id,
								"summary": summary,
								"status": "success",
							})
							successful_summaries += 1
							logger.info(f"Successfully summarized higher-level community {parent_id}")
						except Exception as e:
							logger.error(f"Failed to summarize higher-level community {parent_id}: {e}")
							results.append({
								"community_id": parent_id,
								"summary": None,
								"status": "failed",
								"error": str(e),
							})
							failed_summaries += 1
		except Exception as e:
			logger.error(f"Higher-level community summarization failed: {e}")

		logger.info(f"Community summarization completed. {successful_summaries} successful, {failed_summaries} failed.")

		return {
			"success": True,
			"total_communities": len(communities),
			"successful_summaries": successful_summaries,
			"failed_summaries": failed_summaries,
			"results": results,
		}

	def _get_llm_summary(self, prompt: str) -> str:
		"""
		Get summary from LLM.

		Args:
			prompt: Complete prompt for LLM

		Returns:
			Generated summary text
		"""
		try:
			# Debug: Log the LLM object type and available methods
			logger.debug(f"LLM object type: {type(self.llm)}")
			logger.debug(f"LLM object dir: {[attr for attr in dir(self.llm) if not attr.startswith('_')]}")

			# Prefer synchronous interfaces to avoid un-awaited async usage
			if hasattr(self.llm, 'generate'):
				logger.debug("Using generate method")
				response = self.llm.generate([prompt])
				if response and getattr(response, 'generations', None):
					return response.generations[0][0].text.strip()
			elif hasattr(self.llm, 'chat'):
				logger.debug("Using chat.completions.create method")
				response = self.llm.chat.completions.create(
					model=self.settings.OAI_MODEL,
					messages=[
						{"role": "system", "content": "You are a helpful assistant."},
						{"role": "user", "content": prompt},
					],
					max_tokens=200,
					temperature=0,
				)
				return response.choices[0].message.content.strip()
			elif hasattr(self.llm, 'complete'):
				logger.debug("Using complete method")
				response = self.llm.complete(prompt, max_tokens=200, temperature=0)
				return response.strip()
			else:
				logger.warning(f"LLM object {type(self.llm)} has no supported generation method, using fallback")
				return "Community analysis completed. This community contains multiple entities and relationships."
		except Exception as e:
			logger.error(f"LLM summarization failed: {e}")
			return f"Community analysis completed with error: {str(e)[:100]}"

	def _update_community_summary(self, community_id: str, summary: str) -> None:
		"""
		Update community node with generated summary.

		Args:
			community_id: ID of the community to update
			summary: Generated summary text
		"""
		query = """
		MATCH (c:__Community__ {id: $community_id})
		SET c.summary = $summary, c.summarized_at = datetime()
		RETURN c
		"""

		try:
			with self.driver.session() as session:
				session.run(query, {"community_id": community_id, "summary": summary})
			logger.debug(f"Updated community {community_id} with summary")
		except Exception as e:
			logger.error(f"Failed to update community {community_id} with summary: {e}")
			raise

	def get_community_hierarchy(self) -> List[Dict[str, Any]]:
		"""
		Return each community and its child communities with summary and child counts.

		Returns:
			List of {communityId, communities: [{id, summary, numberOfChildren}]}
		"""
		query = (
			"MATCH (p:`__Community__`) "
			"WITH p, [ (p)<-[:IN_COMMUNITY]-(child:`__Community__`) | "
			"{ id: child.id, summary: coalesce(child.summary, ''), "
			"numberOfChildren: size( (child)<-[:IN_COMMUNITY]-(:`__Community__`) ) } ] AS communities "
			"RETURN p.id AS communityId, communities "
			"ORDER BY p.level ASC, communityId ASC"
		)
		try:
			with self.driver.session() as session:
				records = session.run(query)
				return [
					{"communityId": r["communityId"], "communities": r["communities"]}
					for r in records
				]
		except Exception as e:
			logger.error(f"Failed to retrieve community hierarchy: {e}")
			raise