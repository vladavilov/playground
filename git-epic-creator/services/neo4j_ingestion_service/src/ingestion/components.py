"""
Custom pipeline components for community analysis (detection, retrieval, summarization).

These components are designed to be referenced from a YAML pipeline with
class_ pointing to ingestion.components.<ComponentName>.
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import Any, Dict, List
from collections import Counter
import os
from concurrent.futures import ThreadPoolExecutor, as_completed

from .repository import Neo4jRepository
from neo4j import Driver
from .summarizer import OpenAITextSummarizer
from neo4j_graphrag.experimental.pipeline import Component, DataModel

logger = logging.getLogger(__name__)

class DictDataModel(DataModel):
	data: Dict[str, Any]


class ScriptsComponentBase:
	"""Base for components needing Cypher script loading and a repository."""

	def __init__(self, *, driver: Driver) -> None:
		self._repo = Neo4jRepository(driver)
		self._cypher_dir = Path(__file__).parent / "cypher_scripts"

	def _load_script(self, name: str) -> str:
		return (self._cypher_dir / name).read_text(encoding="utf-8").strip()


class KGIngestionComponent(ScriptsComponentBase, Component):
	"""Execute SimpleKGPipeline using the bundled `kg_builder_config.yaml`."""

	def __init__(self, *, driver: Driver) -> None:
		super().__init__(driver=driver)

	async def run(self, documents: List[Dict[str, Any]], kg_config_path: str) -> DictDataModel:  # type: ignore[override]
		from neo4j_graphrag.experimental.pipeline.config.runner import PipelineRunner
		# Validate config path
		cfg_path = Path(kg_config_path) if isinstance(kg_config_path, (str, Path)) else None
		if not cfg_path or not str(cfg_path):
			raise ValueError("kg_config_path must be provided")
		# Validate documents
		if not isinstance(documents, list) or len(documents) == 0:
			raise ValueError("documents must be a non-empty list")
		seen: set[str] = set()
		for idx, item in enumerate(documents):
			if not isinstance(item, dict):
				raise ValueError(f"documents[{idx}] must be a dict")
			item_id = item.get("id")
			item_text = item.get("text")
			if not isinstance(item_id, str) or not item_id.strip():
				raise ValueError(f"documents[{idx}].id must be a non-empty string")
			if not isinstance(item_text, str):
				raise ValueError(f"documents[{idx}].text must be a string")
			if item_id in seen:
				raise ValueError("duplicate document id: " + item_id)
			seen.add(item_id)
		runner = PipelineRunner.from_config_file(str(cfg_path))
		aggregate_out: Dict[str, Any] = {"ingestion": {"success": True, "items": len(documents), "results": []}}
		# Propagate project scope if all documents share the same project_id
		try:
			project_ids = {str(item.get("project_id")) for item in documents if "project_id" in item}
			if len(project_ids) == 1:
				aggregate_out["project_id"] = next(iter(project_ids))
		except Exception:
			pass
		for item in documents:
			try:
				text_value = str(item.get("text", ""))
				_ = await runner.run({"text": text_value, "kg_config_path": str(cfg_path)})
				aggregate_out["ingestion"]["results"].append({"id": item.get("id"), "status": "success"})
			except Exception as e:
				aggregate_out["ingestion"]["results"].append({"id": item.get("id"), "status": "failed", "error": str(e)})
		# Ensure subsequent stages read after these writes
		self._repo.barrier()
		return DictDataModel(data=aggregate_out)


class CommunityDetectionComponent(ScriptsComponentBase, Component):
	"""Run the end-to-end community detection pipeline (project → detect → populate)."""

	def __init__(self, *, driver: Driver) -> None:
		super().__init__(driver=driver)

	def _delete_community_nodes(self) -> None:
		self._repo.run_write(self._load_script("delete_all_community_nodes.cypher"))

	async def run(self, data: Dict[str, Any]) -> DictDataModel:  # type: ignore[override]

		logger.info("community_detection_start")
		self._delete_community_nodes()

		# Pre-flight size guard
		try:
			# Use leader routing for read-your-writes guarantees
			nodes_rec = self._repo.run_write("MATCH (n:__Entity__) RETURN count(n) AS c")
			rels_rec = self._repo.run_write("MATCH ()-[r]-() RETURN count(r) AS c")
			nodes_count = int(nodes_rec[0]["c"]) if nodes_rec else 0
			rels_count = int(rels_rec[0]["c"]) if rels_rec else 0
		except Exception:
			nodes_count, rels_count = 0, 0

		max_nodes = int(os.getenv("GDS_MAX_NODES", "200000") or 200000)
		max_rels = int(os.getenv("GDS_MAX_RELS", "2000000") or 2000000)
		if nodes_count > max_nodes or rels_count > max_rels:
			raise RuntimeError(f"Graph too large for projection: nodes={nodes_count} rels={rels_count} (limits: {max_nodes}, {max_rels})")

		# Configurable projection filters
		project_id = (data or {}).get("project_id") if isinstance(data, dict) else None
		min_weight = float(os.getenv("GDS_MIN_WEIGHT", "1.0") or 1.0)
		rel_types_env = os.getenv("GDS_REL_TYPES", "") or ""
		rel_types = [s.strip() for s in rel_types_env.split(",") if s.strip()] or None

		# Single, straightforward sequence: project → detect → populate
		project = self._repo.run(
			self._load_script("project_graph.cypher"),
			{"projectId": project_id, "relationshipTypes": rel_types, "minWeight": min_weight},
		)
		detect = self._repo.run_write(
			self._load_script("detect_communities_with_property.cypher"),
			{
				"graph_name": "communities",
				"writeProperty": "communities",
				"concurrency": int(os.getenv("GDS_CONCURRENCY", "2") or 2),
			},
		)
		ran_levels = int(detect[0].get("ranLevels", 0) if detect else 0)
		rel_count = int(detect[0].get("relationshipCount", 0) if detect else 0)
		create = []
		if rel_count > 0:
			create = self._repo.run_write(
			self._load_script("create_community_nodes_with_property.cypher"),
			{"community_property": "communities", "id_prefix": ""},
		)

		communities_created = self._count_communities()
		logger.info("community_detection_done", extra={"ran_levels": ran_levels})
		out = dict(data or {})
		out["community_detection"] = {
			"success": True,
			"communities_created": communities_created,
			"ran_levels": ran_levels,
			"projection_result": project,
			"detection_result": detect,
			"creation_result": create,
			"relationship_count": rel_count,
		}
		return DictDataModel(data=out)

	def _count_communities(self) -> int:
		res = self._repo.run(self._load_script("count_communities.cypher"))
		return int(res[0]["count"]) if res else 0


class CommunityRetrievalComponent(ScriptsComponentBase, Component):
	"""Retrieve communities for summarization up to the max_levels from detection results or param."""

	def __init__(self, *, driver: Driver, max_levels: int | None = None, size_cap: int | None = None) -> None:
		super().__init__(driver=driver)
		self._max_levels = max_levels
		self._size_cap = size_cap or int(os.getenv("RETRIEVE_PAGE_SIZE", "25") or 25)
		self._node_cap = int(os.getenv("RETRIEVE_NODE_CAP", "100") or 100)
		self._rel_cap = int(os.getenv("RETRIEVE_REL_CAP", "300") or 300)

	async def run(self, data: Dict[str, Any]) -> DictDataModel:  # type: ignore[override]
		max_levels = self._max_levels
		if max_levels is None and data and isinstance(data.get("community_detection"), dict):
			ran_levels = int(data["community_detection"].get("ran_levels", 0))
			max_levels = max(0, ran_levels - 1)

		size_cap = self._size_cap
		if data and isinstance(data.get("size_cap"), (int, float)):
			size_cap = int(data.get("size_cap"))
		query = self._load_script("retrieve_communities.cypher")
		# Pagination to bound memory
		offset = 0
		page_size = size_cap
		communities: List[Dict[str, Any]] = []
		while True:
			page = self._repo.run(
				query,
				{
					"maxLevels": max_levels or 0,
					"offset": offset,
					"pageSize": page_size,
					"nodeCap": self._node_cap,
					"relCap": self._rel_cap,
				},
			)
			if not page:
				break
			communities.extend(page)
			if len(page) < page_size:
				break
			offset += page_size
		# Compute simple size distribution (top-5 sizes by size value)
		sizes: List[int] = []
		for c in communities:
			nodes = c.get("nodes") if isinstance(c, dict) else None
			if isinstance(nodes, list):
				sizes.append(len(nodes))
		size_hist = Counter(sizes)
		top_sizes = sorted(size_hist.items(), key=lambda kv: (-kv[0]))[:5]
		logger.info(
			"community_retrieval_stats",
			extra={
				"communities_retrieved": len(communities),
				"size_top": top_sizes,
				"ran_levels": max_levels or 0,
				"size_cap": size_cap,
			},
		)
		out = dict(data or {})
		out["communities"] = communities
		out["ran_levels"] = max_levels or 0
		out["size_cap"] = size_cap
		out["community_retrieval"] = {
			"communities_retrieved": len(communities),
			"size_top": top_sizes,
			"ran_levels": max_levels or 0,
			"size_cap": size_cap,
		}
		return DictDataModel(data=out)


class CommunitySummarizationComponent(ScriptsComponentBase, Component):
	"""Summarize communities and update graph using OpenAITextSummarizer."""

	def __init__(self, *, driver: Driver, summarizer: OpenAITextSummarizer | None = None) -> None:
		super().__init__(driver=driver)
		self._summarizer = summarizer or OpenAITextSummarizer()

	def _format_community(self, community: Dict[str, Any]) -> str:
		nodes = sorted(community.get("nodes", []), key=lambda n: n.get("id", ""))
		rels = sorted(community.get("rels", []), key=lambda r: (r.get("start", ""), r.get("end", ""), r.get("type", "")))

		lines = ["Nodes are:"]
		for node in nodes:
			node_id = node.get("id", "Unknown")
			node_type = node.get("type", "Unknown")
			desc = (node.get("description") or "").strip()
			if desc:
				lines.append(f"id: {node_id}, type: {node_type}, description: {desc}")
			else:
				lines.append(f"id: {node_id}, type: {node_type}")

		lines.append("")
		lines.append("Relationships are:")
		for rel in rels:
			start = rel.get("start", "Unknown")
			end = rel.get("end", "Unknown")
			rel_type = rel.get("type", "Unknown")
			desc = (rel.get("description") or "").strip()
			line = f"({start})-[:{rel_type}]->({end})"
			if desc:
				line += f", description: {desc}"
			lines.append(line)

		return "\n".join(lines)

	def _prompt(self, community_data: str) -> str:
		return (
			"Analyze the nodes and relationships and provide a concise, informative summary capturing:\n"
			"1) Main theme/purpose\n2) Key entities and their roles\n3) Important relationships/patterns (edge types, notable links)\n4) Overall significance/context\n\n"
			"Constraints:\n- Preserve exact identifiers and types.\n- Quantify sizes/counts when available (nodes, edges).\n- <=200 words, crisp, neutral, professional.\n\n"
			"Output format:\n1) Executive summary (2–3 sentences)\n2) Key details (bulleted)\n3) Action items (if any)\n4) Open questions (only if blocking)\n\n"
			f"Community data to analyze:\n{community_data}\n"
		)

	def _update_summary(self, cid: str, summary: str) -> None:
		query = self._load_script("update_community_summary.cypher")
		self._repo.run(query, {"community_id": cid, "summary": summary})

	def _summarize_and_update(self, cid: str, prompt: str, fail_log_label: str) -> Dict[str, Any]:
		"""Summarize prompt and update community; return standardized result."""

		try:
			summary = self._summarizer.summarize(prompt, max_tokens=200, temperature=0.0)
			self._update_summary(cid, summary)
			return {"community_id": cid, "status": "success", "summary": summary}
		except Exception as e:
			logger.warning(f"{fail_log_label}: {e}")
			return {"community_id": cid, "status": "failed", "summary": None, "error": str(e)}

	async def run(self, data: Dict[str, Any]) -> DictDataModel:  # type: ignore[override]
		logger.info("community_summarization_start")
		communities: List[Dict[str, Any]] = list((data or {}).get("communities", []))
		results: List[Dict[str, Any]] = []
		success, failed = 0, 0

		# Process level 0 communities sequentially
		for community in communities:
			cid = community.get("communityId")
			formatted = self._format_community(community)
			prompt = self._prompt(formatted)
			entry = self._summarize_and_update(cid, prompt, "summarize_failed")
			results.append(entry)
			if entry.get("status") == "success":
				success += 1
			else:
				failed += 1

		# Process higher levels sequentially
		rec = self._repo.run(self._load_script("max_community_level.cypher"))
		max_level = int(rec[0]["max_level"]) if rec else 0
		for level in range(1, max_level + 1):
			level_ids = [r["id"] for r in self._repo.run(self._load_script("level_ids.cypher"), {"level": level})]
			for pid in level_ids:
				rec = self._repo.run(self._load_script("children_with_summaries.cypher"), {"pid": pid})
				children = rec[0]["communities"] if rec else []

				parent_input = "Context:\nParent Community ID: " + pid + "\n\nChild communities:\n" + "\n".join(
					[f"- {ch.get('id', 'Unknown')} (children: {ch.get('numberOfChildren', 0)}) summary: {ch.get('summary') or 'No summary yet'}" for ch in children]
				)
				prompt = self._prompt(parent_input)
				entry = self._summarize_and_update(pid, prompt, "summarize_higher_failed")
				results.append(entry)
				if entry.get("status") == "success":
					success += 1
				else:
					failed += 1

		logger.info(
			"community_summarization_stats",
			extra={
				"total_communities": len(communities),
				"successful_summaries": success,
				"failed_summaries": failed,
			},
		)
		out = dict(data or {})
		out["community_summarization"] = {
			"success": True,
			"total_communities": len(communities),
			"successful_summaries": success,
			"failed_summaries": failed,
			"results": results,
		}
		return DictDataModel(data=out)


class EntityRelationshipDescriptionBackfillComponent(ScriptsComponentBase, Component):
	"""Backfill missing `description` on entities and key relationships using LLM.

	- Pulls a limited batch of missing items (configurable via BACKFILL_BATCH, default 100)
	- Builds compact prompts using neighbor context
	- Uses OpenAITextSummarizer to generate 1–2 sentence descriptions
	- Skips already described items; idempotent by design
	"""

	def __init__(self, *, driver: Driver, summarizer: OpenAITextSummarizer | None = None) -> None:
		super().__init__(driver=driver)
		self._summarizer = summarizer or OpenAITextSummarizer()

	def _prompt_entity(self, entity_type: str, neighbors: List[dict]) -> str:
		lines = [
			"Provide exactly one sentence describing an entity in a knowledge graph.",
			f"Entity type: {entity_type}",
			"Neighbor context (type and relation):",
		]
		for nb in neighbors[:10]:
			rel = nb.get("rel", "")
			nb_type = nb.get("neighbor_type", "")
			nb_desc = (nb.get("neighbor_desc") or "").strip()
			if nb_desc:
				lines.append(f"- {rel} {nb_type}: {nb_desc}")
			else:
				lines.append(f"- {rel} {nb_type}")
		lines.append("Write a neutral, factual description. Avoid repeating neighbor text. Output strictly one sentence.")
		return "\n".join(lines)

	def _prompt_relationship(self, start_type: str, rel_type: str, end_type: str) -> str:
		return (
			"Provide exactly one sentence describing a relationship in a knowledge graph.\n"
			f"Relationship: ({start_type})-[:{rel_type}]->({end_type})\n"
			"Describe its typical meaning in this context, neutral and factual. Output strictly one sentence."
		)

	def _list_entities_missing(self, limit: int) -> List[Dict[str, Any]]:
		query = self._load_script("list_entities_missing_descriptions.cypher")
		return self._repo.run(query, {"limit": limit})

	def _entity_context(self, entity_id: str) -> Dict[str, Any]:
		query = self._load_script("get_entity_neighbor_context.cypher")
		res = self._repo.run(query, {"id": entity_id})
		return res[0] if res else {"id": entity_id, "type": "Unknown", "neighbors": []}

	def _update_entity(self, entity_id: str, description: str) -> None:
		query = self._load_script("update_entity_description.cypher")
		self._repo.run(query, {"id": entity_id, "description": description})

	def _list_relationships_missing(self, limit: int) -> List[Dict[str, Any]]:
		query = self._load_script("list_relationships_missing_descriptions.cypher")
		return self._repo.run(query, {"limit": limit})

	def _update_relationship(self, start_id: str, rel_type: str, end_id: str, description: str) -> None:
		query = self._load_script("update_relationship_description.cypher")
		self._repo.run(query, {"start": start_id, "type": rel_type, "end": end_id, "description": description})

	async def run(self, data: Dict[str, Any]) -> DictDataModel:  # type: ignore[override]
		logger.info("backfill_descriptions_start")
		batch = max(1, min(int(os.getenv("BACKFILL_BATCH", "50") or 50), 100))
		max_workers = max(1, min(int(os.getenv("BACKFILL_MAX_WORKERS", "1") or 1), 2))

		# Entities
		entities_missing = self._list_entities_missing(batch)
		entity_results: List[Dict[str, Any]] = []

		def _process_entity(item: Dict[str, Any]) -> Dict[str, Any]:
			try:
				ctx = self._entity_context(str(item["id"]))
				prompt = self._prompt_entity(str(ctx.get("type", "Unknown")), list(ctx.get("neighbors", [])))
				desc = self._summarizer.summarize(prompt, temperature=0.0)
				self._update_entity(str(item["id"]), desc)
				return {"entity_id": str(item["id"]), "status": "updated"}
			except Exception as e:
				return {"entity_id": str(item.get("id", "")), "status": "failed", "error": str(e)}

		# Process entities in small sequential batches to reduce memory and pool pressure
		for i in range(0, len(entities_missing), max(1, batch // 5)):
			chunk = entities_missing[i : i + max(1, batch // 5)]
			with ThreadPoolExecutor(max_workers=max_workers) as executor:
				for fut in as_completed([executor.submit(_process_entity, it) for it in chunk]):
					entity_results.append(fut.result())

		# Relationships
		rel_missing = self._list_relationships_missing(batch)
		rel_results: List[Dict[str, Any]] = []

		def _process_rel(item: Dict[str, Any]) -> Dict[str, Any]:
			try:
				start_type = str(item.get("start_type", ""))
				rel_type = str(item.get("type", ""))
				end_type = str(item.get("end_type", ""))
				prompt = self._prompt_relationship(start_type, rel_type, end_type)
				desc = self._summarizer.summarize(prompt, temperature=0.0)
				self._update_relationship(str(item["start"]), rel_type, str(item["end"]), desc)
				return {"start": str(item["start"]), "type": rel_type, "end": str(item["end"]), "status": "updated"}
			except Exception as e:
				return {"start": str(item.get("start", "")), "type": item.get("type", ""), "end": str(item.get("end", "")), "status": "failed", "error": str(e)}

		for i in range(0, len(rel_missing), max(1, batch // 5)):
			chunk = rel_missing[i : i + max(1, batch // 5)]
			with ThreadPoolExecutor(max_workers=max_workers) as executor:
				for fut in as_completed([executor.submit(_process_rel, it) for it in chunk]):
					rel_results.append(fut.result())

		out = dict(data or {})
		out["backfill_descriptions"] = {
			"entities": entity_results,
			"relationships": rel_results,
			"success": True,
		}
		logger.info("backfill_descriptions_done", extra={"entities_processed": len(entity_results), "relationships_processed": len(rel_results)})
		# Ensure downstream stages see the updates
		self._repo.barrier()
		return DictDataModel(data=out)

