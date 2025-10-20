from typing import Any, Dict, List


class Neo4jRepository:
    def __init__(self, session: Any) -> None:
        self._session = session

    def list_index_names(self) -> List[str]:
        return [r["name"] for r in self._session.run("SHOW INDEXES YIELD name RETURN name")]

    def vector_query_nodes(self, index_name: str, k: int, qvec: List[float], project_id: str) -> List[Any]:
        query = (
            "MATCH (p:__Project__ {id: $projectId}) "
            "CALL db.index.vector.queryNodes($name, $k, $qvec) YIELD node AS n, score "
            "WHERE (n)-[:IN_PROJECT]->(p) "
            "RETURN n AS node, score"
        )
        return list(self._session.run(query, name=index_name, k=k, qvec=qvec, projectId=project_id))

    def sample_chunks_for_communities(self, community_ids: List[int], chunk_index: str, qvec: List[float], project_id: str) -> Dict[int, List[str]]:
        query = (
            "MATCH (p:__Project__ {id: $projectId}) "
            "MATCH (c:__Community__)-[:IN_PROJECT]->(p) WHERE c.community IN $communityIds "
            # Neo4j 5.28+ explicit variable scope syntax: CALL () { WITH ... }
            "CALL (c, p) { WITH c, p MATCH (c)<-[:IN_COMMUNITY]-(ch:__Chunk__)-[:IN_PROJECT]->(p) "
            "WITH ch CALL db.index.vector.queryNodes($chunkIndex, 50, $qvec) YIELD node AS cand, score "
            "WHERE cand = ch RETURN cand AS chunk, score ORDER BY score DESC LIMIT 3 } "
            "RETURN c.community AS cid, collect(distinct chunk.id) AS chunk_ids"
        )
        res = list(self._session.run(query, communityIds=community_ids, qvec=qvec, chunkIndex=chunk_index, projectId=project_id))
        out: Dict[int, List[str]] = {}
        for row in res:
            cid = int(row.get("cid")) if row.get("cid") is not None else None
            if cid is None:
                continue
            out[cid] = [str(x) for x in (row.get("chunk_ids") or []) if x is not None]
        return out

    def fetch_community_summaries(self, ids: List[int], project_id: str) -> Dict[int, str]:
        rows = list(self._session.run(
            "MATCH (c:__Community__)-[:IN_PROJECT]->(:__Project__ {id: $projectId}) WHERE c.community IN $ids RETURN c.community AS id, c.summary AS summary",
            ids=ids,
            projectId=project_id,
        ))
        out: Dict[int, str] = {}
        for r in rows:
            try:
                cid = int(r.get("id"))
                out[cid] = r.get("summary") or ""
            except Exception:
                continue
        return out

    def fetch_communities_brief(self, ids: List[int], project_id: str) -> List[Dict[str, Any]]:
        rows = list(self._session.run(
            "MATCH (c:__Community__)-[:IN_PROJECT]->(:__Project__ {id: $projectId}) WHERE c.community IN $ids RETURN c.community AS id, c.summary AS summary",
            ids=ids,
            projectId=project_id,
        ))
        result: List[Dict[str, Any]] = []
        for r in rows:
            result.append({
                "id": int(r.get("id")) if r.get("id") is not None else None,
                "summary": r.get("summary") or "",
            })
        return result

    def scoped_chunk_ids(self, cids: List[int], chunk_index: str, qvec: List[float], limit: int, project_id: str) -> List[str]:
        scoped_q = (
            "MATCH (p:__Project__ {id: $projectId}) "
            "MATCH (c:__Community__)-[:IN_PROJECT]->(p) WHERE c.community IN $cids "
            "MATCH (c)<-[:IN_COMMUNITY]-(ch:__Chunk__)-[:IN_PROJECT]->(p) "
            "WITH DISTINCT ch, p "
            "CALL db.index.vector.queryNodes($chunkIndex, 200, $qvec) YIELD node AS cand, score "
            "WHERE cand = ch "
            "RETURN DISTINCT ch.id AS cid, score ORDER BY score DESC LIMIT $limit"
        )
        rows = list(self._session.run(scoped_q, qvec=qvec, cids=cids, chunkIndex=chunk_index, limit=int(limit or 1), projectId=project_id))
        return [str(r.get("cid")) for r in rows if r.get("cid") is not None]

    def expand_neighborhood_minimal(self, chunk_ids: List[str], project_id: str) -> List[Dict[str, Any]]:
        query = (
            "UNWIND $chunkIds AS cid "
            "MATCH (p:__Project__ {id: $projectId}) "
            "MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p) WHERE ch.id = cid "
            # Get entities from current chunk
            "OPTIONAL MATCH (ch)-[:HAS_ENTITY]->(e:__Entity__)-[:IN_PROJECT]->(p) "
            # Get related entities and their relationships
            "OPTIONAL MATCH (e)-[r:RELATED]->(re:__Entity__)-[:IN_PROJECT]->(p) "
            # Get chunks containing related entities
            "OPTIONAL MATCH (ch2:__Chunk__)-[:HAS_ENTITY]->(re) "
            "WHERE ch2 <> ch AND (ch2)-[:IN_PROJECT]->(p) "
            "WITH cid, ch, "
            "collect(DISTINCT { _id: e.id, properties: { "
            "title: e.title, description: e.description, type: coalesce(e.type, ''), "
            "text_unit_ids: e.text_unit_ids } })[0..3] AS entities, "
            "collect(DISTINCT { _id: re.id, properties: { "
            "title: re.title, description: re.description, type: coalesce(re.type, ''), "
            "text_unit_ids: re.text_unit_ids } })[0..5] AS related_entities, "
            "collect(DISTINCT { type: type(r), description: r.description })[0..10] AS relationships, "
            "collect(DISTINCT ch2.id)[0..5] AS neighbor_chunk_ids "
            "RETURN cid AS chunk_id, ch.text AS text, "
            "entities AS neighbours, related_entities, relationships, neighbor_chunk_ids"
        )
        rows = list(self._session.run(query, chunkIds=chunk_ids[:3], projectId=project_id))
        result: List[Dict[str, Any]] = []
        for r in rows:
            result.append({
                "chunk_id": str(r.get("chunk_id")) if r.get("chunk_id") is not None else None,
                "text": r.get("text") or "",
                "neighbours": r.get("neighbours") or [],
                "related_entities": r.get("related_entities") or [],
                "relationships": r.get("relationships") or [],
                "neighbor_chunk_ids": [str(x) for x in (r.get("neighbor_chunk_ids") or []) if x is not None],
            })
        return result


