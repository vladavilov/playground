from typing import Any, Dict, List


class Neo4jRepository:
    def __init__(self, session: Any) -> None:
        self._session = session

    def list_index_names(self) -> List[str]:
        return [r["name"] for r in self._session.run("SHOW INDEXES YIELD name RETURN name")]

    def vector_query_nodes(self, index_name: str, k: int, qvec: List[float]) -> List[Any]:
        return list(self._session.run(
            "CALL db.index.vector.queryNodes($name, $k, $qvec)",
            name=index_name,
            k=k,
            qvec=qvec,
        ))

    def sample_chunks_for_communities(self, community_ids: List[int], chunk_index: str, qvec: List[float]) -> Dict[int, List[int]]:
        query = (
            "MATCH (c:__Community__) WHERE id(c) IN $communityIds "
            "CALL { WITH c MATCH (c)<-[:IN_COMMUNITY]-(:Node)-[:IN_CHUNK]->(ch:__Chunk__) "
            "WITH ch CALL db.index.vector.queryNodes($chunkIndex, 50, $qvec) YIELD node AS cand, score "
            "WHERE cand = ch RETURN cand AS chunk, score ORDER BY score DESC LIMIT 3 } "
            "RETURN id(c) AS cid, collect(id(chunk)) AS chunk_ids"
        )
        res = list(self._session.run(query, communityIds=community_ids, qvec=qvec, chunkIndex=chunk_index))
        out: Dict[int, List[int]] = {}
        for row in res:
            cid = int(row.get("cid")) if row.get("cid") is not None else None
            if cid is None:
                continue
            out[cid] = [int(x) for x in (row.get("chunk_ids") or [])]
        return out

    def fetch_community_summaries(self, ids: List[int]) -> Dict[int, str]:
        rows = list(self._session.run(
            "MATCH (c:__Community__) WHERE id(c) IN $ids RETURN id(c) AS id, c.summary AS summary",
            ids=ids,
        ))
        out: Dict[int, str] = {}
        for r in rows:
            try:
                cid = int(r.get("id"))
                out[cid] = r.get("summary") or ""
            except Exception:
                continue
        return out

    def scoped_chunk_ids(self, cids: List[int], chunk_index: str, qvec: List[float]) -> List[int]:
        scoped_q = (
            "WITH $qvec AS qvec, $cids AS cids "
            "MATCH (c:__Community__) WHERE id(c) IN cids "
            "MATCH (c)<-[:IN_COMMUNITY]-(:Node)-[:IN_CHUNK]->(ch:__Chunk__) "
            "WITH DISTINCT ch, qvec CALL db.index.vector.queryNodes($chunkIndex, 200, qvec) YIELD node AS cand, score "
            "WHERE cand = ch RETURN id(ch) AS cid ORDER BY score DESC LIMIT 30"
        )
        rows = list(self._session.run(scoped_q, qvec=qvec, cids=cids, chunkIndex=chunk_index))
        return [int(r.get("cid")) for r in rows if r.get("cid") is not None]

    def expand_neighborhood_minimal(self, chunk_ids: List[int]) -> List[Dict[str, Any]]:
        query = (
            "UNWIND $chunkIds AS cid "
            "MATCH (ch:__Chunk__) WHERE id(ch) = cid "
            "OPTIONAL MATCH (e)-[:IN_CHUNK]->(ch) "
            "WHERE e:__Entity__ OR e:Entity "
            "WITH ch, cid, e "
            "WITH cid, ch, collect({ _id: id(e), properties: { "
            "name: e.name, description: e.description, type: coalesce(e.type, e.category, ''), "
            "communities: toString(e.communities) } })[0..5] AS neigh "
            "RETURN cid AS chunk_id, ch.text AS text, neigh AS neighbours"
        )
        rows = list(self._session.run(query, chunkIds=chunk_ids[:3]))
        result: List[Dict[str, Any]] = []
        for r in rows:
            result.append({
                "chunk_id": int(r.get("chunk_id")) if r.get("chunk_id") is not None else None,
                "text": r.get("text") or "",
                "neighbours": r.get("neighbours") or [],
            })
        return result


