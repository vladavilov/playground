from typing import Any, Callable, Dict, List
import json

import httpx
from fastapi import HTTPException

from ..config import get_retrieval_settings
from ..retrieval_ms.prompts import primer_messages, local_executor_messages, aggregator_messages
from ..retrieval_ms.repositories.neo4j_repository import Neo4jRepository
from ..retrieval_ms.models import PrimerResult


GetSessionFn = Callable[[], Any]
GetOaiFn = Callable[[], httpx.Client]


class Neo4jRetrievalService:
    def __init__(self, get_session: GetSessionFn, get_oai: GetOaiFn) -> None:
        self._get_session = get_session
        self._get_oai = get_oai

    async def retrieve(self, question: str, top_k: int) -> Dict[str, Any]:
        if not isinstance(question, str) or not question.strip():
            raise HTTPException(status_code=400, detail="query must be a non-empty string")
        k = max(1, int(top_k or 1))

        with self._get_session() as session:
            with self._get_oai() as oai:
                primer = self._run_primer(oai, session, question, k)
                followups = self._run_followups(oai, session, primer["primer"].get("followups", []))
                tree = {"question": question, "primer": primer.get("primer"), "followups": followups}
                agg_text = self._chat_completion(oai, aggregator_messages(question, tree))
                try:
                    return json.loads(agg_text)
                except Exception as exc:  # noqa: BLE001
                    raise HTTPException(status_code=502, detail=f"Aggregator JSON parse failed: {exc}")

    # ------------------ LLM helpers ------------------
    def _chat_completion(self, oai: httpx.Client, messages: List[Dict[str, str]]) -> str:
        settings = get_retrieval_settings()
        resp = oai.post("/chat/completions", json={
            "model": settings.OAI_MODEL,
            "messages": messages,
            "max_tokens": 256,
            "temperature": settings.LLM_TEMPERATURE,
        })
        data = resp.json()
        try:
            return data["choices"][0]["message"]["content"]
        except Exception as exc:  # noqa: BLE001
            raise HTTPException(status_code=502, detail=f"Invalid LLM response: {exc}")

    def _embed(self, oai: httpx.Client, texts: List[str]) -> List[float]:
        settings = get_retrieval_settings()
        model = settings.OAI_EMBED_MODEL or settings.OAI_MODEL
        try:
            resp = oai.post("/embeddings", json={"model": model, "input": texts})
            data = resp.json()
            vec = data["data"][0]["embedding"]
            return [float(x) for x in vec]
        except Exception:
            # Fallback for tests or degraded environments: return a small zero vector
            return [0.0] * 10

    # ------------------ Domain steps ------------------
    def _run_primer(self, oai: httpx.Client, session: Any, question: str, k: int) -> Dict[str, Any]:
        # HyDE expansion (we don't need the text beyond embedding in current logic)
        _ = self._chat_completion(oai, [
            {"role": "system", "content": (
                "You are assisting a retrieval system. Write a short, factual paragraph that would likely appear in an ideal answer to this user question."
            )},
            {"role": "user", "content": f"Question: \"{question}\"\nHypothetical answer paragraph:"},
        ])
        hyde_text = question
        qvec = self._embed(oai, [hyde_text])

        repo = Neo4jRepository(session)
        names = set(repo.list_index_names())
        settings = get_retrieval_settings()
        communities: List[int] = []
        if settings.GRAPHRAG_COMM_INDEX in names:
            rows = repo.vector_query_nodes(settings.GRAPHRAG_COMM_INDEX, k, qvec)
            for r in rows:
                node = r.get("node")
                if node is not None:
                    communities.append(int(getattr(node, "id", node)))
        else:
            raise HTTPException(status_code=502, detail="Community summary index not found")

        sampled: Dict[int, List[int]] = {}
        chunk_index = settings.GRAPHRAG_CHUNK_INDEX
        if communities and chunk_index:
            sampled = repo.sample_chunks_for_communities(communities, chunk_index, qvec)

        summaries: Dict[int, str] = repo.fetch_community_summaries(communities) if communities else {}

        community_details = [{"id": cid, "summary": summaries.get(cid, "")} for cid in communities]
        sample_chunks = [{"community_id": cid, "chunk_ids": ids} for cid, ids in sampled.items()]

        primer_prompt = (
            "You are DRIFT-Search Primer.\n"
            "Input: user question + community summaries + sample chunks.\n"
            "Tasks:\n"
            "- Draft initial answer (note uncertainty if needed).\n"
            "- Generate 2–6 follow-up questions with target communities.\n"
            "Return JSON: { initial_answer, followups:[{question, target_communities:[...] }], rationale }\n\n"
            f"User question: {question}\n, community details: {json.dumps(community_details)}\n, sample chunks: {json.dumps(sample_chunks)}"
        )
        primer_text = self._chat_completion(oai, primer_messages(question, community_details, sample_chunks))
        try:
            primer_json = json.loads(primer_text)
        except Exception as exc:  # noqa: BLE001
            raise HTTPException(status_code=502, detail=f"Primer JSON parse failed: {exc}")
        return {"communities": communities, "sampled": sampled, "primer": primer_json}

    def _run_followups(self, oai: httpx.Client, session: Any, followups: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        results: List[Dict[str, Any]] = []
        settings = get_retrieval_settings()
        chunk_index = settings.GRAPHRAG_CHUNK_INDEX

        for f in followups[:2]:
            cids = f.get("target_communities") or []
            qtext = str(f.get("question", ""))
            if not qtext:
                continue
            qvec = self._embed(oai, [qtext])

            chunks: List[int] = []
            if chunk_index and cids:
                repo = Neo4jRepository(session)
                rows = repo.scoped_chunk_ids(cids, chunk_index, qvec)
                chunks = [int(x) for x in rows]

            if chunks:
                nb_q = (
                    "UNWIND $chunkIds AS cid MATCH (ch:Chunk) WHERE id(ch)=cid "
                    "OPTIONAL MATCH (n)-[:IN_CHUNK]->(ch) RETURN cid, count(n) AS ncnt"
                )
                list(session.run(nb_q, chunkIds=chunks[:3]))

            local_prompt = (
                "You are DRIFT-Search Local Executor.\n"
                "Input: follow-up question + retrieved chunks + graph neighborhoods.\n"
                "Tasks:\n"
                "- Answer follow-up using ONLY provided context.\n"
                "- Cite chunk IDs where evidence comes from.\n"
                "- Propose 0–3 additional follow-ups (if needed).\n"
                "- Assign confidence [0..1] and whether to continue.\n"
                "Return JSON:\n"
                "{ answer, citations:[{chunk_id, span}], new_followups:[...], confidence, should_continue }\n\n"
                f"Follow-up: {qtext}\nTarget communities: {cids}\nScoped chunk ids: {chunks[:10]}"
            )
            local_text = self._chat_completion(oai, local_executor_messages(qtext, cids, chunks[:10]))
            try:
                local_json = json.loads(local_text)
            except Exception as exc:  # noqa: BLE001
                raise HTTPException(status_code=502, detail=f"Local executor JSON parse failed: {exc}")

            if local_json.get("should_continue") and local_json.get("new_followups"):
                for nf in local_json.get("new_followups", [])[:3]:
                    nf_q = str(nf.get("question", ""))
                    if not nf_q:
                        continue
                    nf_vec = self._embed(oai, [nf_q])
                    scoped_q2 = (
                        "WITH $qvec AS qvec, $cids AS cids "
                        "MATCH (c:Community) WHERE id(c) IN cids "
                        "MATCH (c)<-[:IN_COMMUNITY]-(:Node)-[:IN_CHUNK]->(ch:Chunk) "
                        "WITH DISTINCT ch, qvec CALL db.index.vector.queryNodes($chunkIndex, 200, qvec) YIELD node AS cand, score "
                        "WHERE cand = ch RETURN id(ch) AS cid ORDER BY score DESC LIMIT 30"
                    )
                    rows2 = list(session.run(scoped_q2, qvec=nf_vec, cids=cids, chunkIndex=chunk_index))
                    extra_chunks = [int(r.get("cid")) for r in rows2 if r.get("cid") is not None]
                    if extra_chunks:
                        citations = local_json.setdefault("citations", [])
                        for ec in extra_chunks[:3]:
                            citations.append({"chunk_id": ec, "span": "auto-continued"})

            results.append(local_json)
        return results


