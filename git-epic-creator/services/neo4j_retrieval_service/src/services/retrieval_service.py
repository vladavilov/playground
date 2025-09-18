from typing import Any, Callable, Dict, List
import json
import logging

import httpx
from fastapi import HTTPException

from ..config import get_retrieval_settings
from ..retrieval_ms.prompts import primer_messages, local_executor_messages, aggregator_messages, hyde_messages, build_hyde_embed_text
from ..retrieval_ms.repositories.neo4j_repository import Neo4jRepository


GetSessionFn = Callable[[], Any]
GetOaiFn = Callable[[], httpx.Client]

logger = logging.getLogger(__name__)


class Neo4jRetrievalService:
    def __init__(self, get_session: GetSessionFn, get_oai: GetOaiFn) -> None:
        self._get_session = get_session
        self._get_oai = get_oai

    async def retrieve(self, question: str, top_k: int) -> Dict[str, Any]:
        if not isinstance(question, str) or not question.strip():
            raise HTTPException(status_code=400, detail="query must be a non-empty string")
        k = max(5, int(top_k or 1))

        with self._get_session() as session:
            with self._get_oai() as oai:
                primer = self._run_primer(oai, session, question, k)
                logger.info("retrieve primer done: primer=%r", primer)
                followups = self._run_followups(
                    oai,
                    session,
                    primer["primer"].get("followups", []),
                    primer.get("communities", []),
                    k,
                )
                logger.info("retrieve followups done: followups=%r", followups)
                tree = {"question": question, "primer": primer.get("primer"), "followups": followups}
                agg_text = self._chat_completion(oai, aggregator_messages(question, tree))
                try:
                    return json.loads(agg_text)
                except Exception as exc:  # noqa: BLE001
                    raise HTTPException(status_code=502, detail=f"Aggregator JSON parse failed: {exc}")

    # ------------------ LLM helpers ------------------
    def _chat_completion(self, oai: httpx.Client, messages: List[Dict[str, str]]) -> str:
        settings = get_retrieval_settings()
        logger.info("LLM chat_completion start: model=%s, messages=%r", settings.OAI_MODEL, messages)
        resp = oai.post("/chat/completions", json={
            "model": settings.OAI_MODEL,
            "messages": messages,
            "max_tokens": 256,
            "temperature": settings.LLM_TEMPERATURE,
        })
        data = resp.json()
        try:
            content = data["choices"][0]["message"]["content"]
            usage = data.get("usage") or {}
            logger.info("LLM chat_completion done: content=%r, usage=%r", content, usage)
            return content
        except Exception as exc:  # noqa: BLE001
            raise HTTPException(status_code=502, detail=f"Invalid LLM response: {exc}")

    def _embed(self, oai: httpx.Client, texts: List[str]) -> List[float]:
        settings = get_retrieval_settings()
        model = settings.OAI_EMBED_MODEL or settings.OAI_MODEL
        try:
            resp = oai.post("/embeddings", json={"model": model, "input": texts})
            data = resp.json()
            vec = data["data"][0]["embedding"]
            result = [float(x) for x in vec]
            return result
        except Exception as exc:  # noqa: BLE001
            raise HTTPException(status_code=502, detail=f"Invalid Embeddings response: {exc}")

    # ------------------ Domain steps ------------------
    def _run_primer(self, oai: httpx.Client, session: Any, question: str, k: int) -> Dict[str, Any]:
        # HyDE expansion: use model response, and embed "question + LLM answer" per spec
        hyde_text_resp = self._chat_completion(oai, hyde_messages(question))
        hyde_text = build_hyde_embed_text(question, hyde_text_resp)
        qvec = self._embed(oai, [hyde_text])

        repo = Neo4jRepository(session)
        names = set(repo.list_index_names())
        settings = get_retrieval_settings()
        communities: List[int] = []
        if settings.GRAPHRAG_COMM_INDEX in names:
            logger.info("Repo.vector_query_nodes start: index=%s, k=%d, qvec=%r", settings.GRAPHRAG_COMM_INDEX, k, len(qvec))
            rows = repo.vector_query_nodes(settings.GRAPHRAG_COMM_INDEX, k, qvec)
            logger.info("Repo.vector_query_nodes done: rows=%r", len(rows))
            for r in rows:
                node = r.get("node")
                if node is not None:
                    communities.append(int(getattr(node, "id", node)))
        else:
            raise HTTPException(status_code=502, detail="Community summary index not found")

        sampled: Dict[int, List[int]] = {}
        chunk_index = settings.GRAPHRAG_CHUNK_INDEX
        if communities and chunk_index:
            logger.info("Repo.sample_chunks_for_communities start: communities=%r, index=%s, qvec=%r", communities, chunk_index, len(qvec))
            sampled = repo.sample_chunks_for_communities(communities, chunk_index, qvec)
            logger.info("Repo.sample_chunks_for_communities done: sampled=%r", sampled)

        if communities:
            logger.info("Repo.fetch_community_summaries start: communities=%r", communities)
            summaries: Dict[int, str] = repo.fetch_community_summaries(communities)
            logger.info("Repo.fetch_community_summaries done: summaries=%r", summaries)
        else:
            summaries = {}

        if communities:
            logger.info("Repo.fetch_communities_brief start: communities=%r", communities)
            community_brief = Neo4jRepository(session).fetch_communities_brief(communities)
            logger.info("Repo.fetch_communities_brief done: community_brief=%r", community_brief)
        else:
            community_brief = []
        # Fallback to id+summary if name missing
        if not community_brief and communities:
            community_brief = [{"id": cid, "summary": summaries.get(cid, "")} for cid in communities]
        sample_chunks = [{"community_id": cid, "chunk_ids": ids} for cid, ids in sampled.items()]

        primer_text = self._chat_completion(oai, primer_messages(question, community_brief, sample_chunks))
        try:
            primer_json = json.loads(primer_text)
        except Exception as exc:  # noqa: BLE001
            raise HTTPException(status_code=502, detail=f"Primer JSON parse failed: {exc}")
        return {"communities": communities, "sampled": sampled, "primer": primer_json}

    def _run_followups(self, oai: httpx.Client, session: Any, followups: List[Dict[str, Any]], communities: List[int], k: int) -> List[Dict[str, Any]]:
        results: List[Dict[str, Any]] = []
        settings = get_retrieval_settings()
        chunk_index = settings.GRAPHRAG_CHUNK_INDEX

        for f in followups:
            # Ignore any target_communities on the followup; use primer communities instead
            cids = communities or []
            qtext = str(f.get("question", ""))
            if not qtext:
                continue
            qvec = self._embed(oai, [qtext])

            chunks: List[int] = []
            if chunk_index and cids:
                repo = Neo4jRepository(session)
                logger.info("Repo.scoped_chunk_ids start: cids=%r, index=%s, qvec=%r", cids, chunk_index, len(qvec))
                rows = repo.scoped_chunk_ids(cids, chunk_index, qvec, k)
                logger.info("Repo.scoped_chunk_ids done: rows=%r", rows)
                chunks = [int(x) for x in rows]
            
            if chunks:
                logger.info("Repo.expand_neighborhood_minimal start: seeds=%r", chunks)
                chunks = Neo4jRepository(session).expand_neighborhood_minimal(chunks)
                logger.info("Repo.expand_neighborhood_minimal done: chunks=%r", chunks)

            # Replace target_communities ids with brief community objects for the LLM
            if cids:
                logger.info("Repo.fetch_communities_brief start: cids=%r", cids)
                target_communities_brief = Neo4jRepository(session).fetch_communities_brief(cids)
                logger.info("Repo.fetch_communities_brief done: target_communities_brief=%r", target_communities_brief)
            else:
                target_communities_brief = []

            local_text = self._chat_completion(oai, local_executor_messages(qtext, target_communities_brief, chunks))
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
                    repo = Neo4jRepository(session)
                    logger.info("Repo.scoped_chunk_ids start (nf): cids=%r, index=%s, qvec=%r", cids, chunk_index, len(nf_vec))
                    extra_chunks = repo.scoped_chunk_ids(cids, chunk_index, nf_vec, k)
                    logger.info("Repo.scoped_chunk_ids done (nf): rows=%r", extra_chunks)
                    if extra_chunks:
                        logger.info("Repo.expand_neighborhood_minimal start (nf): seeds=%r", extra_chunks)
                        expanded = Neo4jRepository(session).expand_neighborhood_minimal(extra_chunks)
                        logger.info("Repo.expand_neighborhood_minimal done (nf): expanded=%r", expanded)
                        nf["answer_context"] = expanded

            # remove target_communities from final JSON
            if isinstance(local_json, dict) and "target_communities" in local_json:
                local_json.pop("target_communities", None)

            results.append(local_json)
        return results
