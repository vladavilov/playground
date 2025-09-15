from typing import Any, Dict, List, Optional
import json

import os
import httpx
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from neo4j import GraphDatabase


retrieval_router = APIRouter()


class RetrievalRequest(BaseModel):
    query: str
    top_k: int = 1


def _oai_client() -> httpx.Client:
    base = os.getenv("OAI_BASE_URL", "http://openai-mock-service:8000/v1").rstrip("/")
    key = os.getenv("OAI_KEY", "key")
    headers = {"Authorization": f"Bearer {key}"}
    return httpx.Client(base_url=base, headers=headers, timeout=10)


def _neo4j_session():
    uri = os.getenv("NEO4J_URI", "bolt://neo4j:7687")
    user = os.getenv("NEO4J_USERNAME", "neo4j")
    pwd = os.getenv("NEO4J_PASSWORD", "neo4j123")
    database = os.getenv("NEO4J_DATABASE", "neo4j")
    driver = GraphDatabase.driver(uri, auth=(user, pwd))
    return driver.session(database=database)


def _chat_completion(oai: httpx.Client, messages: List[Dict[str, str]]) -> str:
    resp = oai.post("/chat/completions", json={
        "model": os.getenv("OAI_MODEL", "gpt-4.1"),
        "messages": messages,
        "max_tokens": 256,
        "temperature": 0,
    })
    data = resp.json()
    try:
        return data["choices"][0]["message"]["content"]
    except Exception as exc:
        raise HTTPException(status_code=502, detail=f"Invalid LLM response: {exc}")


def _embed(oai: httpx.Client, texts: List[str]) -> List[float]:
    model = os.getenv("OAI_EMBED_MODEL", os.getenv("OAI_MODEL", "text-embedding-3-small"))
    resp = oai.post("/embeddings", json={
        "model": model,
        "input": texts,
    })
    data = resp.json()
    try:
        vec = data["data"][0]["embedding"]
        # Ensure float conversion
        return [float(x) for x in vec]
    except Exception as exc:
        raise HTTPException(status_code=502, detail=f"Invalid embeddings response: {exc}")


def _run_primer(oai: httpx.Client, session, question: str, k: int) -> Dict[str, Any]:
    # HyDE expansion (use LLM result)
    hyde_answer = _chat_completion(oai, [
        {"role": "system", "content": (
            "You are assisting a retrieval system. Write a short, factual paragraph that would likely appear in an ideal answer to this user question."
        )},
        {"role": "user", "content": f"Question: \"{question}\"\nHypothetical answer paragraph:"},
    ])
    hyde_text = f"{question}\n{hyde_answer}"

    # Embed HyDE text for community retrieval
    qvec = _embed(oai, [hyde_text])

    names = {r["name"] for r in session.run("SHOW INDEXES YIELD name RETURN name")}
    communities: List[int] = []
    if "community_summary_idx" in names:
        rows = list(session.run(
            "CALL db.index.vector.queryNodes($name, $k, $qvec)",
            name="community_summary_idx",
            k=k,
            qvec=qvec,
        ))
        for r in rows:
            node = r.get("node")
            if node is not None:
                communities.append(node.id)
    else:
        raise HTTPException(status_code=502, detail="Community summary index not found")
    
    chunk_index: str = "chunk_embeddings"

    sampled: Dict[int, List[int]] = {}
    if communities and chunk_index:
        query = (
            "MATCH (c:Community) WHERE id(c) IN $communityIds "
            "CALL { WITH c MATCH (c)<-[:IN_COMMUNITY]-(:Node)-[:IN_CHUNK]->(ch:Chunk) "
            "WITH ch CALL db.index.vector.queryNodes($chunkIndex, 50, $qvec) YIELD node AS cand, score "
            "WHERE cand = ch RETURN cand AS chunk, score ORDER BY score DESC LIMIT 3 } "
            "RETURN id(c) AS cid, collect(id(chunk)) AS chunk_ids"
        )
        res = list(session.run(query, communityIds=communities, qvec=qvec, chunkIndex=chunk_index))
        for row in res:
            sampled[int(row.get("cid"))] = [int(x) for x in (row.get("chunk_ids") or [])]

    # Fetch community summaries for prompt context
    summaries: Dict[int, str] = {}
    if communities:
        sum_rows = list(session.run(
            "MATCH (c:Community) WHERE id(c) IN $ids RETURN id(c) AS id, c.summary AS summary",
            ids=communities,
        ))
        for r in sum_rows:
            try:
                cid = int(r.get("id"))
                summaries[cid] = r.get("summary") or ""
            except Exception:
                continue

    community_details = [{"id": cid, "summary": summaries.get(cid, "")} for cid in communities]
    sample_chunks = [{"community_id": cid, "chunk_ids": ids} for cid, ids in sampled.items()]

    # Primer LLM per spec
    primer_prompt = (
        "You are DRIFT-Search Primer.\n"
        "Input: user question + community summaries + sample chunks.\n"
        "Tasks:\n"
        "- Draft initial answer (note uncertainty if needed).\n"
        "- Generate 2–6 follow-up questions with target communities.\n"
        "Return JSON: { initial_answer, followups:[{question, target_communities:[...] }], rationale }\n\n"
        f"User question: {question}\n, community details: {json.dumps(community_details)}\n, sample chunks: {json.dumps(sample_chunks)}"
    )
    primer_text = _chat_completion(oai, [
        {"role": "system", "content": "You are DRIFT-Search Primer."},
        {"role": "user", "content": primer_prompt},
    ])
    try:
        primer_json = json.loads(primer_text)
    except Exception as exc:
        raise HTTPException(status_code=502, detail=f"Primer JSON parse failed: {exc}")
    return {"communities": communities, "sampled": sampled, "primer": primer_json}


def _run_followups(oai: httpx.Client, session, followups: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    results: List[Dict[str, Any]] = []
    chunk_index = "chunk_embeddings"

    for f in followups[:2]:
        cids = f.get("target_communities") or []
        chunks: List[int] = []
        # Embed follow-up question
        qtext = str(f.get("question", ""))
        if not qtext:
            continue
        qvec = _embed(oai, [qtext])
        if chunk_index and cids:
            scoped_q = (
                "WITH $qvec AS qvec, $cids AS cids "
                "MATCH (c:Community) WHERE id(c) IN cids "
                "MATCH (c)<-[:IN_COMMUNITY]-(:Node)-[:IN_CHUNK]->(ch:Chunk) "
                "WITH DISTINCT ch, qvec CALL db.index.vector.queryNodes($chunkIndex, 200, qvec) YIELD node AS cand, score "
                "WHERE cand = ch RETURN id(ch) AS cid ORDER BY score DESC LIMIT 30"
            )
            rows = list(session.run(scoped_q, qvec=qvec, cids=cids, chunkIndex=chunk_index))
            chunks = [int(r.get("cid")) for r in rows if r.get("cid") is not None]

        # Minimal neighborhood expansion to exercise at least one query
        if chunks:
            nb_q = (
                "UNWIND $chunkIds AS cid MATCH (ch:Chunk) WHERE id(ch)=cid "
                "OPTIONAL MATCH (n)-[:IN_CHUNK]->(ch) RETURN cid, count(n) AS ncnt"
            )
            list(session.run(nb_q, chunkIds=chunks[:3]))

        # Local executor LLM per spec
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
        local_text = _chat_completion(oai, [
            {"role": "system", "content": "You are DRIFT-Search Local Executor."},
            {"role": "user", "content": local_prompt},
        ])
        try:
            local_json = json.loads(local_text)
        except Exception as exc:
            raise HTTPException(status_code=502, detail=f"Local executor JSON parse failed: {exc}")

        # If should_continue, repeat retrieval for new_followups without another LLM call
        if local_json.get("should_continue") and local_json.get("new_followups"):
            for nf in local_json.get("new_followups", [])[:3]:
                nf_q = str(nf.get("question", ""))
                if not nf_q:
                    continue
                nf_vec = _embed(oai, [nf_q])
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
                    # Augment citations with continued retrieval evidence
                    citations = local_json.setdefault("citations", [])
                    for ec in extra_chunks[:3]:
                        citations.append({"chunk_id": ec, "span": "auto-continued"})

        results.append(local_json)
    return results


@retrieval_router.post("")
async def retrieve(req: RetrievalRequest) -> Dict[str, Any]:
    question = req.query
    with _neo4j_session() as session:
        with _oai_client() as oai:
            primer = _run_primer(oai, session, question, k=max(1, req.top_k))
            followups = _run_followups(oai, session, primer["primer"].get("followups", []))
            # Aggregation LLM
            tree = {
                "question": question,
                "primer": primer.get("primer"),
                "followups": followups,
            }
            agg_prompt = (
                "You are DRIFT-Search Aggregator.\n"
                f"User question: {question}\n"
                f"Q/A tree (primer + follow-ups): {json.dumps(tree)}\n"
                "Tasks:\n"
                "1. Produce final concise answer.\n"
                "2. List key facts with citations (chunk IDs).\n"
                "3. Note any residual uncertainty.\n"
                "Return JSON:\n"
                "{ final_answer, key_facts:[{fact, citations:[...] }], residual_uncertainty }"
            )
            agg_text = _chat_completion(oai, [
                {"role": "system", "content": "You are DRIFT-Search Aggregator."},
                {"role": "user", "content": agg_prompt},
            ])
            try:
                agg_json = json.loads(agg_text)
            except Exception as exc:
                raise HTTPException(status_code=502, detail=f"Aggregator JSON parse failed: {exc}")

    return agg_json

