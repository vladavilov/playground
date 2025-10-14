from typing import Any, Callable, Dict, List, TypedDict, Annotated
from operator import add
import json
from contextlib import contextmanager
import structlog

from langgraph.graph import StateGraph, START, END
from langgraph.checkpoint.memory import InMemorySaver
from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_core.messages.utils import trim_messages, count_tokens_approximately
from fastapi import HTTPException

from config import get_retrieval_settings
from retrieval_ms.prompts import (
    hyde_prompt,
    build_hyde_embed_text,
    primer_prompt,
    local_executor_prompt,
    aggregator_prompt,
)
from retrieval_ms.repositories.neo4j_repository import Neo4jRepository


GetSessionFn = Callable[[], Any]
GetLlmFn = Callable[[], ChatOpenAI]
GetEmbedderFn = Callable[[], OpenAIEmbeddings]

logger = structlog.get_logger(__name__)


class _State(TypedDict, total=False):
    question: str
    top_k: int
    project_id: str
    messages: Annotated[List[Any], add]
    hyde_text: str
    qvec: List[float]
    communities: List[int]
    sampled_chunks: Dict[int, List[int]]
    community_brief: List[Dict[str, Any]]
    primer_json: Dict[str, Any]
    followups: List[Dict[str, Any]]
    followup_results: List[Dict[str, Any]]
    tree: Dict[str, Any]
    agg_text: str
    result_json: Dict[str, Any]


async def _create_graph(get_session: GetSessionFn, get_llm: GetLlmFn, get_embedder: GetEmbedderFn):
    settings = get_retrieval_settings()

    builder = StateGraph(_State)


    @contextmanager
    def _repo_ctx():
        with get_session() as session:
            yield Neo4jRepository(session)

    def _ensure_top_k(state: Dict[str, Any]) -> int:
        try:
            return max(5, int(state.get("top_k") or 1))
        except Exception:  # noqa: BLE001
            return 5

    def _as_str_content(obj: Any) -> str:
        return obj.content if hasattr(obj, "content") else str(obj)

    def _parse_json_or_502(raw: Any, label: str) -> Dict[str, Any]:
        try:
            parsed = json.loads(_as_str_content(raw))
        except Exception as exc:  # noqa: BLE001
            raise HTTPException(status_code=502, detail=f"{label} JSON parse failed: {exc}")
        return parsed if isinstance(parsed, dict) else {}

    async def _embed_one_or_502(text: str) -> List[float]:
        embedder = get_embedder()
        try:
            vectors = await embedder.aembed_documents([text])
            return [float(x) for x in (vectors[0] if vectors else [])]
        except Exception as exc:  # noqa: BLE001
            raise HTTPException(status_code=502, detail=f"Embeddings failed: {exc}")

    def _trim_msgs(msgs: List[Any]) -> List[Any]:
        if not msgs:
            return msgs
        return trim_messages(
            msgs,
            strategy="last",
            token_counter=count_tokens_approximately,
            max_tokens=256,
            start_on="human",
            end_on=("human", "tool"),
        )

    async def _format_invoke_parse(prompt_obj: Any, llm: ChatOpenAI, label: str, **fmt_kwargs: Any) -> Dict[str, Any]:
        msg = prompt_obj.format_messages(**fmt_kwargs)
        res = await llm.ainvoke(msg)
        return _parse_json_or_502(res, label)

    async def _format_invoke_content(prompt_obj: Any, llm: ChatOpenAI, **fmt_kwargs: Any) -> str:
        msg = prompt_obj.format_messages(**fmt_kwargs)
        res = await llm.ainvoke(msg)
        return _as_str_content(res)

    def _fetch_communities(repo: Neo4jRepository, index_name: str, k: int, qvec: List[float], project_id: str) -> List[int]:
        rows = repo.vector_query_nodes(index_name, k, qvec, project_id)
        communities: List[int] = []
        for r in rows:
            node = r.get("node")
            if node is not None:
                communities.append(int(node["community"]))
        return communities

    def _sample_chunks(
        repo: Neo4jRepository,
        communities: List[int],
        chunk_index: str,
        qvec: List[float],
        project_id: str,
    ) -> Dict[int, List[int]]:
        if not (communities and chunk_index):
            return {}
        return repo.sample_chunks_for_communities(communities, chunk_index, qvec, project_id)

    def _fetch_community_brief(
        repo: Neo4jRepository, communities: List[int], project_id: str
    ) -> List[Dict[str, Any]]:
        if not communities:
            return []
        summaries: Dict[int, str] = repo.fetch_community_summaries(communities, project_id)
        brief = repo.fetch_communities_brief(communities, project_id)
        if brief:
            return brief
        return [{"id": cid, "summary": summaries.get(cid, "")} for cid in communities]

    def _scoped_chunks_expanded(
        repo: Neo4jRepository,
        communities: List[int],
        chunk_index: str,
        query_vec: List[float],
        k: int,
        project_id: str,
    ) -> List[Dict[str, Any]]:
        if not (chunk_index and communities):
            logger.warning("retrieval.scoped_chunks_empty", reason="missing_chunk_index_or_communities", chunk_index=bool(chunk_index), communities_count=len(communities or []))
            return []
        rows = repo.scoped_chunk_ids(communities, chunk_index, query_vec, k, project_id)
        chunks = rows
        logger.info("retrieval.scoped_chunks", chunk_ids_found=len(chunks), communities_count=len(communities or []))
        if not chunks:
            logger.warning("retrieval.scoped_chunks_empty", reason="no_chunks_found_for_communities", communities=communities[:5])
            return []
        return repo.expand_neighborhood_minimal(chunks, project_id)

    async def init_node(state: Dict[str, Any]) -> Dict[str, Any]:
        msgs = state.get("messages") or []
        logger.info("retrieval.init", messages=len(msgs))
        return {"messages": _trim_msgs(msgs)}

    async def hyde_node(state: Dict[str, Any]) -> Dict[str, Any]:
        llm = get_llm()
        prompt = hyde_prompt()
        hyde_answer = await _format_invoke_content(prompt, llm, question=state["question"])
        hyde_text = build_hyde_embed_text(state["question"], hyde_answer)
        qvec = await _embed_one_or_502(hyde_text)
        logger.info("retrieval.hyde.done", hyde_text_len=len(hyde_text or ""), qvec_len=len(qvec or []))
        return {"hyde_text": hyde_text, "qvec": qvec}

    async def primer_node(state: Dict[str, Any]) -> Dict[str, Any]:
        k = _ensure_top_k(state)
        with _repo_ctx() as repo:
            communities = _fetch_communities(
                repo,
                settings.vector_index.COMMUNITY_VECTOR_INDEX_NAME,
                k,
                state["qvec"],
                state["project_id"],
            )
            chunk_index = settings.vector_index.CHUNK_VECTOR_INDEX_NAME
            sampled = _sample_chunks(
                repo,
                communities,
                chunk_index,
                state["qvec"],
                state["project_id"],
            )
            community_brief = _fetch_community_brief(repo, communities, state["project_id"])

        sample_chunks = [{"community_id": cid, "chunk_ids": ids} for cid, ids in sampled.items()]
        llm = get_llm()
        prompt = primer_prompt()
        primer_json = await _format_invoke_parse(
            prompt,
            llm,
            "Primer",
            question=state["question"],
            community_details=json.dumps(community_brief),
            sample_chunks=json.dumps(sample_chunks),
        )
        followups = list(primer_json.get("followups", []))
        logger.info(
                "retrieval.primer.done",
                communities=len(communities or []),
                sampled_total=sum(len(v or []) for v in sampled.values()),
                community_brief=len(community_brief or []),
                followups=len(followups or []),
            )
        return {
            "communities": communities,
            "sampled_chunks": sampled,
            "community_brief": community_brief,
            "primer_json": primer_json,
            "followups": followups,
        }

    async def followups_node(state: Dict[str, Any]) -> Dict[str, Any]:
        k = _ensure_top_k(state)
        cids = list(state.get("communities") or [])
        llm = get_llm()
        results: List[Dict[str, Any]] = []
        chunk_index = settings.vector_index.CHUNK_VECTOR_INDEX_NAME
        with _repo_ctx() as repo:
            for f in list(state.get("followups") or []):
                qtext = str(f.get("question", ""))
                if not qtext:
                    continue
                qvec = await _embed_one_or_502(qtext)

                chunks = _scoped_chunks_expanded(
                    repo,
                    cids,
                    chunk_index,
                    qvec,
                    k,
                    state["project_id"],
                )

                target_communities_brief = (
                    _fetch_community_brief(repo, cids, state["project_id"]) if cids else []
                )

                prompt = local_executor_prompt()
                local_json = await _format_invoke_parse(
                    prompt,
                    llm,
                    "Local executor",
                    qtext=qtext,
                    target_communities=json.dumps(target_communities_brief),
                    chunks_preview=json.dumps(chunks),
                )

                # Preserve retrieval context for aggregation stage
                local_json["context"] = chunks

                if "target_communities" in local_json:
                    local_json.pop("target_communities", None)

                if local_json.get("should_continue") and local_json.get("new_followups"):
                    for nf in list(local_json.get("new_followups", []))[:3]:
                        nf_q = str(nf.get("question", ""))
                        if not nf_q:
                            continue
                        nf_vec = await _embed_one_or_502(nf_q)
                        expanded = _scoped_chunks_expanded(
                            repo,
                            cids,
                            chunk_index,
                            nf_vec,
                            k,
                            state["project_id"],
                        )
                        if expanded:
                            nf["answer_context"] = expanded

                results.append(local_json)
        logger.info("retrieval.followups.done", processed=len(results))
        return {"followup_results": results}

    async def aggregate_node(state: Dict[str, Any]) -> Dict[str, Any]:
        llm = get_llm()
        prompt = aggregator_prompt()
        tree = {
            "question": state["question"],
            "primer": state.get("primer_json"),
            "followups": state.get("followup_results"),
        }
        result_json = await _format_invoke_parse(
            prompt,
            llm,
            "Aggregator",
            question=state["question"],
            tree=json.dumps(tree),
        )
        logger.info("retrieval.aggregate.done", result_keys=len(list(result_json.keys())))
        return {"tree": tree, "result_json": result_json}

    builder.add_node("init", init_node)
    builder.add_node("hyde", hyde_node)
    builder.add_node("primer", primer_node)
    builder.add_node("followups", followups_node)
    builder.add_node("aggregate", aggregate_node)

    builder.add_edge(START, "init")
    builder.add_edge("init", "hyde")
    builder.add_edge("hyde", "primer")
    builder.add_edge("primer", "followups")
    builder.add_edge("followups", "aggregate")
    builder.add_edge("aggregate", END)

    graph = builder.compile(checkpointer=InMemorySaver())
    logger.info("retrieval.graph_compiled")
    return graph


class Neo4jRetrievalService:
    def __init__(self, get_session: GetSessionFn, get_llm: GetLlmFn, get_embedder: GetEmbedderFn) -> None:
        self._get_session = get_session
        self._get_llm = get_llm
        self._get_embedder = get_embedder

    async def retrieve(self, question: str, top_k: int, project_id: str) -> Dict[str, Any]:
        if not isinstance(question, str) or not question.strip():
            raise HTTPException(status_code=400, detail="query must be a non-empty string")
        logger.info(
                "retrieval.request",
                question_len=len(question),
                top_k=int(top_k or 1),
                project_id=str(project_id),
            )
        graph = await _create_graph(self._get_session, self._get_llm, self._get_embedder)
        # Provide a stable thread_id for the checkpointer
        thread_id = f"{project_id}:{abs(hash(question))}"
        state = await graph.ainvoke(
            {
                "question": question,
                "top_k": int(top_k or 1),
                "project_id": project_id,
                "messages": [{"role": "user", "content": question}],
            },
            {"configurable": {"thread_id": thread_id}},
        )
        result = state.get("result_json") or {}
        logger.info("retrieval.response", has_result=bool(result), keys=len(list(result.keys())))
        return result
