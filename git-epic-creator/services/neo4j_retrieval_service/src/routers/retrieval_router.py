from typing import Any, Dict, List

from fastapi import APIRouter
from models.retrieval import RetrievalPlan, ContextPack


retrieval_router = APIRouter()


@retrieval_router.post("", response_model=ContextPack)
async def retrieve(plan: RetrievalPlan) -> ContextPack:
    """
    Mock retrieval endpoint to satisfy GraphRAG client contract.

    TODO: Replace mock logic with real retrieval pipeline integrating Neo4j and RAG.
    TODO: Validate and support advanced `options` such as top_k, filters, and ranking strategy.
    TODO: Populate `snippets` and `provenance` with real source metadata and scores.
    """
    citations: List[str] = []
    if plan.query:
        citations.append(f"mock:{plan.query}")
    for intent in plan.intents[:2]:
        citations.append(f"mock:intent:{intent}")

    provenance: List[Dict[str, Any]] = [{"source": "mock", "score": 1.0}]

    return ContextPack(citations=citations, snippets=[], provenance=provenance)

