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
    # Deterministic Smallpdf-grounded citations/snippets for upstream workflow
    citations: List[str] = [
        "Welcome to Smallpdf",
        "Digital Documents—All In One Place",
        "Access Files Anytime, Anywhere",
        "Enhance Documents in One Click",
        "Collaborate With Others",
        "When you enable the ‘Storage’ option, we’ll also store all processed files here.",
        "We’ll also sync files from the Smallpdf Mobile App to our online portal",
        "When you right-click on a file, we’ll present you with options to convert, compress, or modify it.",
        "request e-signatures, send large files, enable the Smallpdf G Suite App",
    ]

    snippets: List[str] = [
        "Welcome to Smallpdf\nDigital Documents—All In One Place",
        "Access files stored on Smallpdf from your computer, phone, or tablet.",
        "Enhance Documents in One Click",
        "When you enable the ‘Storage’ option, we’ll also store all processed files here.",
        "We’ll also sync files from the Smallpdf Mobile App to our online portal",
        "When you right-click on a file, we’ll present options to convert, compress, or modify it.",
        "With Smallpdf, you can request e-signatures, send large files, or enable the Smallpdf G Suite App.",
    ]

    provenance: List[Dict[str, Any]] = [{"source": "mock", "file": "smallpdf_intro.txt", "score": 1.0}]

    return ContextPack(citations=citations, snippets=snippets, provenance=provenance)

