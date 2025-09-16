from typing import Any, Dict

import httpx
from fastapi import APIRouter
from pydantic import BaseModel

from ..services.clients import get_oai_client as _clients_oai, get_neo4j_session as _clients_neo4j
from ..services.retrieval_service import Neo4jRetrievalService


retrieval_router = APIRouter()


class RetrievalRequest(BaseModel):
    query: str
    top_k: int = 1


def _oai_client() -> httpx.Client:  # kept for tests monkeypatching
    return _clients_oai()


def _neo4j_session():  # kept for tests monkeypatching
    return _clients_neo4j()


@retrieval_router.post("")
async def retrieve(req: RetrievalRequest) -> Dict[str, Any]:
    service = Neo4jRetrievalService(get_session=_neo4j_session, get_oai=_oai_client)
    return await service.retrieve(req.query, top_k=req.top_k)

