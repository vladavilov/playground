from typing import Any, Dict

from fastapi import APIRouter
from pydantic import BaseModel

from ..services.clients import get_oai_client as _clients_oai, get_neo4j_session as _clients_neo4j
from ..services.retrieval_service import Neo4jRetrievalService


retrieval_router = APIRouter()


class RetrievalRequest(BaseModel):
    query: str
    top_k: int = 1

@retrieval_router.post("")
async def retrieve(req: RetrievalRequest) -> Dict[str, Any]:
    service = Neo4jRetrievalService(get_session=_clients_neo4j, get_oai=_clients_oai)
    return await service.retrieve(req.query, top_k=req.top_k)

