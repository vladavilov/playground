from typing import Any, Dict

from fastapi import APIRouter
from pydantic import BaseModel

from ..services.clients import get_llm, get_embedder, get_neo4j_session
from ..services.retrieval_service import Neo4jRetrievalService


retrieval_router = APIRouter()


class RetrievalRequest(BaseModel):
    query: str
    top_k: int = 1
    project_id: str

@retrieval_router.post("")
async def retrieve(req: RetrievalRequest) -> Dict[str, Any]:
    service = Neo4jRetrievalService(get_session=get_neo4j_session, get_llm=get_llm, get_embedder=get_embedder)
    return await service.retrieve(req.query, top_k=req.top_k, project_id=req.project_id)

