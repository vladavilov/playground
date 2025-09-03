from typing import Any, Dict, List

from pydantic import BaseModel, Field


class RetrievalPlan(BaseModel):
    query: str
    intents: List[str] = Field(default_factory=list)
    options: Dict[str, Any] = Field(default_factory=dict)


class ContextPack(BaseModel):
    citations: List[str] = Field(default_factory=list)
    snippets: List[str] = Field(default_factory=list)
    provenance: List[Dict[str, Any]] = Field(default_factory=list)

    @classmethod
    def from_upstream(cls, data: Dict[str, Any]) -> "ContextPack":
        citations_raw = data.get("citations") or []
        snippets_raw = data.get("snippets") or []
        prov_raw = data.get("provenance") or []

        citations = [c for c in citations_raw if isinstance(c, str)]
        snippets = [s for s in snippets_raw if isinstance(s, str)]
        provenance = [p for p in prov_raw if isinstance(p, dict)]
        return cls(citations=citations, snippets=snippets, provenance=provenance)


