from __future__ import annotations

from typing import Any, Dict, List, Optional

from pydantic import BaseModel, ConfigDict, Field, field_validator, model_validator


def _coerce_list(value: Any) -> list:
    if value is None:
        return []
    if isinstance(value, list):
        return value
    if isinstance(value, tuple):
        return list(value)
    if isinstance(value, set):
        return list(value)
    if hasattr(value, "tolist"):
        try:
            return list(value.tolist())
        except Exception:
            pass
    return [value]


def _coerce_str_list(value: Any) -> list[str]:
    items = _coerce_list(value)
    out: list[str] = []
    for v in items:
        if v is None:
            continue
        s = str(v).strip()
        if s:
            out.append(s)
    return out


def _coerce_float_list(value: Any) -> list[float]:
    items = _coerce_list(value)
    out: list[float] = []
    for v in items:
        if v is None:
            continue
        try:
            out.append(float(v))
        except Exception:
            continue
    return out


def _coerce_int(value: Any) -> int:
    if value is None:
        raise ValueError("value is required")
    if isinstance(value, bool):
        return int(value)
    if isinstance(value, int):
        return value
    if isinstance(value, float):
        return int(value)
    if hasattr(value, "item"):
        try:
            return _coerce_int(value.item())
        except Exception:
            pass
    return int(str(value).strip())


class _RowBase(BaseModel):
    model_config = ConfigDict(extra="ignore")


class DocumentRow(_RowBase):
    id: str
    title: Optional[str] = None
    # Neo4j properties cannot be maps; keep metadata for validation/derivation only,
    # but never serialize it into the outgoing DTO payload.
    metadata: Dict[str, Any] = Field(default_factory=dict, exclude=True)

    @field_validator("id", mode="before")
    @classmethod
    def _id(cls, v: Any) -> str:
        s = str(v).strip()
        if not s:
            raise ValueError("id must be non-empty")
        return s

    @field_validator("metadata", mode="before")
    @classmethod
    def _metadata(cls, v: Any) -> Dict[str, Any]:
        return v if isinstance(v, dict) else {}

    @model_validator(mode="after")
    def _derive_title_from_metadata(self) -> "DocumentRow":
        # Preserve previous behavior where title could be inferred from metadata.file_name
        # (repository-side cypher used value.metadata.file_name before we excluded metadata).
        if self.title is None or str(self.title).strip() == "":
            fn = self.metadata.get("file_name")
            if isinstance(fn, str) and fn.strip():
                self.title = fn.strip()
        return self


class ChunkRow(_RowBase):
    id: str
    text: Optional[str] = None
    document_ids: List[str] = Field(default_factory=list)

    @field_validator("id", mode="before")
    @classmethod
    def _id(cls, v: Any) -> str:
        s = str(v).strip()
        if not s:
            raise ValueError("id must be non-empty")
        return s

    @field_validator("document_ids", mode="before")
    @classmethod
    def _document_ids(cls, v: Any) -> List[str]:
        return _coerce_str_list(v)


class EntityRow(_RowBase):
    id: str
    title: Optional[str] = None
    norm_title: Optional[str] = None
    description: Optional[str] = None
    text_unit_ids: List[str] = Field(default_factory=list)

    @field_validator("id", mode="before")
    @classmethod
    def _id(cls, v: Any) -> str:
        s = str(v).strip()
        if not s:
            raise ValueError("id must be non-empty")
        return s

    @field_validator("text_unit_ids", mode="before")
    @classmethod
    def _text_unit_ids(cls, v: Any) -> List[str]:
        return _coerce_str_list(v)


class RelationshipRow(_RowBase):
    id: Optional[str] = None
    source: str
    target: str
    description: Optional[str] = None
    weight: Optional[float] = None
    combined_degree: Optional[float] = None
    text_unit_ids: List[str] = Field(default_factory=list)

    @field_validator("source", "target", mode="before")
    @classmethod
    def _non_empty_str(cls, v: Any) -> str:
        s = str(v).strip()
        if not s:
            raise ValueError("must be non-empty")
        return s

    @field_validator("text_unit_ids", mode="before")
    @classmethod
    def _text_unit_ids(cls, v: Any) -> List[str]:
        return _coerce_str_list(v)


class CommunityReportRow(_RowBase):
    community: int
    level: Optional[int] = None
    title: Optional[str] = None
    rank: Optional[int] = None
    rating_explanation: Optional[str] = None
    full_content: Optional[str] = None
    summary: Optional[str] = None
    full_content_json: Any = None

    @field_validator("community", mode="before")
    @classmethod
    def _community(cls, v: Any) -> int:
        return _coerce_int(v)

    @field_validator("level", "rank", mode="before")
    @classmethod
    def _optional_int(cls, v: Any) -> Optional[int]:
        if v is None:
            return None
        try:
            return _coerce_int(v)
        except Exception:
            return None


class CommunityRow(_RowBase):
    community: int
    level: Optional[int] = None
    entity_ids: List[str] = Field(default_factory=list)
    text_unit_ids: List[str] = Field(default_factory=list)

    @field_validator("community", mode="before")
    @classmethod
    def _community(cls, v: Any) -> int:
        return _coerce_int(v)

    @field_validator("entity_ids", "text_unit_ids", mode="before")
    @classmethod
    def _ids(cls, v: Any) -> List[str]:
        return _coerce_str_list(v)


class RequirementsGraphBundleRequest(BaseModel):
    model_config = ConfigDict(extra="ignore")

    project_id: str
    documents: List[DocumentRow] = Field(default_factory=list)
    chunks: List[ChunkRow] = Field(default_factory=list)
    entities: List[EntityRow] = Field(default_factory=list)
    relationships: List[RelationshipRow] = Field(default_factory=list)
    community_reports: List[CommunityReportRow] = Field(default_factory=list)
    communities: List[CommunityRow] = Field(default_factory=list)

    @field_validator("project_id", mode="before")
    @classmethod
    def _project_id(cls, v: Any) -> str:
        s = str(v).strip()
        if not s:
            raise ValueError("project_id must be non-empty")
        return s


class ChunkEmbeddingRow(_RowBase):
    id: str
    embedding: List[float]

    @field_validator("id", mode="before")
    @classmethod
    def _id(cls, v: Any) -> str:
        s = str(v).strip()
        if not s:
            raise ValueError("id must be non-empty")
        return s

    @field_validator("embedding", mode="before")
    @classmethod
    def _embedding(cls, v: Any) -> List[float]:
        return _coerce_float_list(v)


class EntityEmbeddingRow(_RowBase):
    id: Optional[str] = None
    norm_title: Optional[str] = None
    text: Optional[str] = None
    embedding: List[float]

    @field_validator("embedding", mode="before")
    @classmethod
    def _embedding(cls, v: Any) -> List[float]:
        return _coerce_float_list(v)


class CommunityEmbeddingRow(_RowBase):
    community: Optional[int] = None
    id: Optional[str] = None
    text: Optional[str] = None
    embedding: List[float]

    @field_validator("community", mode="before")
    @classmethod
    def _community(cls, v: Any) -> Optional[int]:
        if v is None:
            return None
        try:
            return _coerce_int(v)
        except Exception:
            return None

    @field_validator("embedding", mode="before")
    @classmethod
    def _embedding(cls, v: Any) -> List[float]:
        return _coerce_float_list(v)


