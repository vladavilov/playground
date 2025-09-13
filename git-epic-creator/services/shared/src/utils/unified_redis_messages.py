"""
Unified Redis message classes that replace duplicated message types.
"""

from dataclasses import dataclass, field
from typing import Dict, Any, Optional
from uuid import UUID, uuid4
from datetime import datetime
import structlog

logger = structlog.get_logger(__name__)

@dataclass
class ProjectProgressMessage():
    """Project progress message for real-time updates."""
    project_id: UUID
    status: str
    process_step: Optional[str] = None
    processed_count: Optional[int] = None
    total_count: Optional[int] = None
    processed_pct: Optional[float] = None
    message_type: str = "project_progress"
    message_id: UUID = field(default_factory=uuid4)
    timestamp: datetime = field(default_factory=datetime.now)

    def __init__(
        self,
        project_id: UUID,
        status: str,
        process_step: Optional[str] = None,
        processed_count: Optional[int] = None,
        total_count: Optional[int] = None,
        processed_pct: Optional[float] = None,
        timestamp: Optional[datetime] = None,
        message_id: Optional[UUID] = None,
        message_type: str = "project_progress"
    ):
        # Set identifiers and timestamps locally; there is no superclass to initialize.
        self.message_id = message_id or uuid4()
        self.timestamp = timestamp or datetime.now()
        self.project_id = project_id
        self.status = status
        self.process_step = process_step
        self.processed_count = processed_count
        self.total_count = total_count
        self.processed_pct = processed_pct
        self.message_type = message_type

    def to_dict(self) -> Dict[str, Any]:
        return {
            "message_id": str(self.message_id),
            "timestamp": self.timestamp.isoformat(),
            "message_type": self.message_type,
            "project_id": str(self.project_id),
            "status": self.status,
            "process_step": self.process_step,
            "processed_count": self.processed_count,
            "total_count": self.total_count,
            "processed_pct": self.processed_pct,
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'ProjectProgressMessage':
        try:
            for field in ["message_id", "project_id"]:
                if field in data:
                    data[field] = UUID(data[field])
            if "timestamp" in data:
                data["timestamp"] = datetime.fromisoformat(data["timestamp"])
            return cls(**data)
        except (KeyError, ValueError, TypeError) as e:
            raise ValueError(f"Invalid ProjectProgressMessage format: {e}")
