"""
Unified Redis message classes that replace duplicated message types.
"""

from dataclasses import dataclass
from typing import Dict, Any, Optional
from uuid import UUID, uuid4
from datetime import datetime
import structlog

from utils.redis_abstractions import (
    RedisMessage,
    MessageTypeRegistry
)

logger = structlog.get_logger(__name__)


@dataclass
class TaskRequestMessage(RedisMessage):
    """Unified task request message for cross-service communication."""
    task_type: str
    project_id: UUID
    correlation_id: UUID
    parameters: Dict[str, Any]
    message_type: str = "task_request"

    def __init__(
        self,
        task_type: str,
        project_id: UUID,
        correlation_id: UUID,
        parameters: Dict[str, Any],
        timestamp: Optional[datetime] = None,
        message_id: Optional[UUID] = None,
        message_type: str = "task_request"
    ):
        super().__init__(
            message_id=message_id or uuid4(),
            timestamp=timestamp or datetime.now()
        )
        self.task_type = task_type
        self.project_id = project_id
        self.correlation_id = correlation_id
        self.parameters = parameters
        self.message_type = message_type

    def to_dict(self) -> Dict[str, Any]:
        data = super().to_dict()
        data.update({
            "message_type": self.message_type,
            "task_type": self.task_type,
            "project_id": str(self.project_id),
            "correlation_id": str(self.correlation_id),
            "parameters": self.parameters
        })
        return data

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'TaskRequestMessage':
        try:
            for field in ["message_id", "project_id", "correlation_id"]:
                if field in data:
                    data[field] = UUID(data[field])
            if "timestamp" in data:
                data["timestamp"] = datetime.fromisoformat(data["timestamp"])
            return cls(**data)
        except (KeyError, ValueError, TypeError) as e:
            raise ValueError(f"Invalid TaskRequestMessage format: {e}")


@dataclass
class ProjectProgressMessage(RedisMessage):
    """Unified project progress message for real-time updates."""
    project_id: UUID
    status: str
    processed_count: Optional[int] = None
    total_count: Optional[int] = None
    processed_pct: Optional[float] = None
    message_type: str = "project_progress"

    def __init__(
        self,
        project_id: UUID,
        status: str,
        processed_count: Optional[int] = None,
        total_count: Optional[int] = None,
        processed_pct: Optional[float] = None,
        timestamp: Optional[datetime] = None,
        message_id: Optional[UUID] = None,
        message_type: str = "project_progress"
    ):
        super().__init__(
            message_id=message_id or uuid4(),
            timestamp=timestamp or datetime.now()
        )
        self.project_id = project_id
        self.status = status
        self.processed_count = processed_count
        self.total_count = total_count
        self.processed_pct = processed_pct
        self.message_type = message_type

    def to_dict(self) -> Dict[str, Any]:
        data = super().to_dict()
        data.update({
            "message_type": self.message_type,
            "project_id": str(self.project_id),
            "status": self.status,
            "processed_count": self.processed_count,
            "total_count": self.total_count,
            "processed_pct": self.processed_pct
        })
        return data

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

def create_message_registry() -> MessageTypeRegistry:
    """Create message type registry for unified Redis messages."""
    registry = MessageTypeRegistry()
    registry.register_message_type("task_request", TaskRequestMessage)
    registry.register_message_type("project_progress", ProjectProgressMessage)
    return registry
