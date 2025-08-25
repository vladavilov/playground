import pytest
from unittest.mock import AsyncMock

from utils.app_factory import RedisHealthService


@pytest.mark.asyncio
async def test_redis_health_service_success():
    client = AsyncMock()
    client.ping.return_value = True
    client.info.return_value = {
        "redis_version": "6.2",
        "connected_clients": 5,
        "used_memory": 1024000,
        "uptime_in_seconds": 3600,
    }

    service = RedisHealthService(client)
    result = await service.check_health_with_details()

    assert result["healthy"] is True
    assert result["version"] == "6.2"
    # Optional fields should exist when info is available
    assert result["connected_clients"] == 5
    assert result["used_memory"] == 1024000
    assert result["uptime_in_seconds"] == 3600


@pytest.mark.asyncio
async def test_redis_health_service_ping_false():
    client = AsyncMock()
    client.ping.return_value = False

    service = RedisHealthService(client)
    result = await service.check_health_with_details()

    assert result["healthy"] is False
    assert "error" in result


@pytest.mark.asyncio
async def test_redis_health_service_ping_exception():
    client = AsyncMock()
    client.ping.side_effect = RuntimeError("connection error")

    service = RedisHealthService(client)
    result = await service.check_health_with_details()

    assert result["healthy"] is False
    assert "connection error" in result["error"]
