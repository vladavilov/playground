import sys
from pathlib import Path

import pytest

root = Path(__file__).resolve().parents[4]
service_src = root / "risk-analytics-agent" / "src"
if str(service_src) not in sys.path:
    sys.path.insert(0, str(service_src))

from news_sentiment_service.src.common.service_bus import InMemoryServiceBusClient  # type: ignore


def test_send_and_receive_single_message():
    client = InMemoryServiceBusClient()
    client.send_message("hello")
    msgs = client.receive_messages()
    assert msgs == ["hello"]
    # Queue now empty
    assert client.receive_messages() == []


def test_fifo_order():
    client = InMemoryServiceBusClient()
    for i in range(3):
        client.send_message(i)
    msgs = client.receive_messages(max_messages=3)
    assert msgs == [0, 1, 2]


@pytest.mark.parametrize("max_messages", [0, -1])
def test_invalid_max_messages(max_messages):
    client = InMemoryServiceBusClient()
    with pytest.raises(ValueError):
        client.receive_messages(max_messages=max_messages)
