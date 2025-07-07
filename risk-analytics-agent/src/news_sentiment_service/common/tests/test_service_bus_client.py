import pytest
import os
from unittest.mock import patch, MagicMock

from service_bus.service_bus_client import ServiceBusClient

FULLY_QUALIFIED_NAMESPACE = "test.servicebus.windows.net"
QUEUE_NAME = "test-queue"

@pytest.fixture
def mock_sbus_client():
    """Fixture to mock the underlying Azure SBusClient."""
    with patch('service_bus.service_bus_client.DefaultAzureCredential') as mock_credential, \
         patch('service_bus.service_bus_client.SBusClient') as mock_sbus_client_class:
        
        mock_sbus_instance = MagicMock()
        mock_sender = MagicMock()
        mock_receiver = MagicMock()

        mock_sbus_client_class.return_value = mock_sbus_instance
        mock_sbus_instance.get_queue_sender.return_value = mock_sender
        mock_sbus_instance.get_queue_receiver.return_value = mock_receiver
        
        mock_sender.__enter__.return_value = mock_sender
        mock_sender.__exit__.return_value = None
        mock_receiver.__enter__.return_value = mock_receiver
        mock_receiver.__exit__.return_value = None

        yield mock_sbus_client_class, mock_sbus_instance, mock_sender, mock_receiver, mock_credential

def test_servicebus_client_initialization(mock_sbus_client):
    """Tests that the ServiceBusClient initializes correctly."""
    mock_sbus_client_class, _, _, _, mock_credential = mock_sbus_client
    
    os.environ['SERVICE_BUS_NAMESPACE'] = 'test'
    
    try:
        client = ServiceBusClient(queue_name=QUEUE_NAME)
        
        mock_credential.assert_called_once()
        mock_sbus_client_class.assert_called_once_with(
            fully_qualified_namespace='test.servicebus.windows.net',
            credential=mock_credential.return_value
        )
        assert client.queue_name == QUEUE_NAME
    finally:
        os.environ.pop('SERVICE_BUS_NAMESPACE', None)

def test_send_message_calls_sdk(mock_sbus_client):
    """Tests that the send_message method calls the underlying SDK's send_messages."""
    mock_sbus_client_class, mock_sbus_instance, mock_sender, _, _ = mock_sbus_client

    os.environ.pop('SERVICE_BUS_NAMESPACE', None)
    
    mock_sbus_client_class.from_connection_string.return_value = mock_sbus_instance
    
    client = ServiceBusClient(queue_name=QUEUE_NAME)
    
    test_message = "Hello, Service Bus!"
    client.send_message(test_message)
    
    mock_sbus_instance.get_queue_sender.assert_called_with(queue_name=QUEUE_NAME)
    mock_sender.send_messages.assert_called_once()

def test_receive_messages_calls_sdk_and_callback(mock_sbus_client):
    """Tests that the receive_messages method calls the SDK and processes messages."""
    mock_sbus_client_class, mock_sbus_instance, _, mock_receiver, _ = mock_sbus_client
    
    mock_message = MagicMock()
    mock_message.body = b'{"key": "value"}'
    mock_receiver.receive_messages.return_value = [mock_message]

    callback_func = MagicMock()

    os.environ.pop('SERVICE_BUS_NAMESPACE', None)
    
    mock_sbus_client_class.from_connection_string.return_value = mock_sbus_instance
    
    client = ServiceBusClient(queue_name=QUEUE_NAME)
    client.receive_messages(callback_func)

    mock_sbus_instance.get_queue_receiver.assert_called_with(queue_name=QUEUE_NAME, max_wait_time=30)
    mock_receiver.receive_messages.assert_called_once_with(max_message_count=10, max_wait_time=30)
    callback_func.assert_called_once_with(b'{"key": "value"}')
    mock_receiver.complete_message.assert_called_once_with(mock_message) 