import pytest
import os
from unittest.mock import patch, MagicMock, Mock
from azure.servicebus import ServiceBusMessage

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

class TestServiceBusClient:
    """Test suite for ServiceBusClient."""
    
    def test_init_with_queue_name(self):
        """Test initialization with queue name."""
        with patch.object(ServiceBusClient, '_initialize_client') as mock_init:
            mock_init.return_value = Mock()
            client = ServiceBusClient(queue_name="test-queue")
            assert client.queue_name == "test-queue"
            mock_init.assert_called_once()
    
    def test_send_message_success(self):
        """Test successful message sending."""
        with patch.object(ServiceBusClient, '_initialize_client') as mock_init:
            mock_client = Mock()
            mock_sender = Mock()
            mock_client.get_queue_sender.return_value.__enter__.return_value = mock_sender
            mock_init.return_value = mock_client
            
            client = ServiceBusClient(queue_name="test-queue")
            client.send_message("test message")
            
            mock_sender.send_messages.assert_called_once()
            # Verify that a ServiceBusMessage was created
            call_args = mock_sender.send_messages.call_args[0][0]
            assert isinstance(call_args, ServiceBusMessage)
    
    def test_send_message_error(self):
        """Test error handling during message sending."""
        with patch.object(ServiceBusClient, '_initialize_client') as mock_init:
            mock_client = Mock()
            mock_sender = Mock()
            mock_sender.send_messages.side_effect = Exception("Send failed")
            mock_client.get_queue_sender.return_value.__enter__.return_value = mock_sender
            mock_init.return_value = mock_client
            
            client = ServiceBusClient(queue_name="test-queue")
            
            with pytest.raises(Exception, match="Send failed"):
                client.send_message("test message")
    
    def test_receive_messages_success(self):
        """Test successful message receiving."""
        with patch.object(ServiceBusClient, '_initialize_client') as mock_init:
            mock_client = Mock()
            mock_receiver = Mock()
            
            # Create mock message with generator body (as in Azure SDK)
            mock_message = Mock()
            mock_message.body = (b for b in [b"test message"])  # Generator that yields bytes
            
            mock_receiver.receive_messages.return_value = [mock_message]
            mock_context_manager = MagicMock()
            mock_context_manager.__enter__.return_value = mock_receiver
            mock_context_manager.__exit__.return_value = None
            mock_client.get_queue_receiver.return_value = mock_context_manager
            mock_init.return_value = mock_client
            
            client = ServiceBusClient(queue_name="test-queue")
            
            # Mock callback function
            callback = Mock()
            client.receive_messages(callback, max_messages=1, timeout=10)
            
            # Verify callback was called with message body
            callback.assert_called_once()
            # The callback should receive the actual message body, not the generator
            mock_receiver.complete_message.assert_called_once_with(mock_message)
    
    def test_receive_messages_with_generator_body(self):
        """Test receiving messages when body is a generator (common in Azure SDK)."""
        with patch.object(ServiceBusClient, '_initialize_client') as mock_init:
            mock_client = Mock()
            mock_receiver = Mock()
            
            # Create mock message with generator body (as in Azure SDK)
            mock_message = Mock()
            test_data = b"test message content"
            mock_message.body = (chunk for chunk in [test_data])  # Generator that yields bytes
            
            mock_receiver.receive_messages.return_value = [mock_message]
            mock_context_manager = MagicMock()
            mock_context_manager.__enter__.return_value = mock_receiver
            mock_context_manager.__exit__.return_value = None
            mock_client.get_queue_receiver.return_value = mock_context_manager
            mock_init.return_value = mock_client
            
            client = ServiceBusClient(queue_name="test-queue")
            
            # Mock callback function
            callback = Mock()
            client.receive_messages(callback, max_messages=1, timeout=10)
            
            # Verify callback was called with bytes (not generator)
            callback.assert_called_once()
            # The fix should have converted the generator to bytes
            call_args = callback.call_args[0][0]
            assert isinstance(call_args, bytes), f"Expected bytes, got {type(call_args)}"
            assert call_args == test_data, f"Expected {test_data}, got {call_args}"
            mock_receiver.complete_message.assert_called_once_with(mock_message)
    
    def test_receive_messages_error(self):
        """Test error handling during message receiving."""
        with patch.object(ServiceBusClient, '_initialize_client') as mock_init:
            mock_client = Mock()
            mock_receiver = Mock()
            mock_receiver.receive_messages.side_effect = Exception("Receive failed")
            mock_client.get_queue_receiver.return_value.__enter__.return_value = mock_receiver
            mock_init.return_value = mock_client
            
            client = ServiceBusClient(queue_name="test-queue")
            
            with pytest.raises(Exception, match="Receive failed"):
                client.receive_messages(Mock(), max_messages=1, timeout=10)
    
    @patch.dict('os.environ', {'USE_LOCAL_FALLBACK': 'false', 'AZURE_SERVICEBUS_NAMESPACE': 'test-namespace'})
    def test_initialize_client_azure_success(self):
        """Test successful Azure Service Bus initialization."""
        with patch('service_bus.service_bus_client.SBusClient') as mock_sbus:
            with patch('service_bus.service_bus_client.DefaultAzureCredential') as mock_cred:
                mock_client = Mock()
                mock_sbus.return_value = mock_client
                mock_client.get_queue_sender.return_value.__enter__.return_value = Mock()
                
                client = ServiceBusClient(queue_name="test-queue")
                
                mock_sbus.assert_called_once()
                mock_cred.assert_called_once()
    
    @patch.dict('os.environ', {'USE_LOCAL_FALLBACK': 'true', 'SERVICE_BUS_EMULATOR_CONNECTION_STRING': 'test-connection'})
    def test_initialize_client_emulator_success(self):
        """Test successful emulator initialization."""
        with patch('service_bus.service_bus_client.SBusClient') as mock_sbus:
            mock_client = Mock()
            mock_sbus.from_connection_string.return_value = mock_client
            mock_client.get_queue_sender.return_value.__enter__.return_value = Mock()
            
            client = ServiceBusClient(queue_name="test-queue")
            
            mock_sbus.from_connection_string.assert_called_once_with('test-connection')
    
    def test_receive_messages_callback_error(self):
        """Test error handling when callback function fails."""
        with patch.object(ServiceBusClient, '_initialize_client') as mock_init:
            mock_client = Mock()
            mock_receiver = Mock()
            
            # Create mock message
            mock_message = Mock()
            mock_message.body = b"test message"
            
            mock_receiver.receive_messages.return_value = [mock_message]
            mock_client.get_queue_receiver.return_value.__enter__.return_value = mock_receiver
            mock_init.return_value = mock_client
            
            client = ServiceBusClient(queue_name="test-queue")
            
            # Mock callback function that raises an exception
            callback = Mock(side_effect=Exception("Callback failed"))
            
            # Should not raise exception, but should abandon the message
            client.receive_messages(callback, max_messages=1, timeout=10)
            
            callback.assert_called_once()
            mock_receiver.abandon_message.assert_called_once_with(mock_message) 