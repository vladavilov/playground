import pytest
import os
import logging
from unittest.mock import patch, MagicMock, Mock
from azure.servicebus import ServiceBusClient as SBusClient, ServiceBusMessage
from azure.identity import DefaultAzureCredential

from service_bus.service_bus_client import ServiceBusClient

# Configure logging for tests
logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)


class TestServiceBusClientFallback:
    """Test suite for Service Bus client fallback mechanism."""
    
    def setup_method(self):
        """Setup for each test method."""
        # Clear environment variables to ensure clean state
        self.original_env = os.environ.copy()
        for key in ['AZURE_SERVICEBUS_NAMESPACE', 'SERVICE_BUS_NAMESPACE', 'USE_LOCAL_FALLBACK', 'SERVICE_BUS_EMULATOR_CONNECTION_STRING']:
            os.environ.pop(key, None)
    
    def teardown_method(self):
        """Cleanup after each test method."""
        # Restore original environment
        os.environ.clear()
        os.environ.update(self.original_env)
    
    @patch('service_bus.service_bus_client.SBusClient')
    @patch('service_bus.service_bus_client.DefaultAzureCredential')
    def test_azure_connection_success(self, mock_credential, mock_sbus_client):
        """Test successful connection to Azure Service Bus."""
        # Setup
        os.environ['SERVICE_BUS_NAMESPACE'] = 'test-namespace'
        
        mock_credential_instance = MagicMock()
        mock_credential.return_value = mock_credential_instance
        
        mock_client_instance = MagicMock()
        mock_sbus_client.return_value = mock_client_instance
        
        # Mock successful sender creation (connection test)
        mock_sender = MagicMock()
        mock_client_instance.get_queue_sender.return_value.__enter__.return_value = mock_sender
        mock_client_instance.get_queue_sender.return_value.__exit__.return_value = None
        
        # Test
        client = ServiceBusClient(queue_name="test-queue")
        
        # Verify
        mock_credential.assert_called_once()
        mock_sbus_client.assert_called_once_with(
            fully_qualified_namespace='test-namespace.servicebus.windows.net',
            credential=mock_credential_instance
        )
        assert client.queue_name == "test-queue"
    
    @patch('service_bus.service_bus_client.SBusClient')
    @patch('service_bus.service_bus_client.DefaultAzureCredential')
    def test_azure_connection_failure_fallback_to_emulator(self, mock_credential, mock_sbus_client):
        """Test fallback to emulator when Azure connection fails."""
        # Setup
        os.environ['SERVICE_BUS_NAMESPACE'] = 'test-namespace'
        os.environ['SERVICE_BUS_EMULATOR_CONNECTION_STRING'] = 'Endpoint=sb://localhost:5672;SharedAccessKeyName=test;SharedAccessKey=test'
        
        mock_credential_instance = MagicMock()
        mock_credential.return_value = mock_credential_instance
        
        # Mock Azure connection failure
        mock_sbus_client.side_effect = Exception("Azure connection failed")
        
        # Mock emulator connection success
        mock_emulator_client = MagicMock()
        mock_sbus_client.from_connection_string.return_value = mock_emulator_client
        
        # Setup emulator client mock
        mock_sender = MagicMock()
        mock_emulator_client.get_queue_sender.return_value.__enter__.return_value = mock_sender
        mock_emulator_client.get_queue_sender.return_value.__exit__.return_value = None
        
        # Test
        client = ServiceBusClient(queue_name="test-queue")
        
        # Verify Azure was tried first, then emulator
        mock_sbus_client.assert_called_once_with(
            fully_qualified_namespace='test-namespace.servicebus.windows.net',
            credential=mock_credential_instance
        )
        
        # Verify fallback to emulator
        mock_sbus_client.from_connection_string.assert_called_once_with(
            'Endpoint=sb://localhost:5672;SharedAccessKeyName=test;SharedAccessKey=test'
        )
    
    @patch('service_bus.service_bus_client.SBusClient')
    def test_forced_local_fallback(self, mock_sbus_client):
        """Test forced local fallback mode."""
        # Setup
        os.environ['USE_LOCAL_FALLBACK'] = 'true'
        os.environ['SERVICE_BUS_NAMESPACE'] = 'test-namespace'
        os.environ['SERVICE_BUS_EMULATOR_CONNECTION_STRING'] = 'Endpoint=sb://localhost:5672;SharedAccessKeyName=test;SharedAccessKey=test'
        
        mock_client_instance = MagicMock()
        mock_sbus_client.from_connection_string.return_value = mock_client_instance
        
        # Mock successful sender creation
        mock_sender = MagicMock()
        mock_client_instance.get_queue_sender.return_value.__enter__.return_value = mock_sender
        mock_client_instance.get_queue_sender.return_value.__exit__.return_value = None
        
        # Test
        client = ServiceBusClient(queue_name="test-queue")
        
        # Verify only emulator was called (Azure skipped due to forced fallback)
        mock_sbus_client.from_connection_string.assert_called_once_with(
            'Endpoint=sb://localhost:5672;SharedAccessKeyName=test;SharedAccessKey=test'
        )
        # Verify Azure constructor was not called
        assert not mock_sbus_client.called
    
    @patch('service_bus.service_bus_client.SBusClient')
    def test_no_azure_namespace_fallback_to_emulator(self, mock_sbus_client):
        """Test fallback when no Azure namespace is configured."""
        # Setup - no Azure namespace
        mock_client_instance = MagicMock()
        mock_sbus_client.from_connection_string.return_value = mock_client_instance
        
        # Mock successful sender creation
        mock_sender = MagicMock()
        mock_client_instance.get_queue_sender.return_value.__enter__.return_value = mock_sender
        mock_client_instance.get_queue_sender.return_value.__exit__.return_value = None
        
        # Test
        client = ServiceBusClient(queue_name="test-queue")
        
        # Verify only emulator was called with default connection string
        mock_sbus_client.from_connection_string.assert_called_once_with(
            "Endpoint=sb://localhost:5672;SharedAccessKeyName=RootManageSharedAccessKey;SharedAccessKey=SAS_KEY_VALUE;UseDevelopmentEmulator=true"
        )
    
    @patch('service_bus.service_bus_client.SBusClient')
    def test_both_azure_and_emulator_fail(self, mock_sbus_client):
        """Test when both Azure and emulator connections fail."""
        # Setup
        os.environ['SERVICE_BUS_NAMESPACE'] = 'test-namespace'
        
        # Mock Azure connection failing
        mock_sbus_client.side_effect = Exception("Connection failed")
        # Mock emulator connection failing
        mock_sbus_client.from_connection_string.side_effect = Exception("Emulator connection failed")
        
        # Test
        with pytest.raises(ValueError, match="Could not connect to either Azure Service Bus or emulator"):
            ServiceBusClient(queue_name="test-queue")
    
    @patch('service_bus.service_bus_client.SBusClient')
    def test_send_message(self, mock_sbus_client):
        """Test sending messages."""
        # Setup client
        mock_client_instance = MagicMock()
        mock_sbus_client.from_connection_string.return_value = mock_client_instance
        
        # Mock sender
        mock_sender = MagicMock()
        mock_client_instance.get_queue_sender.return_value.__enter__.return_value = mock_sender
        mock_client_instance.get_queue_sender.return_value.__exit__.return_value = None
        
        client = ServiceBusClient(queue_name="test-queue")
        
        # Test sending message
        test_message = "Test message content"
        client.send_message(test_message)
        
        # Verify
        mock_client_instance.get_queue_sender.assert_called_with(queue_name="test-queue")
        mock_sender.send_messages.assert_called_once()
        
        # Verify the message was created correctly
        sent_message = mock_sender.send_messages.call_args[0][0]
        assert isinstance(sent_message, ServiceBusMessage)
    
    @patch('service_bus.service_bus_client.SBusClient')
    @patch('service_bus.service_bus_client.ServiceBusMessage')
    def test_send_message_with_proper_message_creation(self, mock_message_class, mock_sbus_client):
        """Test that messages are created properly when sending."""
        # Setup client
        mock_client_instance = MagicMock()
        mock_sbus_client.from_connection_string.return_value = mock_client_instance
        
        # Mock sender
        mock_sender = MagicMock()
        mock_client_instance.get_queue_sender.return_value.__enter__.return_value = mock_sender
        mock_client_instance.get_queue_sender.return_value.__exit__.return_value = None
        
        # Mock message creation
        mock_message_instance = MagicMock()
        mock_message_class.return_value = mock_message_instance
        
        client = ServiceBusClient(queue_name="test-queue")
        
        # Test sending message
        test_message = "Test message content"
        client.send_message(test_message)
        
        # Verify message creation and sending
        mock_message_class.assert_called_once_with(test_message)
        mock_sender.send_messages.assert_called_once_with(mock_message_instance)
    
    @patch('service_bus.service_bus_client.SBusClient')
    def test_receive_messages(self, mock_sbus_client):
        """Test receiving messages."""
        # Setup client
        mock_client_instance = MagicMock()
        mock_sbus_client.from_connection_string.return_value = mock_client_instance
        
        # Mock receiver
        mock_receiver = MagicMock()
        mock_client_instance.get_queue_receiver.return_value.__enter__.return_value = mock_receiver
        mock_client_instance.get_queue_receiver.return_value.__exit__.return_value = None
        
        # Mock messages
        mock_message1 = MagicMock()
        mock_message1.body = b"Test message 1"
        mock_message2 = MagicMock()
        mock_message2.body = b"Test message 2"
        mock_receiver.receive_messages.return_value = [mock_message1, mock_message2]
        
        client = ServiceBusClient(queue_name="test-queue")
        
        # Test receiving messages
        received_messages = []
        def test_callback(message_body):
            received_messages.append(message_body)
        
        client.receive_messages(test_callback, max_messages=5, timeout=10)
        
        # Verify
        mock_client_instance.get_queue_receiver.assert_called_with(queue_name="test-queue", max_wait_time=10)
        mock_receiver.receive_messages.assert_called_once_with(max_message_count=5, max_wait_time=10)
        
        # Verify messages were processed
        assert len(received_messages) == 2
        assert received_messages[0] == b"Test message 1"
        assert received_messages[1] == b"Test message 2"
        
        # Verify messages were completed
        mock_receiver.complete_message.assert_any_call(mock_message1)
        mock_receiver.complete_message.assert_any_call(mock_message2)
    
    @patch('service_bus.service_bus_client.SBusClient')
    def test_receive_messages_with_callback_error(self, mock_sbus_client):
        """Test receiving messages when callback raises an error."""
        # Setup client
        mock_client_instance = MagicMock()
        mock_sbus_client.from_connection_string.return_value = mock_client_instance
        
        # Mock receiver
        mock_receiver = MagicMock()
        mock_client_instance.get_queue_receiver.return_value.__enter__.return_value = mock_receiver
        mock_client_instance.get_queue_receiver.return_value.__exit__.return_value = None
        
        # Mock message
        mock_message = MagicMock()
        mock_message.body = b"Test message"
        mock_receiver.receive_messages.return_value = [mock_message]
        
        client = ServiceBusClient(queue_name="test-queue")
        
        # Test receiving messages with failing callback
        def failing_callback(message_body):
            raise Exception("Callback failed")
        
        client.receive_messages(failing_callback)
        
        # Verify message was abandoned (not completed)
        mock_receiver.abandon_message.assert_called_once_with(mock_message)
        assert not mock_receiver.complete_message.called
    
    @patch('service_bus.service_bus_client.SBusClient')
    def test_namespace_formatting(self, mock_sbus_client):
        """Test that namespace formatting works correctly."""
        # Setup with namespace that needs .servicebus.windows.net appended
        os.environ['SERVICE_BUS_NAMESPACE'] = 'test-namespace'
        
        mock_client_instance = MagicMock()
        mock_sbus_client.return_value = mock_client_instance
        
        # Mock successful sender creation
        mock_sender = MagicMock()
        mock_client_instance.get_queue_sender.return_value.__enter__.return_value = mock_sender
        mock_client_instance.get_queue_sender.return_value.__exit__.return_value = None
        
        # Test
        ServiceBusClient(queue_name="test-queue")
        
        # Verify namespace was formatted correctly
        call_args = mock_sbus_client.call_args
        assert call_args[1]['fully_qualified_namespace'] == 'test-namespace.servicebus.windows.net'
    
    @patch('service_bus.service_bus_client.SBusClient')
    def test_namespace_already_formatted(self, mock_sbus_client):
        """Test that already formatted namespace is not double-formatted."""
        # Setup with namespace that already has .servicebus.windows.net
        os.environ['SERVICE_BUS_NAMESPACE'] = 'test-namespace.servicebus.windows.net'
        
        mock_client_instance = MagicMock()
        mock_sbus_client.return_value = mock_client_instance
        
        # Mock successful sender creation
        mock_sender = MagicMock()
        mock_client_instance.get_queue_sender.return_value.__enter__.return_value = mock_sender
        mock_client_instance.get_queue_sender.return_value.__exit__.return_value = None
        
        # Test
        ServiceBusClient(queue_name="test-queue")
        
        # Verify namespace was not double-formatted
        call_args = mock_sbus_client.call_args
        assert call_args[1]['fully_qualified_namespace'] == 'test-namespace.servicebus.windows.net' 