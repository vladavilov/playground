import os
import logging
from typing import Callable
from azure.servicebus import ServiceBusClient as SBusClient, ServiceBusMessage
from azure.identity import DefaultAzureCredential

logger = logging.getLogger(__name__)

class ServiceBusClient:
    """
    A client for interacting with Azure Service Bus or Service Bus Emulator.

    This client uses DefaultAzureCredential for Azure authentication, and falls back
    to the Service Bus emulator when Azure services are not available.
    """
    def __init__(self, namespace: str = None, queue_name: str = None):
        """
        Initializes the ServiceBusClient.

        Args:
            namespace (str, optional): The Service Bus namespace (for backward compatibility).
            queue_name (str): The name of the Service Bus queue.
        """
        self.queue_name = queue_name
        self.sbus_client = self._initialize_client()

    def _initialize_client(self) -> SBusClient:
        """
        Initialize the Service Bus client, trying Azure first, then emulator fallback.
        
        Returns:
            SBusClient: Initialized Service Bus client
        """
        use_local_fallback = os.environ.get("USE_LOCAL_FALLBACK", "false").lower() == "true"
        
        if not use_local_fallback:
            azure_namespace = os.environ.get("AZURE_SERVICEBUS_NAMESPACE") or os.environ.get("SERVICE_BUS_NAMESPACE")
            if azure_namespace:
                try:
                    logger.info("Attempting to connect to Azure Service Bus...")
                    
                    if not azure_namespace.endswith('.servicebus.windows.net'):
                        fully_qualified_namespace = f"{azure_namespace}.servicebus.windows.net"
                    else:
                        fully_qualified_namespace = azure_namespace
                    
                    credential = DefaultAzureCredential()
                    client = SBusClient(
                        fully_qualified_namespace=fully_qualified_namespace,
                        credential=credential
                    )
                    
                    with client.get_queue_sender(queue_name=self.queue_name) as sender:
                        pass
                    
                    logger.info(f"Successfully connected to Azure Service Bus: {fully_qualified_namespace}")
                    return client
                    
                except Exception as e:
                    logger.warning(f"Failed to connect to Azure Service Bus: {e}")
        
        logger.info("Falling back to Service Bus emulator...")
        emulator_connection_string = os.environ.get("SERVICE_BUS_EMULATOR_CONNECTION_STRING")
        
        if not emulator_connection_string:
            emulator_connection_string = "Endpoint=sb://localhost:5672;SharedAccessKeyName=RootManageSharedAccessKey;SharedAccessKey=SAS_KEY_VALUE;UseDevelopmentEmulator=true"
        
        try:
            client = SBusClient.from_connection_string(emulator_connection_string)
            
            with client.get_queue_sender(queue_name=self.queue_name) as sender:
                pass
            
            logger.info(f"Successfully connected to Service Bus emulator")
            return client
            
        except Exception as e:
            logger.error(f"Failed to connect to Service Bus emulator: {e}")
            raise ValueError(f"Could not connect to either Azure Service Bus or emulator. Last error: {e}")

    def send_message(self, message: str) -> None:
        """
        Sends a message to the specified queue.

        Args:
            message (str): The message to send.
        """
        try:
            with self.sbus_client.get_queue_sender(queue_name=self.queue_name) as sender:
                service_bus_message = ServiceBusMessage(message)
                sender.send_messages(service_bus_message)
                logger.debug(f"Message sent to queue '{self.queue_name}'")
                
        except Exception as e:
            logger.error(f"Failed to send message to queue '{self.queue_name}': {e}")
            raise

    def receive_messages(self, callback: Callable[[bytes], None], max_messages: int = 10, timeout: int = 30) -> None:
        """
        Listens for messages from the queue and processes them using a callback.

        Args:
            callback (Callable[[bytes], None]): The function to call with the message body.
            max_messages (int): Maximum number of messages to receive in one batch.
            timeout (int): Timeout in seconds for receiving messages.
        """
        try:
            with self.sbus_client.get_queue_receiver(queue_name=self.queue_name, max_wait_time=timeout) as receiver:
                messages = receiver.receive_messages(max_message_count=max_messages, max_wait_time=timeout)
                
                for msg in messages:
                    try:
                        callback(msg.body)
                        receiver.complete_message(msg)
                        logger.debug(f"Processed message from queue '{self.queue_name}'")
                    except Exception as e:
                        logger.error(f"Error processing message: {e}")
                        receiver.abandon_message(msg)
                        
                logger.info(f"Processed {len(messages)} messages from queue '{self.queue_name}'")
                
        except Exception as e:
            logger.error(f"Failed to receive messages from queue '{self.queue_name}': {e}")
            raise 