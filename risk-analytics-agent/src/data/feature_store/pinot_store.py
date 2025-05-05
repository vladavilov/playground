"""Apache Pinot implementation of the feature store interface.

This module provides an Apache Pinot-based implementation of the feature store interface,
optimized for analytical queries and aggregations across large datasets.
"""

import logging
import json
from datetime import datetime
from typing import Any, Dict, List, Optional, Tuple, Union

import pandas as pd
from pinotdb import connect as pinot_connect
from sqlalchemy.exc import SQLAlchemyError

from .base_store import FeatureStore

# Configure logging
logger = logging.getLogger(__name__)


class PinotFeatureStore(FeatureStore):
    """Apache Pinot implementation of the feature store interface.
    
    This implementation uses Apache Pinot for analytical queries and aggregations 
    on large datasets, particularly for historical feature values.
    """
    
    def __init__(
        self,
        host: str = "localhost",
        port: int = 8099,
        scheme: str = "http",
        timeout: int = 60,
        feature_table: str = "features",
        history_table: str = "feature_history",
        entity_column: str = "entity_id",
        feature_column: str = "feature_name",
        value_column: str = "feature_value",
        metadata_column: str = "metadata",
        timestamp_column: str = "timestamp",
        use_ssl: bool = False,
        ssl_verify: bool = True
    ):
        """Initialize the Pinot feature store.
        
        Args:
            host: Pinot server host
            port: Pinot server port
            scheme: HTTP scheme (http or https)
            timeout: Timeout for Pinot queries (in seconds)
            feature_table: Table name for current feature values
            history_table: Table name for historical feature values
            entity_column: Column name for entity IDs
            feature_column: Column name for feature names
            value_column: Column name for feature values
            metadata_column: Column name for metadata
            timestamp_column: Column name for timestamps
            use_ssl: Whether to use SSL
            ssl_verify: Whether to verify SSL certificates
        """
        self.host = host
        self.port = port
        self.scheme = scheme
        self.timeout = timeout
        self.feature_table = feature_table
        self.history_table = history_table
        self.entity_column = entity_column
        self.feature_column = feature_column
        self.value_column = value_column
        self.metadata_column = metadata_column
        self.timestamp_column = timestamp_column
        self.use_ssl = use_ssl
        self.ssl_verify = ssl_verify
        
        self.connection = None
        self.is_connected = False
        
    def connect(self) -> bool:
        """Connect to Pinot.
        
        Returns:
            bool: True if connection successful, False otherwise
        """
        try:
            self.connection = pinot_connect(
                host=self.host,
                port=self.port,
                scheme=self.scheme,
                timeout=self.timeout,
                path="/query/sql",
                verify=self.ssl_verify if self.use_ssl else None
            )
            
            # Test connection with a simple query
            cursor = self.connection.cursor()
            cursor.execute("SELECT 1")
            cursor.fetchall()
            cursor.close()
            
            self.is_connected = True
            logger.info(f"Connected to Pinot at {self.host}:{self.port}")
            return True
            
        except Exception as e:
            logger.error(f"Failed to connect to Pinot at {self.host}:{self.port}: {e}")
            self.connection = None
            self.is_connected = False
            return False
            
    def disconnect(self) -> bool:
        """Disconnect from Pinot.
        
        Returns:
            bool: True if disconnection successful, False otherwise
        """
        if not self.is_connected:
            return True
            
        try:
            if self.connection:
                self.connection.close()
                
            self.connection = None
            self.is_connected = False
            
            logger.info(f"Disconnected from Pinot at {self.host}:{self.port}")
            return True
            
        except Exception as e:
            logger.error(f"Error disconnecting from Pinot: {e}")
            return False
            
    def _serialize_value(self, value: Any) -> str:
        """Serialize a value for storage in Pinot.
        
        Args:
            value: Value to serialize
            
        Returns:
            str: JSON-serialized value
        """
        try:
            return json.dumps(value)
        except (TypeError, ValueError) as e:
            logger.error(f"Error serializing value: {e}")
            return str(value)
            
    def _deserialize_value(self, serialized_value: str) -> Any:
        """Deserialize a value from Pinot storage.
        
        Args:
            serialized_value: Serialized value
            
        Returns:
            Any: Deserialized value
        """
        if not serialized_value:
            return None
            
        try:
            return json.loads(serialized_value)
        except (json.JSONDecodeError, ValueError) as e:
            logger.error(f"Error deserializing value: {e}")
            return serialized_value
            
    def _serialize_metadata(self, metadata: Dict[str, Any]) -> str:
        """Serialize metadata for storage in Pinot.
        
        Args:
            metadata: Metadata to serialize
            
        Returns:
            str: JSON-serialized metadata
        """
        try:
            return json.dumps(metadata)
        except (TypeError, ValueError) as e:
            logger.error(f"Error serializing metadata: {e}")
            return "{}"
            
    def _deserialize_metadata(self, serialized_metadata: str) -> Dict[str, Any]:
        """Deserialize metadata from Pinot storage.
        
        Args:
            serialized_metadata: Serialized metadata
            
        Returns:
            Dict[str, Any]: Deserialized metadata
        """
        if not serialized_metadata:
            return {}
            
        try:
            return json.loads(serialized_metadata)
        except (json.JSONDecodeError, ValueError) as e:
            logger.error(f"Error deserializing metadata: {e}")
            return {}
            
    def write_feature(
        self, 
        feature_name: str, 
        feature_value: Any,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> bool:
        """Write a feature value to Pinot.
        
        Args:
            feature_name: Name of the feature
            feature_value: Value of the feature
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for the feature value
            metadata: Optional metadata associated with this feature value
            
        Returns:
            bool: True if write successful, False otherwise
        """
        if not self.is_connected or not self.connection:
            logger.error("Not connected to Pinot")
            return False
            
        try:
            # Prepare timestamp
            if timestamp is None:
                timestamp = datetime.now()
            timestamp_str = timestamp.isoformat()
            
            # Prepare metadata
            if metadata is None:
                metadata = {}
            metadata['timestamp'] = timestamp_str
            
            # Serialize value and metadata
            serialized_value = self._serialize_value(feature_value)
            serialized_metadata = self._serialize_metadata(metadata)
            
            # Create cursor
            cursor = self.connection.cursor()
            
            # For the feature table (current values), we need to use UPSERT semantics
            # Check if the feature exists for this entity
            entity_clause = f"AND {self.entity_column} = '{entity_id}'" if entity_id else ""
            check_query = f"""
            SELECT COUNT(*) FROM {self.feature_table}
            WHERE {self.feature_column} = %s {entity_clause}
            """
            
            cursor.execute(check_query, (feature_name,))
            exists = cursor.fetchone()[0] > 0
            
            # If it exists, update it; otherwise, insert it
            if exists:
                update_query = f"""
                UPDATE {self.feature_table}
                SET {self.value_column} = %s,
                    {self.metadata_column} = %s,
                    {self.timestamp_column} = %s
                WHERE {self.feature_column} = %s {entity_clause}
                """
                cursor.execute(update_query, (serialized_value, serialized_metadata, timestamp_str, feature_name))
            else:
                entity_val = entity_id if entity_id else "NULL"
                insert_query = f"""
                INSERT INTO {self.feature_table} (
                    {self.feature_column}, 
                    {self.entity_column}, 
                    {self.value_column}, 
                    {self.metadata_column}, 
                    {self.timestamp_column}
                ) VALUES (%s, %s, %s, %s, %s)
                """
                cursor.execute(insert_query, (feature_name, entity_val, serialized_value, serialized_metadata, timestamp_str))
                
            # Always insert into history table for auditing/time series
            history_insert_query = f"""
            INSERT INTO {self.history_table} (
                {self.feature_column}, 
                {self.entity_column}, 
                {self.value_column}, 
                {self.metadata_column}, 
                {self.timestamp_column}
            ) VALUES (%s, %s, %s, %s, %s)
            """
            entity_val = entity_id if entity_id else "NULL"
            cursor.execute(history_insert_query, (feature_name, entity_val, serialized_value, serialized_metadata, timestamp_str))
            
            cursor.close()
            
            logger.debug(f"Wrote feature {feature_name} for entity {entity_id} to Pinot")
            return True
            
        except Exception as e:
            logger.error(f"Error writing feature to Pinot: {e}")
            return False
            
    def read_feature(
        self,
        feature_name: str,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None
    ) -> Tuple[Any, Dict[str, Any]]:
        """Read a feature value from Pinot.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for point-in-time feature retrieval
            
        Returns:
            Tuple[Any, Dict[str, Any]]: Feature value and metadata
        """
        if not self.is_connected or not self.connection:
            logger.error("Not connected to Pinot")
            return None, {}
            
        try:
            # If timestamp is provided, use history to get point-in-time value
            if timestamp is not None:
                history_data = self.read_feature_history(
                    feature_name=feature_name,
                    entity_id=entity_id,
                    end_time=timestamp,
                    limit=1
                )
                
                if history_data:
                    # Return the most recent value before or at the timestamp
                    ts, value, meta = history_data[0]
                    return value, meta
                else:
                    logger.warning(f"No historical data found for feature {feature_name} at {timestamp}")
                    return None, {}
            
            # Otherwise get the latest value from the feature table
            cursor = self.connection.cursor()
            
            entity_clause = f"AND {self.entity_column} = '{entity_id}'" if entity_id else ""
            query = f"""
            SELECT {self.value_column}, {self.metadata_column}
            FROM {self.feature_table}
            WHERE {self.feature_column} = %s {entity_clause}
            """
            
            cursor.execute(query, (feature_name,))
            row = cursor.fetchone()
            cursor.close()
            
            if not row:
                logger.warning(f"Feature {feature_name} for entity {entity_id} not found in Pinot")
                return None, {}
                
            # Extract value and metadata
            serialized_value, serialized_metadata = row
            value = self._deserialize_value(serialized_value)
            metadata = self._deserialize_metadata(serialized_metadata)
            
            return value, metadata
            
        except Exception as e:
            logger.error(f"Error reading feature from Pinot: {e}")
            return None, {}
            
    def read_feature_history(
        self,
        feature_name: str,
        entity_id: Optional[str] = None,
        start_time: Optional[datetime] = None,
        end_time: Optional[datetime] = None,
        limit: Optional[int] = None
    ) -> List[Tuple[datetime, Any, Dict[str, Any]]]:
        """Read historical feature values from Pinot.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            start_time: Optional start time for the history (inclusive)
            end_time: Optional end time for the history (inclusive)
            limit: Optional maximum number of values to return
            
        Returns:
            List[Tuple[datetime, Any, Dict[str, Any]]]: List of (timestamp, value, metadata) tuples
        """
        if not self.is_connected or not self.connection:
            logger.error("Not connected to Pinot")
            return []
            
        try:
            cursor = self.connection.cursor()
            
            # Build query conditions
            conditions = [f"{self.feature_column} = '{feature_name}'"]
            
            if entity_id:
                conditions.append(f"{self.entity_column} = '{entity_id}'")
                
            if start_time:
                start_time_str = start_time.isoformat()
                conditions.append(f"{self.timestamp_column} >= '{start_time_str}'")
                
            if end_time:
                end_time_str = end_time.isoformat()
                conditions.append(f"{self.timestamp_column} <= '{end_time_str}'")
                
            # Build query
            where_clause = " AND ".join(conditions)
            limit_clause = f"LIMIT {limit}" if limit is not None else ""
            
            query = f"""
            SELECT {self.timestamp_column}, {self.value_column}, {self.metadata_column}
            FROM {self.history_table}
            WHERE {where_clause}
            ORDER BY {self.timestamp_column} DESC
            {limit_clause}
            """
            
            cursor.execute(query)
            rows = cursor.fetchall()
            cursor.close()
            
            # Process results
            history = []
            for timestamp_str, serialized_value, serialized_metadata in rows:
                # Convert timestamp string to datetime
                try:
                    timestamp = datetime.fromisoformat(timestamp_str)
                except ValueError:
                    # Handle various timestamp formats
                    timestamp = datetime.strptime(timestamp_str, "%Y-%m-%dT%H:%M:%S.%fZ")
                    
                # Deserialize value and metadata
                value = self._deserialize_value(serialized_value)
                metadata = self._deserialize_metadata(serialized_metadata)
                
                history.append((timestamp, value, metadata))
                
            return history
            
        except Exception as e:
            logger.error(f"Error reading feature history from Pinot: {e}")
            return []
            
    def read_features(
        self,
        feature_names: List[str],
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None
    ) -> Dict[str, Tuple[Any, Dict[str, Any]]]:
        """Read multiple feature values from Pinot.
        
        Args:
            feature_names: List of feature names
            entity_id: Optional ID of the entity associated with these features
            timestamp: Optional timestamp for point-in-time feature retrieval
            
        Returns:
            Dict[str, Tuple[Any, Dict[str, Any]]]: Dict mapping feature names to (value, metadata) tuples
        """
        if not self.is_connected or not self.connection:
            logger.error("Not connected to Pinot")
            return {}
            
        try:
            cursor = self.connection.cursor()
            
            # If timestamp is provided, we need to use the history table
            if timestamp is not None:
                # For each feature, find the most recent value before or at the timestamp
                results = {}
                timestamp_str = timestamp.isoformat()
                
                # Build IN clause for feature names
                feature_names_str = ", ".join([f"'{name}'" for name in feature_names])
                
                # Build query conditions
                conditions = [f"{self.feature_column} IN ({feature_names_str})"]
                
                if entity_id:
                    conditions.append(f"{self.entity_column} = '{entity_id}'")
                    
                conditions.append(f"{self.timestamp_column} <= '{timestamp_str}'")
                
                where_clause = " AND ".join(conditions)
                
                # We need the most recent value for each feature before the timestamp
                query = f"""
                SELECT {self.feature_column}, {self.value_column}, {self.metadata_column}
                FROM {self.history_table}
                WHERE {where_clause}
                GROUP BY {self.feature_column}
                ORDER BY {self.timestamp_column} DESC
                """
                
                cursor.execute(query)
                rows = cursor.fetchall()
                
                for feature_name, serialized_value, serialized_metadata in rows:
                    value = self._deserialize_value(serialized_value)
                    metadata = self._deserialize_metadata(serialized_metadata)
                    results[feature_name] = (value, metadata)
                    
                cursor.close()
                return results
                
            # Otherwise, get the latest values from the feature table
            feature_names_str = ", ".join([f"'{name}'" for name in feature_names])
            entity_clause = f"AND {self.entity_column} = '{entity_id}'" if entity_id else ""
            
            query = f"""
            SELECT {self.feature_column}, {self.value_column}, {self.metadata_column}
            FROM {self.feature_table}
            WHERE {self.feature_column} IN ({feature_names_str}) {entity_clause}
            """
            
            cursor.execute(query)
            rows = cursor.fetchall()
            cursor.close()
            
            results = {}
            for feature_name, serialized_value, serialized_metadata in rows:
                value = self._deserialize_value(serialized_value)
                metadata = self._deserialize_metadata(serialized_metadata)
                results[feature_name] = (value, metadata)
                
            return results
            
        except Exception as e:
            logger.error(f"Error reading features from Pinot: {e}")
            return {}
            
    def delete_feature(
        self,
        feature_name: str,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None
    ) -> bool:
        """Delete a feature value from Pinot.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for the feature value to delete
            
        Returns:
            bool: True if deletion successful, False otherwise
        """
        if not self.is_connected or not self.connection:
            logger.error("Not connected to Pinot")
            return False
            
        try:
            cursor = self.connection.cursor()
            
            # Build conditions
            conditions = [f"{self.feature_column} = '{feature_name}'"]
            
            if entity_id:
                conditions.append(f"{self.entity_column} = '{entity_id}'")
                
            where_clause = " AND ".join(conditions)
            
            if timestamp is not None:
                # Delete specific timestamp from history table
                timestamp_str = timestamp.isoformat()
                history_query = f"""
                DELETE FROM {self.history_table}
                WHERE {where_clause} AND {self.timestamp_column} = '{timestamp_str}'
                """
                cursor.execute(history_query)
            else:
                # Delete from both feature and history tables
                feature_query = f"""
                DELETE FROM {self.feature_table}
                WHERE {where_clause}
                """
                cursor.execute(feature_query)
                
                history_query = f"""
                DELETE FROM {self.history_table}
                WHERE {where_clause}
                """
                cursor.execute(history_query)
                
            cursor.close()
            
            logger.info(f"Deleted feature {feature_name} for entity {entity_id} from Pinot")
            return True
            
        except Exception as e:
            logger.error(f"Error deleting feature from Pinot: {e}")
            return False
            
    def list_features(
        self,
        entity_id: Optional[str] = None,
        prefix: Optional[str] = None
    ) -> List[str]:
        """List available features in Pinot.
        
        Args:
            entity_id: Optional ID of the entity to filter features
            prefix: Optional prefix to filter feature names
            
        Returns:
            List[str]: List of feature names
        """
        if not self.is_connected or not self.connection:
            logger.error("Not connected to Pinot")
            return []
            
        try:
            cursor = self.connection.cursor()
            
            # Build conditions
            conditions = []
            
            if entity_id:
                conditions.append(f"{self.entity_column} = '{entity_id}'")
                
            if prefix:
                conditions.append(f"{self.feature_column} LIKE '{prefix}%'")
                
            where_clause = f"WHERE {' AND '.join(conditions)}" if conditions else ""
            
            # Get distinct feature names
            query = f"""
            SELECT DISTINCT {self.feature_column}
            FROM {self.feature_table}
            {where_clause}
            """
            
            cursor.execute(query)
            rows = cursor.fetchall()
            cursor.close()
            
            # Extract feature names
            features = [row[0] for row in rows]
            return features
            
        except Exception as e:
            logger.error(f"Error listing features from Pinot: {e}")
            return []
            
    def health_check(self) -> Dict[str, Any]:
        """Check the health of the Pinot feature store.
        
        Returns:
            Dict[str, Any]: Health check information including connection status and metrics
        """
        health_info = {
            "store_type": "pinot",
            "connected": self.is_connected,
            "host": self.host,
            "port": self.port,
            "metrics": {}
        }
        
        if not self.is_connected or not self.connection:
            return health_info
            
        try:
            # Test connection
            cursor = self.connection.cursor()
            
            # Check if tables exist
            tables_query = "SHOW TABLES"
            cursor.execute(tables_query)
            tables = [row[0] for row in cursor.fetchall()]
            
            health_info["tables"] = tables
            health_info["feature_table_exists"] = self.feature_table in tables
            health_info["history_table_exists"] = self.history_table in tables
            
            # Get feature counts if tables exist
            if self.feature_table in tables:
                feature_count_query = f"SELECT COUNT(*) FROM {self.feature_table}"
                cursor.execute(feature_count_query)
                feature_count = cursor.fetchone()[0]
                health_info["feature_count"] = feature_count
                
            if self.history_table in tables:
                history_count_query = f"SELECT COUNT(*) FROM {self.history_table}"
                cursor.execute(history_count_query)
                history_count = cursor.fetchone()[0]
                health_info["history_count"] = history_count
                
            # Get server metrics
            server_metrics_query = "SELECT * FROM pinot.server.metrics"
            cursor.execute(server_metrics_query)
            server_metrics = cursor.fetchall()
            
            health_info["metrics"] = {
                "server_count": len(server_metrics)
            }
            
            cursor.close()
            return health_info
            
        except Exception as e:
            logger.error(f"Error checking Pinot health: {e}")
            health_info["error"] = str(e)
            health_info["connected"] = False
            return health_info
            
    def run_query(self, query: str) -> pd.DataFrame:
        """Run a custom SQL query against Pinot.
        
        Args:
            query: SQL query string
            
        Returns:
            pd.DataFrame: Query results as a pandas DataFrame
        """
        if not self.is_connected or not self.connection:
            logger.error("Not connected to Pinot")
            return pd.DataFrame()
            
        try:
            cursor = self.connection.cursor()
            cursor.execute(query)
            
            # Get column names
            columns = [desc[0] for desc in cursor.description]
            
            # Fetch all rows
            rows = cursor.fetchall()
            cursor.close()
            
            # Create DataFrame
            df = pd.DataFrame(rows, columns=columns)
            return df
            
        except Exception as e:
            logger.error(f"Error executing query against Pinot: {e}")
            return pd.DataFrame() 