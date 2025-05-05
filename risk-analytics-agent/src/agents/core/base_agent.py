"""
Base Agent Implementation for Risk Analytics 
"""

import os
import yaml
import time
import logging
import json
from typing import Dict, List, Any, Optional, Union
from abc import ABC, abstractmethod

from risk_analytics_agent.src.agents.core.memory import AgentMemory

class BaseAgent(ABC):
    """
    Base class for all agents in the risk analytics system.
    
    Provides core functionality for loading configuration, 
    memory management, reasoning, and action execution.
    """
    
    def __init__(self, config_path: str):
        """
        Initialize the base agent.
        
        Args:
            config_path: Path to agent configuration file
        """
        self.config = self._load_config(config_path)
        self.name = self.config.get('name', 'unnamed_agent')
        self.version = self.config.get('version', '0.1')
        self.logger = self._setup_logger()
        
        # Initialize memory based on configuration
        self.memory = self._initialize_memory()
        
        # Initialize models dictionary
        self.models = {}
        
        # Initialize metrics for monitoring
        self.metrics = {
            'decisions_made': 0,
            'decision_latency_ms': [],
            'confidence_scores': []
        }
    
    def _load_config(self, config_path: str) -> Dict:
        """
        Load configuration from YAML file.
        
        Args:
            config_path: Path to configuration file
            
        Returns:
            Configuration dictionary
        """
        with open(config_path, 'r') as file:
            config = yaml.safe_load(file)
        
        self.logger.info(f"Loaded configuration from {config_path}")
        return config
    
    def _setup_logger(self) -> logging.Logger:
        """
        Set up logger for the agent.
        
        Returns:
            Configured logger
        """
        logger = logging.getLogger(f"{self.name}_agent")
        logger.setLevel(logging.INFO)
        
        # Create console handler
        handler = logging.StreamHandler()
        formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        handler.setFormatter(formatter)
        logger.addHandler(handler)
        
        return logger
    
    def _initialize_memory(self) -> AgentMemory:
        """
        Initialize agent memory based on configuration.
        
        Returns:
            Initialized memory component
        """
        # Get memory configuration
        memory_config = self.config.get('components', {}).get('memory', {})
        
        # Create memory component
        memory = AgentMemory(
            short_term_config=memory_config.get('short_term', {}),
            long_term_config=memory_config.get('long_term', {}),
            episodic_config=memory_config.get('episodic', {})
        )
        
        self.logger.info("Initialized agent memory")
        return memory
    
    def load_models(self) -> None:
        """
        Load models specified in the agent configuration.
        
        Each agent implementation should override this to load
        the specific models it needs.
        """
        self.logger.info("Base model loading method - override in subclass")
    
    @abstractmethod
    def process_input(self, inputs: Dict[str, Any]) -> Dict[str, Any]:
        """
        Process input data and return results.
        
        Args:
            inputs: Dictionary containing input data
            
        Returns:
            Dictionary containing processing results
        """
        pass
    
    @abstractmethod
    def make_decision(self, processed_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Make a decision based on processed data.
        
        Args:
            processed_data: Dictionary containing processed input data
            
        Returns:
            Dictionary containing decision and related information
        """
        pass
    
    def explain_decision(self, decision: Dict[str, Any], detail_level: str = 'standard') -> Dict[str, Any]:
        """
        Generate an explanation for a decision.
        
        Args:
            decision: The decision to explain
            detail_level: Level of detail for explanation ('minimal', 'standard', 'detailed')
            
        Returns:
            Dictionary containing explanation
        """
        # Default implementation returns minimal explanation
        # Subclasses should override for more sophisticated explanations
        explanation = {
            'decision': decision.get('decision'),
            'confidence': decision.get('confidence_score'),
            'factors': decision.get('contributing_factors', [])[:3]
        }
        
        if detail_level == 'detailed':
            explanation['full_factors'] = decision.get('contributing_factors', [])
            explanation['reasoning_steps'] = decision.get('reasoning_steps', [])
        
        return explanation
    
    def execute(self, inputs: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute the agent's full pipeline on the input data.
        
        Args:
            inputs: Dictionary containing input data
            
        Returns:
            Dictionary containing execution results
        """
        start_time = time.time()
        
        # Step 1: Store input in memory
        self.memory.add_to_short_term('inputs', inputs)
        
        # Step 2: Process input
        processed_data = self.process_input(inputs)
        self.memory.add_to_short_term('processed_data', processed_data)
        
        # Step 3: Make decision
        decision = self.make_decision(processed_data)
        self.memory.add_to_short_term('decision', decision)
        
        # Step 4: Generate explanation
        explanation = self.explain_decision(decision)
        
        # Step 5: Prepare output
        output = self._format_output(decision, explanation)
        
        # Update metrics
        execution_time_ms = (time.time() - start_time) * 1000
        self.metrics['decisions_made'] += 1
        self.metrics['decision_latency_ms'].append(execution_time_ms)
        self.metrics['confidence_scores'].append(decision.get('confidence_score', 0))
        
        # Log execution
        self.logger.info(f"Executed agent pipeline in {execution_time_ms:.2f}ms with "
                        f"confidence {decision.get('confidence_score', 0):.2f}")
        
        return output
    
    def _format_output(self, decision: Dict[str, Any], explanation: Dict[str, Any]) -> Dict[str, Any]:
        """
        Format the output according to the agent's configuration.
        
        Args:
            decision: Decision dictionary
            explanation: Explanation dictionary
            
        Returns:
            Formatted output dictionary
        """
        # Get output format from config
        output_format = self.config.get('interfaces', {}).get('outputs', {}).get('format', 'json')
        output_fields = self.config.get('interfaces', {}).get('outputs', {}).get('fields', [])
        
        # Start with basic output
        output = {
            'agent_name': self.name,
            'agent_version': self.version,
            'timestamp': time.time()
        }
        
        # Add decision fields
        for field in output_fields:
            if field in decision:
                output[field] = decision[field]
            elif field in explanation:
                output[field] = explanation[field]
        
        # Add explanation if not already included
        if 'explanation' not in output and 'explanation' in explanation:
            output['explanation'] = explanation['explanation']
        
        return output
    
    def get_metrics(self) -> Dict[str, Any]:
        """
        Get current agent metrics.
        
        Returns:
            Dictionary of metrics
        """
        metrics = self.metrics.copy()
        
        # Calculate averages
        if metrics['decision_latency_ms']:
            metrics['avg_decision_latency_ms'] = sum(metrics['decision_latency_ms']) / len(metrics['decision_latency_ms'])
        
        if metrics['confidence_scores']:
            metrics['avg_confidence'] = sum(metrics['confidence_scores']) / len(metrics['confidence_scores'])
        
        return metrics
    
    def save_state(self, path: str) -> None:
        """
        Save agent state to disk.
        
        Args:
            path: Directory to save state
        """
        os.makedirs(path, exist_ok=True)
        
        # Save configuration
        with open(os.path.join(path, f"{self.name}_config.yaml"), 'w') as file:
            yaml.dump(self.config, file)
        
        # Save metrics
        with open(os.path.join(path, f"{self.name}_metrics.json"), 'w') as file:
            json.dump(self.metrics, file)
        
        # Save memory state (if supported by memory implementation)
        memory_path = os.path.join(path, 'memory')
        os.makedirs(memory_path, exist_ok=True)
        self.memory.save(memory_path)
        
        self.logger.info(f"Agent state saved to {path}")
    
    def load_state(self, path: str) -> None:
        """
        Load agent state from disk.
        
        Args:
            path: Directory containing saved state
        """
        # Load configuration
        config_path = os.path.join(path, f"{self.name}_config.yaml")
        if os.path.exists(config_path):
            with open(config_path, 'r') as file:
                self.config = yaml.safe_load(file)
        
        # Load metrics
        metrics_path = os.path.join(path, f"{self.name}_metrics.json")
        if os.path.exists(metrics_path):
            with open(metrics_path, 'r') as file:
                self.metrics = json.load(file)
        
        # Load memory state
        memory_path = os.path.join(path, 'memory')
        if os.path.exists(memory_path):
            self.memory.load(memory_path)
        
        self.logger.info(f"Agent state loaded from {path}") 