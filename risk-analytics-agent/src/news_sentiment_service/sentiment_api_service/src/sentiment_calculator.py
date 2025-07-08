import os
import yaml
import math
from datetime import datetime, timezone
from typing import List, Dict, Any
from pathlib import Path

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', 'common', 'src'))

from models.models import EnrichedNewsEvent


class SentimentCalculator:
    """
    Calculates Aggregated Sentiment Scores (ASS) using the weighted formula:
    ASS = Σ(Si * Mi * W_eventi * W_timei * W_sourcei) / Σ(Mi * W_eventi * W_timei * W_sourcei)
    """

    def __init__(self):
        """Initialize the calculator and load weight configurations."""
        self.event_weights = self._load_event_weights()
        self.source_weights = self._load_source_weights()

    def _load_event_weights(self) -> Dict[str, float]:
        """Load event type weights from YAML configuration."""
        config_path = Path(__file__).parent / "config" / "event_weights.yaml"
        try:
            with open(config_path, 'r') as file:
                config = yaml.safe_load(file)
                return config.get('event_weights', {})
        except Exception as e:
            # Default weights if config file not found
            return {
                "Credit_Rating_Downgrade": 15.0,
                "Earnings_Beat": 3.0,
                "General_News": 1.0
            }

    def _load_source_weights(self) -> Dict[str, float]:
        """Load source credibility weights from YAML configuration."""
        config_path = Path(__file__).parent / "config" / "source_weights.yaml"
        try:
            with open(config_path, 'r') as file:
                config = yaml.safe_load(file)
                return config.get('source_weights', {})
        except Exception as e:
            # Default weights if config file not found
            return {
                "TIER_1_REGULATOR": 1.5,
                "TIER_2_PREMIUM_FINANCIAL": 1.2,
                "TIER_3_GENERAL_FINANCIAL": 1.0
            }

    def _calculate_time_decay_weight(self, event_time: datetime, reference_time: datetime) -> float:
        """
        Calculate time decay weight based on the time difference between event and reference time.
        
        Args:
            event_time: When the event was published
            reference_time: Reference time (current time for REALTIME, end of day for HISTORICAL)
            
        Returns:
            Time decay weight (higher for more recent events)
        """
        # Calculate time difference in hours
        time_diff = (reference_time - event_time).total_seconds() / 3600
        
        # Exponential decay with half-life of 24 hours
        # W_time = 2^(-t/24) where t is hours since event
        decay_weight = math.exp(-time_diff * math.log(2) / 24)
        
        # Ensure minimum weight of 0.01 to avoid division by zero
        return max(decay_weight, 0.01)

    def _calculate_ass(self, events: List[EnrichedNewsEvent], mode: str, reference_time: datetime) -> Dict[str, Any]:
        """
        Calculate the Aggregated Sentiment Score (ASS) for a list of events.
        
        Args:
            events: List of EnrichedNewsEvent objects
            mode: Either "REALTIME" or "HISTORICAL"
            reference_time: Current time for REALTIME, end of day for HISTORICAL
            
        Returns:
            Dictionary with aggregated_sentiment_score and contributing_articles_count
        """
        if mode not in ["REALTIME", "HISTORICAL"]:
            raise ValueError(f"Invalid mode: {mode}. Must be 'REALTIME' or 'HISTORICAL'")
        
        if not events:
            result = {
                "aggregated_sentiment_score": 0.0,
                "contributing_articles_count": 0
            }
            if mode == "REALTIME":
                result["articles"] = []
            return result

        numerator = 0.0
        denominator = 0.0
        article_urls = []

        for event in events:
            # Get component values
            sentiment_score = event.sentiment.score  # Si
            magnitude = event.sentiment.magnitude    # Mi
            
            # Get weights
            w_event = self.event_weights.get(event.event_type, 1.0)
            w_source = self.source_weights.get(event.source_credibility_tier, 1.0)
            w_time = self._calculate_time_decay_weight(event.published_at, reference_time)
            
            # Calculate weighted contribution
            weight_product = magnitude * w_event * w_time * w_source
            
            numerator += sentiment_score * weight_product
            denominator += weight_product
            
            if mode == "REALTIME":
                article_urls.append(event.raw_article_url)

        # Calculate final ASS
        if denominator > 0:
            ass = numerator / denominator
        else:
            ass = 0.0

        result = {
            "aggregated_sentiment_score": ass,
            "contributing_articles_count": len(events)
        }
        
        if mode == "REALTIME":
            result["articles"] = article_urls
            
        return result 