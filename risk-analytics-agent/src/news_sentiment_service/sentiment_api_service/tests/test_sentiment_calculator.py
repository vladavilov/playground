import pytest
from datetime import datetime, timezone
from typing import List
from unittest.mock import patch, mock_open

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', 'common', 'src'))

from models.models import EnrichedNewsEvent, Entities, Sentiment
from src.sentiment_calculator import SentimentCalculator


class TestSentimentCalculator:
    """Test cases for the SentimentCalculator class and _calculate_ass function."""

    @pytest.fixture
    def mock_weights_config(self):
        """Mock configuration data for weights."""
        event_weights_yaml = """
event_weights:
  Credit_Rating_Downgrade: 15.0
  Earnings_Beat: 3.0
  General_News: 1.0
"""
        source_weights_yaml = """
source_weights:
  TIER_1_REGULATOR: 1.5
  TIER_2_PREMIUM_FINANCIAL: 1.2
  TIER_3_GENERAL_FINANCIAL: 1.0
"""
        return event_weights_yaml, source_weights_yaml

    @pytest.fixture
    def calculator(self, mock_weights_config):
        """Create a SentimentCalculator instance with mocked config files."""
        event_weights_yaml, source_weights_yaml = mock_weights_config
        
        with patch("builtins.open", mock_open()) as mock_file:
            mock_file.side_effect = [
                mock_open(read_data=event_weights_yaml).return_value,
                mock_open(read_data=source_weights_yaml).return_value
            ]
            return SentimentCalculator()

    @pytest.fixture
    def sample_events(self) -> List[EnrichedNewsEvent]:
        """Create sample EnrichedNewsEvent objects for testing."""
        base_time = datetime(2024, 1, 15, 12, 0, 0, tzinfo=timezone.utc)
        
        return [
            EnrichedNewsEvent(
                id="event1",
                source="Bloomberg",
                published_at=base_time,
                ingested_at=base_time,
                event_type="Credit_Rating_Downgrade",
                entities=Entities(
                    issuer_name="ABC Corp",
                    sector="Technology",
                    state=None,
                    cusips=["12345678X"]
                ),
                sentiment=Sentiment(score=-0.8, magnitude=0.9),
                source_credibility_tier="TIER_1_REGULATOR",
                summary_excerpt="S&P downgrades ABC Corp",
                raw_article_url="https://example.com/1"
            ),
            EnrichedNewsEvent(
                id="event2",
                source="Reuters",
                published_at=datetime(2024, 1, 15, 10, 0, 0, tzinfo=timezone.utc),
                ingested_at=base_time,
                event_type="Earnings_Beat",
                entities=Entities(
                    issuer_name="ABC Corp",
                    sector="Technology",
                    state=None,
                    cusips=["12345678X"]
                ),
                sentiment=Sentiment(score=0.6, magnitude=0.7),
                source_credibility_tier="TIER_2_PREMIUM_FINANCIAL",
                summary_excerpt="ABC Corp beats earnings",
                raw_article_url="https://example.com/2"
            ),
            EnrichedNewsEvent(
                id="event3",
                source="Financial Times",
                published_at=datetime(2024, 1, 14, 15, 0, 0, tzinfo=timezone.utc),
                ingested_at=base_time,
                event_type="General_News",
                entities=Entities(
                    issuer_name="ABC Corp",
                    sector="Technology",
                    state=None,
                    cusips=["12345678X"]
                ),
                sentiment=Sentiment(score=0.2, magnitude=0.3),
                source_credibility_tier="TIER_3_GENERAL_FINANCIAL",
                summary_excerpt="General market news",
                raw_article_url="https://example.com/3"
            )
        ]

    def test_calculate_ass_realtime_mode_basic(self, calculator, sample_events):
        """Test ASS calculation in REALTIME mode with basic scenario."""
        current_time = datetime(2024, 1, 15, 14, 0, 0, tzinfo=timezone.utc)
        
        result = calculator._calculate_ass(sample_events, "REALTIME", current_time)
        
        # Verify the result structure
        assert "aggregated_sentiment_score" in result
        assert "contributing_articles_count" in result
        assert result["contributing_articles_count"] == 3
        
        # The score should be negative due to the heavily weighted downgrade event
        assert result["aggregated_sentiment_score"] < 0
        assert isinstance(result["aggregated_sentiment_score"], float)

    def test_calculate_ass_historical_mode_basic(self, calculator, sample_events):
        """Test ASS calculation in HISTORICAL mode with basic scenario."""
        as_of_date = datetime(2024, 1, 15, 23, 59, 59, tzinfo=timezone.utc)
        
        result = calculator._calculate_ass(sample_events, "HISTORICAL", as_of_date)
        
        # Verify the result structure
        assert "aggregated_sentiment_score" in result
        assert "contributing_articles_count" in result
        assert result["contributing_articles_count"] == 3
        
        # The score should be negative due to the heavily weighted downgrade event
        assert result["aggregated_sentiment_score"] < 0
        assert isinstance(result["aggregated_sentiment_score"], float)

    def test_calculate_ass_empty_events_list(self, calculator):
        """Test ASS calculation with empty events list."""
        current_time = datetime(2024, 1, 15, 14, 0, 0, tzinfo=timezone.utc)
        
        result = calculator._calculate_ass([], "REALTIME", current_time)
        
        assert result["aggregated_sentiment_score"] == 0.0
        assert result["contributing_articles_count"] == 0

    def test_calculate_ass_single_event(self, calculator):
        """Test ASS calculation with single event."""
        current_time = datetime(2024, 1, 15, 14, 0, 0, tzinfo=timezone.utc)
        event_time = datetime(2024, 1, 15, 12, 0, 0, tzinfo=timezone.utc)
        
        single_event = [EnrichedNewsEvent(
            id="event1",
            source="Bloomberg",
            published_at=event_time,
            ingested_at=event_time,
            event_type="Credit_Rating_Downgrade",
            entities=Entities(
                issuer_name="ABC Corp",
                sector="Technology",
                state=None,
                cusips=["12345678X"]
            ),
            sentiment=Sentiment(score=-0.8, magnitude=0.9),
            source_credibility_tier="TIER_1_REGULATOR",
            summary_excerpt="S&P downgrades ABC Corp",
            raw_article_url="https://example.com/1"
        )]
        
        result = calculator._calculate_ass(single_event, "REALTIME", current_time)
        
        assert result["contributing_articles_count"] == 1
        # With single event, ASS should equal the sentiment score since weights cancel out in numerator/denominator
        assert result["aggregated_sentiment_score"] == -0.8

    def test_calculate_ass_formula_correctness(self, calculator):
        """Test that the ASS formula is implemented correctly with known values."""
        current_time = datetime(2024, 1, 15, 14, 0, 0, tzinfo=timezone.utc)
        
        # Create events with known values for manual calculation
        events = [
            EnrichedNewsEvent(
                id="event1",
                source="Bloomberg",
                published_at=datetime(2024, 1, 15, 13, 0, 0, tzinfo=timezone.utc),  # 1 hour ago
                ingested_at=current_time,
                event_type="Credit_Rating_Downgrade",  # W_event = 15.0
                entities=Entities(issuer_name="Test Corp", sector="Technology", state=None, cusips=["123456789"]),
                sentiment=Sentiment(score=-1.0, magnitude=1.0),  # S = -1.0, M = 1.0
                source_credibility_tier="TIER_1_REGULATOR",  # W_source = 1.5
                summary_excerpt="Test downgrade",
                raw_article_url="https://example.com/1"
            ),
            EnrichedNewsEvent(
                id="event2",
                source="Reuters",
                published_at=datetime(2024, 1, 15, 13, 0, 0, tzinfo=timezone.utc),  # 1 hour ago
                ingested_at=current_time,
                event_type="Earnings_Beat",  # W_event = 3.0
                entities=Entities(issuer_name="Test Corp", sector="Technology", state=None, cusips=["123456789"]),
                sentiment=Sentiment(score=1.0, magnitude=1.0),  # S = 1.0, M = 1.0
                source_credibility_tier="TIER_2_PREMIUM_FINANCIAL",  # W_source = 1.2
                summary_excerpt="Test earnings beat",
                raw_article_url="https://example.com/2"
            )
        ]
        
        result = calculator._calculate_ass(events, "REALTIME", current_time)
        
        # Manual calculation:
        # W_time for both events should be the same (1 hour ago)
        # Event 1: S=-1.0, M=1.0, W_event=15.0, W_source=1.5, W_time=same
        # Event 2: S=1.0, M=1.0, W_event=3.0, W_source=1.2, W_time=same
        # ASS = (S1*M1*W_event1*W_time1*W_source1 + S2*M2*W_event2*W_time2*W_source2) / 
        #       (M1*W_event1*W_time1*W_source1 + M2*W_event2*W_time2*W_source2)
        
        # Since W_time is the same for both, we can factor it out
        # Numerator = W_time * ((-1.0 * 1.0 * 15.0 * 1.5) + (1.0 * 1.0 * 3.0 * 1.2))
        #           = W_time * (-22.5 + 3.6) = W_time * (-18.9)
        # Denominator = W_time * ((1.0 * 15.0 * 1.5) + (1.0 * 3.0 * 1.2))
        #             = W_time * (22.5 + 3.6) = W_time * 26.1
        # ASS = -18.9 / 26.1 ≈ -0.7241
        
        expected_score = -18.9 / 26.1
        assert abs(result["aggregated_sentiment_score"] - expected_score) < 0.001
        assert result["contributing_articles_count"] == 2

    def test_time_decay_weight_calculation_realtime(self, calculator):
        """Test that time decay weights are calculated correctly for REALTIME mode."""
        current_time = datetime(2024, 1, 15, 14, 0, 0, tzinfo=timezone.utc)
        
        # Test with events at different times
        recent_event_time = datetime(2024, 1, 15, 13, 30, 0, tzinfo=timezone.utc)  # 30 min ago
        older_event_time = datetime(2024, 1, 15, 10, 0, 0, tzinfo=timezone.utc)     # 4 hours ago
        
        recent_weight = calculator._calculate_time_decay_weight(recent_event_time, current_time)
        older_weight = calculator._calculate_time_decay_weight(older_event_time, current_time)
        
        # Recent events should have higher weight than older events
        assert recent_weight > older_weight
        assert recent_weight > 0
        assert older_weight > 0

    def test_time_decay_weight_calculation_historical(self, calculator):
        """Test that time decay weights are calculated correctly for HISTORICAL mode."""
        as_of_date = datetime(2024, 1, 15, 23, 59, 59, tzinfo=timezone.utc)
        
        # Test with events at different times relative to end of day
        event_time_1 = datetime(2024, 1, 15, 20, 0, 0, tzinfo=timezone.utc)    # 4 hours before end of day
        event_time_2 = datetime(2024, 1, 14, 20, 0, 0, tzinfo=timezone.utc)    # 1 day + 4 hours before end of day
        
        weight_1 = calculator._calculate_time_decay_weight(event_time_1, as_of_date)
        weight_2 = calculator._calculate_time_decay_weight(event_time_2, as_of_date)
        
        # More recent events should have higher weight
        assert weight_1 > weight_2
        assert weight_1 > 0
        assert weight_2 > 0

    def test_invalid_mode_raises_error(self, calculator, sample_events):
        """Test that invalid calculation mode raises an error."""
        current_time = datetime(2024, 1, 15, 14, 0, 0, tzinfo=timezone.utc)
        
        with pytest.raises(ValueError, match="Invalid mode"):
            calculator._calculate_ass(sample_events, "INVALID_MODE", current_time) 