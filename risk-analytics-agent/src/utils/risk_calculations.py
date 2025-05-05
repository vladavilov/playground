"""
Risk Metric Calculation Module for Fixed Income Securities
"""

import pandas as pd
import numpy as np
from typing import Dict, List, Optional, Union, Tuple
import logging

logger = logging.getLogger(__name__)

class RiskMetricsCalculator:
    """
    Calculator for various fixed income risk metrics.
    
    Provides methods to calculate risk metrics such as duration, convexity,
    OAS, volatility, and liquidity scores.
    """
    
    def __init__(self, yield_curve_data: Optional[Dict] = None):
        """
        Initialize the risk metrics calculator.
        
        Args:
            yield_curve_data: Optional dictionary containing yield curve data
        """
        self.yield_curve_data = yield_curve_data
    
    def calculate_duration_metrics(self, 
                                  price: float, 
                                  yield_to_maturity: float, 
                                  cash_flows: List[Tuple[float, float]] = None,
                                  maturity: float = None) -> Dict[str, float]:
        """
        Calculate duration and convexity metrics.
        
        Args:
            price: Bond price
            yield_to_maturity: YTM in decimal form (e.g., 0.05 for 5%)
            cash_flows: Optional list of (time, amount) cash flows
            maturity: Optional maturity in years if cash_flows not provided
            
        Returns:
            Dictionary with duration metrics
        """
        # Simple duration calculation for zero-coupon bond if no cash flows
        if cash_flows is None:
            if maturity is None:
                raise ValueError("Either cash_flows or maturity must be provided")
            
            # For zero-coupon bond
            macaulay_duration = maturity
            modified_duration = macaulay_duration / (1 + yield_to_maturity)
            convexity = maturity * (maturity + 1) / (1 + yield_to_maturity)**2
        else:
            # Calculate duration from cash flows
            total_pv = 0
            weighted_time = 0
            weighted_time_squared = 0
            
            for time, amount in cash_flows:
                pv_factor = 1 / (1 + yield_to_maturity)**time
                pv = amount * pv_factor
                total_pv += pv
                weighted_time += time * pv
                weighted_time_squared += time * (time + 1) * pv
            
            # Scale to match price if provided
            scale_factor = price / total_pv if total_pv > 0 else 1.0
            
            # Macaulay duration
            macaulay_duration = weighted_time / total_pv
            
            # Modified duration
            modified_duration = macaulay_duration / (1 + yield_to_maturity)
            
            # Convexity
            convexity = weighted_time_squared / (total_pv * (1 + yield_to_maturity)**2)
        
        # Calculate effective duration if yield curve data is available
        if self.yield_curve_data is not None:
            # This would be more complex and depend on the yield curve data structure
            # For now, use a simplified approximation
            effective_duration = modified_duration * 1.05  # Simple approximation
        else:
            effective_duration = modified_duration
        
        return {
            "macaulay_duration": macaulay_duration,
            "modified_duration": modified_duration,
            "effective_duration": effective_duration,
            "convexity": convexity
        }
    
    def calculate_option_adjusted_spread(self, 
                                        price: float,
                                        cash_flows: List[Tuple[float, float]],
                                        volatility: float = 0.2,
                                        steps: int = 100) -> float:
        """
        Calculate Option-Adjusted Spread (OAS).
        
        Args:
            price: Current market price
            cash_flows: List of (time, amount) cash flows
            volatility: Interest rate volatility assumption
            steps: Number of steps in binomial tree
            
        Returns:
            OAS in basis points
        """
        # In a real implementation, this would use a binomial interest rate tree
        # and iteratively solve for the OAS that makes the model price match market price
        
        # For now, returning a simplified approximation
        # In practice, this calculation is complex and often uses specialized libraries
        
        if not cash_flows:
            return 0.0
        
        # Simple approximation based on average maturity and price
        avg_maturity = sum(time * amount for time, amount in cash_flows) / sum(amount for _, amount in cash_flows)
        total_cf = sum(amount for _, amount in cash_flows)
        
        # Simplified OAS approximation (not accurate, just for demonstration)
        oas_approx = ((total_cf / price) - 1) * 100 / avg_maturity
        
        logger.info(f"Calculated simplified OAS approximation: {oas_approx:.2f} bps")
        return max(0, oas_approx * 100)  # Convert to basis points
    
    def calculate_volatility_metrics(self, 
                                    historical_prices: pd.Series, 
                                    historical_volumes: Optional[pd.Series] = None,
                                    window_days: List[int] = [30, 90]) -> Dict[str, float]:
        """
        Calculate volatility and related metrics.
        
        Args:
            historical_prices: Series of historical prices
            historical_volumes: Optional series of trade volumes
            window_days: List of time windows to calculate volatility for
            
        Returns:
            Dictionary with volatility metrics
        """
        result = {}
        
        # Calculate historical price volatility for different windows
        for days in window_days:
            if len(historical_prices) >= days:
                # Calculate log returns
                log_returns = np.log(historical_prices / historical_prices.shift(1)).dropna()
                
                # Calculate volatility (annualized)
                volatility = log_returns.rolling(window=days).std().iloc[-1] * np.sqrt(252)
                result[f"historical_volatility_{days}d"] = volatility
            else:
                logger.warning(f"Not enough data for {days}-day volatility calculation")
        
        # Calculate volume-weighted price volatility if volumes are provided
        if historical_volumes is not None and len(historical_prices) == len(historical_volumes):
            # Volume-weighted volatility gives more weight to high-volume days
            volume_weight = historical_volumes / historical_volumes.sum()
            log_returns = np.log(historical_prices / historical_prices.shift(1)).dropna()
            
            if len(log_returns) == len(volume_weight):
                vw_variance = np.sum(volume_weight.iloc[1:].values * log_returns**2)
                vw_volatility = np.sqrt(vw_variance * 252)  # Annualized
                result["volume_weighted_volatility"] = vw_volatility
        
        return result
    
    def calculate_liquidity_score(self, 
                                 bid_ask_spread: float, 
                                 avg_daily_volume: float,
                                 issue_size: float,
                                 trade_count: Optional[int] = None) -> float:
        """
        Calculate liquidity score (0-100 scale).
        
        Args:
            bid_ask_spread: Current bid-ask spread in basis points
            avg_daily_volume: Average daily trading volume
            issue_size: Total issue size
            trade_count: Optional number of trades per day
            
        Returns:
            Liquidity score on a 0-100 scale (higher = more liquid)
        """
        # Convert components to scores (0-1 scale)
        
        # Bid-ask spread score (lower spread = higher score)
        max_spread = 100  # Max expected spread in basis points
        spread_score = max(0, 1 - (bid_ask_spread / max_spread))
        
        # Volume score (higher volume = higher score)
        volume_ratio = min(1, avg_daily_volume / (issue_size * 0.01))  # Cap at 1% of issue size
        volume_score = volume_ratio
        
        # Trade count score if available
        if trade_count is not None:
            max_trades = 50  # Reference point for maximum score
            trade_score = min(1, trade_count / max_trades)
        else:
            trade_score = 0.5  # Neutral if not available
        
        # Combine scores with weights
        weights = [0.5, 0.3, 0.2]  # Weights for spread, volume, trades
        combined_score = weights[0] * spread_score + weights[1] * volume_score + weights[2] * trade_score
        
        # Convert to 0-100 scale
        liquidity_score = combined_score * 100
        
        return liquidity_score
    
    def apply_all_metrics(self, security_data: pd.DataFrame) -> pd.DataFrame:
        """
        Apply all risk metric calculations to a DataFrame of securities.
        
        Args:
            security_data: DataFrame containing security data
            
        Returns:
            DataFrame with added risk metrics
        """
        result = security_data.copy()
        
        # Check if required columns exist
        required_columns = {
            'duration': ['price', 'yield_to_maturity'],
            'oas': ['price', 'has_optionality'],
            'volatility': ['historical_prices'],
            'liquidity': ['bid_ask_spread', 'avg_daily_volume', 'issue_size']
        }
        
        metrics_to_calculate = []
        for metric, columns in required_columns.items():
            if all(col in security_data.columns for col in columns):
                metrics_to_calculate.append(metric)
            else:
                missing = [col for col in columns if col not in security_data.columns]
                logger.warning(f"Skipping {metric} calculation due to missing columns: {missing}")
        
        # Apply calculations
        for idx, row in result.iterrows():
            # Duration metrics
            if 'duration' in metrics_to_calculate:
                try:
                    duration_metrics = self.calculate_duration_metrics(
                        price=row['price'],
                        yield_to_maturity=row['yield_to_maturity'],
                        maturity=row.get('years_to_maturity')
                    )
                    for key, value in duration_metrics.items():
                        result.at[idx, key] = value
                except Exception as e:
                    logger.error(f"Error calculating duration metrics: {e}")
            
            # OAS calculation
            if 'oas' in metrics_to_calculate and row.get('has_optionality', False):
                try:
                    oas = self.calculate_option_adjusted_spread(
                        price=row['price'],
                        cash_flows=[]  # Would need to extract cash flows from the data
                    )
                    result.at[idx, 'option_adjusted_spread'] = oas
                except Exception as e:
                    logger.error(f"Error calculating OAS: {e}")
            
            # Liquidity score
            if 'liquidity' in metrics_to_calculate:
                try:
                    liquidity_score = self.calculate_liquidity_score(
                        bid_ask_spread=row['bid_ask_spread'],
                        avg_daily_volume=row['avg_daily_volume'],
                        issue_size=row['issue_size'],
                        trade_count=row.get('trade_count')
                    )
                    result.at[idx, 'liquidity_score'] = liquidity_score
                except Exception as e:
                    logger.error(f"Error calculating liquidity score: {e}")
        
        # Volatility metrics (calculated on historical data series)
        if 'volatility' in metrics_to_calculate:
            # This would need to be implemented differently depending on how
            # historical price data is structured in the DataFrame
            logger.info("Volatility calculation needs historical time series data")
        
        return result 