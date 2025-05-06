# Data Contracts Documentation

This document describes the data contracts for the Risk Analytics Agent system, including schemas, validation rules, and usage guidelines.

## Overview

The Risk Analytics Agent system uses structured data models defined as Pydantic schemas for all data flows. These schemas ensure data consistency, validation, and proper documentation across the system. All data exchanged between components must adhere to these contracts.

## Common Schema Patterns

All schemas in the system follow these common patterns:

1. **Base Schema**: All models extend `BaseSchema`, which provides common configuration and behavior
2. **Mixins**: Common properties are implemented as mixins (e.g., `TimestampMixin`, `SourceMetadataMixin`)
3. **Validation**: All schemas include field-level and model-level validation
4. **Documentation**: All fields have descriptions and examples
5. **Serialization**: All schemas can be serialized to/from JSON with camelCase field names

## Data Models

### Market Data

The `MarketData` schema represents market data points from the fixed income markets, typically consumed from the Kafka topic `market-data-fixed-income`.

```python
class MarketData(BaseSchema, TimestampMixin, SourceMetadataMixin):
    data_type: DataType  # Always MARKET_DATA
    asset_class: AssetClass  # Typically FIXED_INCOME
    data_points: List[MarketDataPoint]  # Collection of market data points
```

Each `MarketDataPoint` includes:
- `metric_type`: Type of metric (e.g., YIELD_CURVE, INDEX, FUND_FLOW)
- `key`: Specific key for the metric (e.g., US_TREASURY_10Y)
- `value`: Numeric value of the metric
- `unit`: Unit of measurement (e.g., PERCENT, BPS, USD)

#### Validation Rules

- Timestamp must not be in the future
- At least one data point is required
- Data points must have unique keys
- Values must be appropriate for the specified unit

### Trade Data

The `TradeData` schema represents trade history data, consumed from both the Kafka topic `trade-history-fixed-income-updates` and the REST API.

```python
class TradeData(BaseSchema):
    data_type: DataType  # Always TRADE_HISTORY
    trade_id: str  # Unique identifier for the trade
    security_id: str  # Security identifier (CUSIP/ISIN)
    execution_details: ExecutionDetails  # Details of the execution
    performance_metrics: Optional[PerformanceMetrics]  # Post-trade performance
```

The `ExecutionDetails` include:
- `timestamp`: When the trade was executed
- `price`: Execution price (per 100 par)
- `direction`: BUY or SELL
- `quantity`: Par amount of the trade
- `counterparty_type`: Type of counterparty (CUSTOMER, DEALER, INTERDEALER)
- Additional optional fields for execution context

#### Validation Rules

- Trade ID must be a UUID or at least 8 characters
- Security ID must be a valid CUSIP (9 chars) or ISIN (12 chars)
- Price must be positive and within reasonable range for fixed income (0-200)
- Quantity must be positive

### Security Data

The `SecurityData` schema represents security attributes and calculated metrics.

```python
class SecurityData(BaseSchema, TimestampMixin):
    data_type: DataType  # Always SECURITY_DATA
    security_id: str  # Security identifier (CUSIP/ISIN)
    core_attributes: CoreAttributes  # Core security attributes
    calculated_metrics: CalculatedMetrics  # Calculated metrics
    additional_data: Optional[Dict[str, Any]]  # Additional data
```

`CoreAttributes` include issuer information, credit ratings, tax status, coupon information, etc.
`CalculatedMetrics` include duration, convexity, OAS, liquidity scores, etc.

#### Validation Rules

- Security ID must be a valid CUSIP or ISIN
- Issue date must be in the past
- Maturity date must be after issue date
- Credit ratings must include at least one recognized agency

### Regime Prediction

The `RegimePrediction` schema represents market regime predictions from Model 1.

```python
class RegimePrediction(BaseSchema, TimestampMixin):
    prediction_id: str  # Unique identifier for this prediction
    current_regime: MarketRegimeType  # Current predicted regime
    regime_probabilities: Dict[MarketRegimeType, float]  # Probabilities for each regime
    confidence_score: float  # Confidence in the prediction (0-1)
    regime_indicators: List[RegimeIndicator]  # Key indicators driving the prediction
    model_version: str  # Version of the model used
    valid_until: Optional[datetime]  # When this prediction expires
    previous_regime: Optional[MarketRegimeType]  # Previous regime
    regime_transition_probability: Optional[float]  # Probability of transition
```

Market regimes include:
- CALM_NORMAL
- VOLATILITY_RISK_OFF
- LIQUIDITY_CRISIS
- CREDIT_DRIVEN_SELLOFF
- RATE_DRIVEN_SELLOFF
- STRONG_INFLOW_RALLY
- TECHNICAL_DISLOCATION

#### Validation Rules

- Regime probabilities must sum to approximately 1.0
- Current regime should be the one with highest probability
- Valid until date must be after prediction timestamp
- Confidence score must be between 0 and 1

### Risk Score

The `RiskScore` schema represents trade risk assessments from Model 2.

```python
class RiskScore(BaseSchema, TimestampMixin):
    prediction_id: str  # Unique identifier for this risk assessment
    security_id: str  # Security identifier (CUSIP/ISIN)
    trade_direction: TradeDirection  # Direction of the trade
    trade_size: float  # Size of the trade (par amount)
    risk_score: int  # Risk score (0-100, higher = more risky)
    market_regime: MarketRegimeType  # Current market regime
    risk_factors: List[RiskFactor]  # Risk factors contributing to the score
    confidence_score: float  # Confidence in the risk assessment (0-1)
    similar_scenarios: Optional[List[HistoricalScenario]]  # Similar historical scenarios
    mitigation_suggestions: Optional[List[RiskMitigation]]  # Suggested mitigations
    model_version: str  # Version of the model used
    valid_until: Optional[datetime]  # When this score expires
```

Risk factors include:
- LIQUIDITY
- CREDIT
- INTEREST_RATE
- VOLATILITY
- MOMENTUM
- TECHNICAL
- SUPPLY_DEMAND
- CONCENTRATION
- MARKET_SENTIMENT
- REGULATORY

#### Validation Rules

- Security ID must be a valid CUSIP or ISIN
- Risk score must be between 0 and 100
- At least one risk factor must be specified
- Risk factor contributions should sum to approximately 1.0
- Valid until date must be after prediction timestamp

## Data Quality Expectations

1. **Completeness**: All required fields must be present in all data models
2. **Accuracy**: Values must be within valid ranges and conform to business rules
3. **Consistency**: Related data must maintain referential integrity
4. **Timeliness**: Data should be processed within SLA timeframes

## Schema Evolution and Versioning

1. **Backward Compatibility**: New schema versions should maintain backward compatibility
2. **Field Addition**: New fields should be optional to maintain compatibility
3. **Deprecation Process**: Fields to be removed should be marked deprecated for a period
4. **Version Tracking**: Schema changes should be tracked in version control

## Usage Guidelines

1. **Validation**: Always validate data against schemas before processing
2. **Serialization**: Use schema serialization methods for consistent JSON conversion
3. **Documentation**: Reference this document when implementing new data flows
4. **Testing**: Include schema validation in test suites for all components

## Example Implementations

Example implementations for the key data models can be found in the `src/data/schemas/` directory:

- `market_data.py` - Market data schemas
- `trade_data.py` - Trade data schemas
- `security_data.py` - Security data schemas
- `regime_prediction.py` - Regime prediction schemas
- `risk_score.py` - Risk score schemas 