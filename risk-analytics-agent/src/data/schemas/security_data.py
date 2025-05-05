"""Security Data schemas for fixed income securities.

This module defines the Pydantic models for security data used in risk analytics,
based on the data requirements specifications.
"""

from datetime import date, datetime
from enum import Enum
from typing import Dict, List, Optional, Union, Any

from pydantic import Field, validator, root_validator

from .base import BaseSchema, TimestampMixin
from .market_data import DataType


class CreditRating(str, Enum):
    """Enumeration of credit ratings."""
    
    AAA = "AAA"
    AA_PLUS = "AA+"
    AA = "AA"
    AA_MINUS = "AA-"
    A_PLUS = "A+"
    A = "A"
    A_MINUS = "A-"
    BBB_PLUS = "BBB+"
    BBB = "BBB"
    BBB_MINUS = "BBB-"
    BB_PLUS = "BB+"
    BB = "BB"
    BB_MINUS = "BB-"
    B_PLUS = "B+"
    B = "B"
    B_MINUS = "B-"
    CCC_PLUS = "CCC+"
    CCC = "CCC"
    CCC_MINUS = "CCC-"
    CC = "CC"
    C = "C"
    D = "D"
    NR = "NR"  # Not Rated


class TaxStatus(str, Enum):
    """Enumeration of tax statuses for securities."""
    
    TAX_EXEMPT = "TAX_EXEMPT"
    AMT = "AMT"  # Alternative Minimum Tax
    TAXABLE = "TAXABLE"


class CouponType(str, Enum):
    """Enumeration of coupon types."""
    
    FIXED = "FIXED"
    FLOATING = "FLOATING"
    ZERO = "ZERO"
    STEP_UP = "STEP_UP"
    INFLATION_LINKED = "INFLATION_LINKED"


class SecurityType(str, Enum):
    """Enumeration of security types for fixed income."""
    
    GO = "GO"  # General Obligation
    REVENUE = "REVENUE"
    CORPORATE = "CORPORATE"
    TREASURY = "TREASURY"
    AGENCY = "AGENCY"
    MBS = "MBS"  # Mortgage-Backed Security
    ABS = "ABS"  # Asset-Backed Security
    CMBS = "CMBS"  # Commercial Mortgage-Backed Security
    SUPRANATIONAL = "SUPRANATIONAL"
    SOVEREIGN = "SOVEREIGN"


class CallFeature(BaseSchema):
    """Model for call features of a security."""
    
    call_date: date = Field(
        ...,
        description="Date when the security can be called",
        example="2030-06-15"
    )
    
    call_price: float = Field(
        ...,
        description="Price at which the security can be called (per 100 par)",
        example=102.0,
        gt=0
    )
    
    call_type: str = Field(
        ...,
        description="Type of call feature",
        example="OPTIONAL"
    )
    
    @validator("call_date")
    def validate_call_date(cls, v: date) -> date:
        """Validate that the call date is not in the past."""
        if v < date.today():
            raise ValueError(f"Call date cannot be in the past: {v}")
        return v


class CoreAttributes(BaseSchema):
    """Model for core attributes of a security."""
    
    credit_ratings: Dict[str, str] = Field(
        ...,
        description="Credit ratings from different agencies",
        example={"MOODYS": "Aa2", "SP": "AA", "FITCH": "AA"}
    )
    
    issuer_name: str = Field(
        ...,
        description="Name of the issuer",
        example="City of New York"
    )
    
    issuer_type: str = Field(
        ...,
        description="Type of issuer",
        example="MUNICIPALITY"
    )
    
    state: Optional[str] = Field(
        None,
        description="State/geography",
        example="NY"
    )
    
    sector: str = Field(
        ...,
        description="Sector of the security",
        example="EDUCATION"
    )
    
    subsector: Optional[str] = Field(
        None,
        description="Subsector of the security",
        example="HIGHER_EDUCATION"
    )
    
    tax_status: TaxStatus = Field(
        ...,
        description="Tax status of the security",
        example="TAX_EXEMPT"
    )
    
    issue_size: float = Field(
        ...,
        description="Total size of the issue (USD)",
        example=500000000.0,
        gt=0
    )
    
    issue_date: date = Field(
        ...,
        description="Date when the security was issued",
        example="2020-06-15"
    )
    
    maturity_date: date = Field(
        ...,
        description="Date when the security matures",
        example="2035-06-15"
    )
    
    call_features: Optional[List[CallFeature]] = Field(
        None,
        description="Call features of the security"
    )
    
    coupon_rate: float = Field(
        ...,
        description="Coupon rate of the security (%)",
        example=3.5,
        ge=0
    )
    
    coupon_type: CouponType = Field(
        ...,
        description="Type of coupon",
        example="FIXED"
    )
    
    security_type: SecurityType = Field(
        ...,
        description="Type of security",
        example="REVENUE"
    )
    
    @validator("credit_ratings")
    def validate_credit_ratings(cls, v: Dict[str, str]) -> Dict[str, str]:
        """Validate credit ratings."""
        if not v:
            raise ValueError("At least one credit rating agency should be provided")
        
        valid_agencies = {"MOODYS", "SP", "FITCH"}
        for agency in v.keys():
            if agency.upper() not in valid_agencies:
                raise ValueError(f"Unknown rating agency: {agency}")
        
        return v
    
    @validator("issue_date")
    def validate_issue_date(cls, v: date, values: Dict) -> date:
        """Validate issue date."""
        if v > date.today():
            raise ValueError(f"Issue date cannot be in the future: {v}")
        return v
    
    @root_validator
    def validate_dates(cls, values: Dict) -> Dict:
        """Validate that maturity date is after issue date."""
        issue_date = values.get("issue_date")
        maturity_date = values.get("maturity_date")
        
        if issue_date and maturity_date and maturity_date <= issue_date:
            raise ValueError(f"Maturity date must be after issue date: {maturity_date} <= {issue_date}")
        
        return values


class CalculatedMetrics(BaseSchema):
    """Model for calculated metrics of a security."""
    
    modified_duration: float = Field(
        ...,
        description="Modified duration",
        example=5.2,
        ge=0
    )
    
    effective_duration: float = Field(
        ...,
        description="Effective duration (option-adjusted)",
        example=5.5,
        ge=0
    )
    
    convexity: float = Field(
        ...,
        description="Convexity",
        example=0.45,
        ge=0
    )
    
    oas: float = Field(
        ...,
        description="Option-adjusted spread (bps)",
        example=35.0
    )
    
    historical_volatility_30d: Optional[float] = Field(
        None,
        description="30-day historical volatility (%)",
        example=2.5,
        ge=0
    )
    
    historical_volatility_90d: Optional[float] = Field(
        None,
        description="90-day historical volatility (%)",
        example=3.2,
        ge=0
    )
    
    avg_daily_volume: Optional[float] = Field(
        None,
        description="Average daily trading volume (USD)",
        example=2500000.0,
        ge=0
    )
    
    bid_ask_spread: Optional[float] = Field(
        None,
        description="Bid-ask spread (%)",
        example=0.35,
        ge=0
    )
    
    liquidity_score: Optional[float] = Field(
        None,
        description="Composite liquidity score (0-100)",
        example=75.0,
        ge=0,
        le=100
    )
    
    dealer_count: Optional[int] = Field(
        None,
        description="Number of dealers making markets",
        example=8,
        ge=0
    )
    
    yield_curve_position: Optional[str] = Field(
        None,
        description="Position on the yield curve",
        example="10Y"
    )
    
    yield_ratio_to_benchmark: Optional[float] = Field(
        None,
        description="Ratio of yield to benchmark (%)",
        example=120.5,
        ge=0
    )
    
    call_option_value: Optional[float] = Field(
        None,
        description="Call option value (% of price)",
        example=0.75,
        ge=0
    )
    
    mmd_treasury_ratio: Optional[float] = Field(
        None,
        description="MMD/Treasury ratio (%)",
        example=85.0,
        ge=0
    )
    
    @validator("modified_duration", "effective_duration")
    def validate_duration(cls, v: float) -> float:
        """Validate duration is within reasonable bounds."""
        if v < 0 or v > 30:
            raise ValueError(f"Duration should be between 0 and 30: {v}")
        return v
    
    @validator("convexity")
    def validate_convexity(cls, v: float) -> float:
        """Validate convexity is within reasonable bounds."""
        if v < 0 or v > 5:
            raise ValueError(f"Convexity should be between 0 and 5: {v}")
        return v
    
    @validator("oas")
    def validate_oas(cls, v: float) -> float:
        """Validate OAS is within reasonable bounds."""
        if v < -200 or v > 2000:
            raise ValueError(f"OAS should be between -200 and 2000 bps: {v}")
        return v


class SecurityData(BaseSchema, TimestampMixin):
    """Model for security data."""
    
    data_type: DataType = Field(
        DataType.SECURITY_DATA,
        description="Type of data",
        example="SECURITY_DATA"
    )
    
    security_id: str = Field(
        ...,
        description="Security identifier (CUSIP/ISIN)",
        min_length=9,
        max_length=12,
        example="912828ZQ6"
    )
    
    core_attributes: CoreAttributes = Field(
        ...,
        description="Core attributes of the security"
    )
    
    calculated_metrics: CalculatedMetrics = Field(
        ...,
        description="Calculated metrics for the security"
    )
    
    additional_data: Optional[Dict[str, Any]] = Field(
        None,
        description="Additional security-specific data"
    )
    
    @validator("security_id")
    def validate_security_id(cls, v: str) -> str:
        """Validate security ID format."""
        if len(v) not in [9, 12]:
            raise ValueError(f"Security ID should be 9 chars (CUSIP) or 12 chars (ISIN): {v}")
        return v
    
    class Config:
        """Configuration for the SecurityData model."""
        schema_extra = {
            "example": {
                "dataType": "SECURITY_DATA",
                "timestamp": "2023-07-21T14:30:15.123Z",
                "securityId": "912828ZQ6",
                "coreAttributes": {
                    "creditRatings": {
                        "MOODYS": "Aa2",
                        "SP": "AA",
                        "FITCH": "AA"
                    },
                    "issuerName": "City of New York",
                    "issuerType": "MUNICIPALITY",
                    "state": "NY",
                    "sector": "EDUCATION",
                    "subsector": "HIGHER_EDUCATION",
                    "taxStatus": "TAX_EXEMPT",
                    "issueSize": 500000000.0,
                    "issueDate": "2020-06-15",
                    "maturityDate": "2035-06-15",
                    "callFeatures": [
                        {
                            "callDate": "2030-06-15",
                            "callPrice": 102.0,
                            "callType": "OPTIONAL"
                        }
                    ],
                    "couponRate": 3.5,
                    "couponType": "FIXED",
                    "securityType": "REVENUE"
                },
                "calculatedMetrics": {
                    "modifiedDuration": 5.2,
                    "effectiveDuration": 5.5,
                    "convexity": 0.45,
                    "oas": 35.0,
                    "historicalVolatility30d": 2.5,
                    "historicalVolatility90d": 3.2,
                    "avgDailyVolume": 2500000.0,
                    "bidAskSpread": 0.35,
                    "liquidityScore": 75.0,
                    "dealerCount": 8,
                    "yieldCurvePosition": "10Y",
                    "yieldRatioToBenchmark": 120.5,
                    "callOptionValue": 0.75,
                    "mmdTreasuryRatio": 85.0
                },
                "additionalData": {
                    "escrowProvider": "JP Morgan",
                    "useOfProceeds": "Infrastructure projects"
                }
            }
        } 