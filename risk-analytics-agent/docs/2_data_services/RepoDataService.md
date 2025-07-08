# Repo Data Service: Requirements Specification

**Version: 1.0**

## 1. Overview & Purpose
The Repo Data Service is a specialized microservice responsible for sourcing overnight repurchase agreement (repo) rates for specific fixed-income instruments. Its sole function is to abstract the complexity of integrating with multiple internal and external data sources, providing a single, reliable endpoint for downstream systems to query repo rates.

This service is critical for calculating the cost of carry for a given security, a key input for risk and profitability analysis.

## 2. System Requirements

### 2.1. Functional Requirements
- **RDS-R-01:** The service **shall** accept an instrument's CUSIP and an `as_of_date` as input.
- **RDS-R-02:** The service **shall** implement a fallback logic to source the `specific_repo_rate_bps` for the given CUSIP, as defined in the methodology.
- **RDS-R-03:** The service **shall** source the appropriate `gc_repo_rate_bps` for the instrument's asset class and calculate the `cost_of_carry_bps`.
- **RDS-R-04:** The service **shall** return a structured JSON object containing the CUSIP, the `as_of_date`, and the calculated `cost_of_carry_bps`.

## 3. Data Models

### 3.1. Input Model
```json
{
  "cusip": "string",
  "as_of_date": "date",
  "cost_of_carry_bps": "float"
}
```

### 3.2. Output Model
```json
{
  "cusip": "string",
  "as_of_date": "date",
  "specific_repo_rate_bps": "float",
  "gc_repo_rate_bps": "float"
}
```

## 4. Methodology: Data Sourcing

- **RDS-R-05:** The system **shall** be capable of integrating with internal firm systems and third-party data vendors via API. Specific vendor endpoints and authentication details **shall** be externally configurable.

### 4.1. `specific_repo_rate_bps` Sourcing Logic
The service **shall** attempt to source the specific repo rate in the following priority order:
1.  **Primary Source (Internal):** Query the internal data feed from the firm's own repo trading desk. This is the most accurate source for rates on securities the firm actively finances.
2.  **Secondary Source (External):** If the internal feed does not return a rate, query a specialized data vendor or trading platform that provides specific repo indications (e.g., **BrokerTec, MTS Markets**).

### 4.2. `gc_repo_rate_bps` Sourcing Logic
The service **shall** source the general collateral repo rate from a major financial data vendor providing benchmark rates (e.g., **Bloomberg, Refinitiv**). The appropriate GC rate (e.g., Treasury GC, Agency GC) should be selected based on the instrument's asset class.

### 4.3. Cost of Carry
- **Purpose (Trader View):** To determine the financing cost of holding the instrument in inventory. A high positive value indicates the bond is "on special" and expensive to borrow/finance, creating a drag on returns.
- **Input Model:** `RepoData`
- **Logic:**
    1.  **Formula:** `cost_of_carry_bps = specific_repo_rate_bps - gc_repo_rate_bps`
    2.  **Output:** `cost_of_carry_bps`


## 5. Non-Functional Requirements
- **NFR-RDS-01 (Configurability):** All data source endpoints, API keys, and fallback logic parameters **shall** be externally configurable.
- **NFR-RDS-02 (Latency):** The service should respond within an acceptable latency threshold, potentially utilizing caching for frequently requested GC rates. 