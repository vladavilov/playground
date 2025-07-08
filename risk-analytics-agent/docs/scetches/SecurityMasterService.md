# Security Info Service

## 1. Overview and Purpose

The Security Info Service is responsible for retrieving and serving static reference data about fixed-income instruments from the Caesar Oracle Database.

- Retrieve historical reference data.
- Store records in a local PostgreSQL database and export to CSV format.
- Provide daily incremental updates for newly added or modified data.
- Serve on-demand SecurityMaster data through a RESTful API by CUSIP.

---

## 2. Architecture and Tech Stack

### Key Technologies

- **Oracle Database (Caesar)**: Source system for instrument reference data.
- **PostgreSQL**: Relational database for storing SecurityMaster and CallSchedule records.
- **Python 3**: Core language for service logic, ETL, and API development.
- **FastAPI**: Lightweight, modern web framework for building APIs, with built-in OpenAPI docs.
- **SQLAlchemy**: ORM layer for PostgreSQL database interaction.
- **oracledb**: Python library for Oracle DB connectivity.

---

## 3. Data Model and Schema

### 3.1 SecurityMaster Table

Main source of truth is the table `common_summary` where `instr_id_type = 'C'` and `wma_product` in ('AGNC', 'CB', 'TRSR', 'MUNI')

| Column Name        | Data Type | Description | Source | CSV Export |
|--------------------|-----------|-----------------------------------------------------------------------------|--------|------------|
| cusip              | TEXT (PK) | Unique identifier for the security | instr_id | YES |
| instrument_type    | TEXT      | Classification: 'MUNI', 'TFI_CORPORATE', 'TFI_TREASURY', 'TFI_AGENCY' | updated wa_product | YES |
| issuer_name        | TEXT      | Name of the issuing entity | q_isr_name | YES |
| coupon_rate        | REAL      | Annual coupon rate | qs_coupon_rate | YES |
| maturity_date      | DATE      | Date the instrument matures | qs_matur_date | YES |
| payment_frequency  | INTEGER   | Number of coupon payments per year | JOIN interest_pay_freq_types | YES |
| face_value         | REAL      | Face/par value of the instrument | am_matur_denom `(verify with BA)` | YES |
| sector             | TEXT      | For MUNI: 'GENERAL_OBLIGATION', 'REVENUE_TRANSPORTATION', etc. | pt_purpose_subclass_desc | YES |
| rating_institution             | TEXT      |  Moody’s, S&P, Fetch, or 'NO_RATING' | composite `RA_` columns | YES |
| rating_value             | TEXT      |  Rating value | composite `RA_` columns | YES |
| state              | TEXT      | U.S. state of issuance (only for MUNI) | qs_isr_state_code | YES |
| tax_status         | TEXT      | Tax treatment: 'TAX_EXEMPT_FEDERAL', 'TAXABLE', 'AMT', etc. | composite 'TA_' columns | YES |
| de_minimis_issue         | BOOLEAN      | Indicates if the bond is subject to the de minims tax rule | no candidates (`confirm with BA`) | YES |
| bank_qualified     | BOOL      | "Bank qualified" under the Tax Equity and Fiscal Responsibility Act of 1982 | ta_bank_qualif_stat = 'Y' | YES |
| debt_service_coverage_ratio    | REAL| Issuer's DSCR | no candidates (`confirm with BA`) | YES |
| is_dsr_covenant_breached       | BOOL| The issuer has breached a Debt Service Coverage ratio covenant | no candidates (`confirm with BA`) | YES |
| load_date          | DATE      | Date when the record was loaded into the local DB | Derived | NO |

```sql
-- instrument_type
SELECT
  CASE
    WHEN WMA_PRODUCT = 'AGNC' THEN 'TFI_AGENCY'
    WHEN WMA_PRODUCT = 'TRSR' THEN 'TFI_TREASURY'
    WHEN WMA_PRODUCT = 'CB' THEN 'TFI_CORPORATE'
    WHEN WMA_PRODUCT = 'MUNI' THEN 'MUNI'
    ELSE WMA_PRODUCT
  END AS instrument_type
FROM
  common_summary;
```

```sql
-- payment_frequency
SELECT
  ipft.freq_per_year as payment_frequency
FROM
  common_summary
LEFT JOIN interest_pay_freq_types AS ipft
  ON ipft.int_freq_code = common_summary.in_int_pay_freq_std_period
```

```sql
-- rating
SELECT
  CASE
    WHEN RA_MDY_LTR IS NOT NULL THEN 'MOODYS'
    WHEN RA_MDY_STR IS NOT NULL THEN 'MOODYS'
    WHEN RA_SP_LTR IS NOT NULL THEN 'S&P'
    WHEN RA_SP_STR IS NOT NULL THEN 'S&P'
    WHEN RA_FTH_LTR IS NOT NULL THEN 'FITCH'
    WHEN RA_FTH_STR IS NOT NULL THEN 'FITCH'
    ELSE 'NO_RATING'
  END AS rating_institution,
  CASE
    WHEN RA_MDY_LTR IS NOT NULL THEN RA_MDY_LTR
    WHEN RA_MDY_STR IS NOT NULL THEN RA_MDY_STR
    WHEN RA_SP_LTR IS NOT NULL THEN RA_SP_LTR
    WHEN RA_SP_STR IS NOT NULL THEN RA_SP_STR
    WHEN RA_FTH_LTR IS NOT NULL THEN RA_FTH_LTR
    WHEN RA_FTH_STR IS NOT NULL THEN RA_FTH_STR
    ELSE NULL
  END AS rating_value,
FROM common_summary;
```

```sql
-- tax_status
SELECT
  CASE
    WHEN TA_FED_TAX_STAT = 'Y' AND TA_ALT_MIN_TAX_STAT = 'Y'
      THEN 'AMT'
    WHEN TA_FED_TAX_STAT = 'Y' AND TA_ALT_MIN_TAX_STAT <> 'Y' AND TA_STATE_TAX_IND = 'Y'
      THEN 'TAX_EXEMPT_FEDERAL_AND_STATE'
    WHEN TA_FED_TAX_STAT = 'Y' AND TA_ALT_MIN_TAX_STAT <> 'Y'
      THEN 'TAX_EXEMPT_FEDERAL'
    ELSE 'TAXABLE'
  END AS TAX_STATUS
FROM common_summary;
```

### 3.2 CallSchedule Table

Source: `call_schedules_munis`, `call_schedules_corps` 

| Column Name             | Data Type           | Description | Source    | CSV Export |
|-------------------------|---------------------|-------------------------------------------------------------|-----------|------------|
| security_master_cusip   | TEXT (Composite Key) | FK SecurityMaster.cusip | join common_summary | Yes |
| call_date               | DATE (Composite Key) | Date the call option can be exercised | call_date | Yes |
| call_price              | REAL                 | Call price | call_price | Yes |
| call_type               | TEXT                 | 'CALL' - regular, 'CALV' - atcompound accreted value | call_type, 'CALL' if not present | Yes |
| load_date               | DATE                 | Load date, Autogenerated | DB | No |

```sql
-- instrument_id
select 
  common_summary.instr_id 
from 
  call_schedules_*.isid 
join 
  common_summary.isid
on 
  call_schedules_*.isid = common_summary.isid;
```

## 4. API Endpoints

### 4.1 `/historical`

**Purpose**: Retrieve and load all historical SecurityMaster and CallSchedule data from `start_date` to `end_date`.

- Inserts full data into PostgreSQL database if `reportOnly=false`.
- Writes export to CSV.

**Example**:
```http
Request:
POST /historical?reportOnly=true&start_date=2018-01-01&end_date=2025-01-01
Response:
data.csv
```

### 4.2 `/update`

**Purpose**: Perform an incremental update for the specified `input_date`.

- Adds new records or updates changed records for a given date.
- Provides report if `proivdeReport=true`.

**Example**:
```http
Request:
POST /update?proivdeReport=true&input_date=2023-01-05
Content-Type: application/json
Response:
data.csv
```

### 4.3 `/actual`

**Purpose**: Retrieve current SecurityMaster and CallSchedule data for a given CUSIP.

- Always includes:
  - Matching SecurityMaster record
  - Associated CallSchedule records

**Example**:
```http
GET /actual?cusip=9128285Q9
```

**Response**:
```json
{
  "cusip": "9128285Q9",
  "instrument_type": "TFI_TREASURY",
  "issuer_name": "US Treasury",
  "coupon_rate": 3.25,
  "maturity_date": "2032-11-15",
  "call_schedule": []
}
```

---

## 5. Audit tables

As soon as any row of data is inserted, changed or deleted, a new state is saved to `table_name_audit` containing the same columns as `table_name`, plus `update_datetime`.

---

## 6. Setup & Environment Guide

### Prerequisites

- **Python 3.14+**
- **PostgreSQL 17+** installed and running.
- **Access to Caesar Oracle DB**
- Install Python dependencies:

```bash
pip install oracledb fastapi uvicorn sqlalchemy psycopg2-binary
```

### Configuration

Create a `config.ini` file with:

```ini
[ORACLE]
username=your_oracle_username
password=your_oracle_password
dsn=your_oracle_dsn

[POSTGRES]
host=localhost
port=5432
user=postgres
password=your_pg_password
database=security_info
```
