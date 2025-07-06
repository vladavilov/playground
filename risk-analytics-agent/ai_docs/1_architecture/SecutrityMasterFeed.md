# Security Info Service – Technical Requirements and Architecture

## 1. Architecture Overview

**Components:**

* **Oracle Database (Caesar):** Source of truth for SecurityMaster and CallSchedule data
* **PostgreSQL Database:** Local storage and audit trail
* **Python 3** with:

  * FastAPI
  * SQLAlchemy
  * oracledb

---

## 2. Data Mapping

### SecurityMaster Table

| Target Field                  | Source Field               | Data Type | CSV Export |
| ----------------------------- | -------------------------- | --------- | ---------- |
| `cusip`                       | `instr_id`                 | TEXT      | Yes        |
| `instrument_type`             | derived                    | TEXT      | Yes        |
| `issuer_name`                 | `q_isr_name`               | TEXT      | Yes        |
| `coupon_rate`                 | `qs_coupon_rate`           | REAL      | Yes        |
| `maturity_date`               | `qs_matur_date`            | DATETIME  | Yes        |
| `payment_frequency`           | derived                    | INTEGER   | Yes        |
| `face_value`                  | `am_matur_denom`           | REAL      | Yes        |
| `sector`                      | `pt_purpose_subclass_desc` | TEXT      | Yes        |
| `rating_institution`          | derived                    | TEXT      | Yes        |
| `rating_value`                | derived                    | TEXT      | Yes        |
| `state`                       | `qs_isr_state_code`        | TEXT      | Yes        |
| `tax_status`                  | derived                    | TEXT      | Yes        |
| `de_minimis_issue`            | no candidates              | BOOLEAN   | Yes        |
| `bank_qualified`              | derived                    | BOOLEAN   | Yes        |
| `debt_service_coverage_ratio` | no candidates              | REAL      | Yes        |
| `is_dsr_covenant_breached`    | no candidates              | BOOLEAN   | Yes        |
| `load_date`                   | derived                    | DATETIME  | No         |

---

### CallSchedule Table

| Target Field            | Source Field          | Data Type | CSV Export |
| ----------------------- | --------------------- | -------------------------- | ---------- |
| `security_master_cusip` | join `common_summary` | TEXT (`Composite Key`)     | Yes        |
| `call_date`             | `call_date`           | DATETIME (`Composite Key`) | Yes        |
| `call_price`            | `call_price`          | REAL                       | Yes        |
| `call_type`             | derived               | TEXT                       | Yes        |
| `load_date`             | derived               | DATETIME                   | No         |

---

## 3. Attribute Derivation Logic

**Instrument Type**

```sql
CASE
  WHEN WMA_PRODUCT = 'AGNC' THEN 'TFI_AGENCY'
  WHEN WMA_PRODUCT = 'TRSR' THEN 'TFI_TREASURY'
  WHEN WMA_PRODUCT = 'CB' THEN 'TFI_CORPORATE'
  WHEN WMA_PRODUCT = 'MUNI' THEN 'MUNI'
  ELSE WMA_PRODUCT
END
```

**Payment Frequency**

```sql
SELECT ipft.freq_per_year
FROM interest_pay_freq_types ipft
WHERE ipft.int_freq_code = common_summary.in_int_pay_freq_std_period
```

**Rating Institution and Rating Value**

```sql
CASE
  WHEN RA_MDY_LTR IS NOT NULL OR RA_MDY_STR IS NOT NULL THEN 'MOODYS'
  WHEN RA_SP_LTR IS NOT NULL OR RA_SP_STR IS NOT NULL THEN 'S&P'
  WHEN RA_FTH_LTR IS NOT NULL OR RA_FTH_STR IS NOT NULL THEN 'FITCH'
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
END AS rating_value
```

**Tax Status**

```sql
CASE
  WHEN TA_FED_TAX_STAT = 'Y' AND TA_ALT_MIN_TAX_STAT = 'Y' THEN 'AMT'
  WHEN TA_FED_TAX_STAT = 'Y' AND TA_ALT_MIN_TAX_STAT <> 'Y' AND TA_STATE_TAX_IND = 'Y' THEN 'TAX_EXEMPT_FEDERAL_AND_STATE'
  WHEN TA_FED_TAX_STAT = 'Y' AND TA_ALT_MIN_TAX_STAT <> 'Y' THEN 'TAX_EXEMPT_FEDERAL'
  ELSE 'TAXABLE'
END
```

**Bank Qualified**

```sql
CASE
  WHEN ta_bank_qualif_stat = 'Y' THEN TRUE
  ELSE FALSE
END
```

**Call Type**

```sql
CASE
  WHEN call_type IS NOT NULL THEN call_type
  ELSE 'CALL'
END
```

---

## 4. API Endpoints

### 4.1 `/historical`

**Method:** `POST`
**Input:**

* `start_date`
* `end_date`
* `reportOnly`

**Logic:**

* Retrieve SecurityMaster and CallSchedule records for the date range
* If `reportOnly=false`:

  * Insert records into PostgreSQL
  * If CUSIP primary key exists, replace the existing record
* Generate CSV output

**Example Request:**

```http
POST /historical?reportOnly=true&start_date=2018-01-01&end_date=2025-01-01
```

**Output:**

* `data.csv`

---

### 4.2 `/update`

**Method:** `POST`
**Input:**

* `input_date`
* `provideReport`

**Logic:**

* Retrieve incremental records for `input_date`
* Insert or update PostgreSQL
* If CUSIP primary key exists, replace the existing record
* Append audit records
* Generate CSV output

**Example Request:**

```http
POST /update?provideReport=true&input_date=2023-01-05
```

**Output:**

* `data.csv`

---

### 4.3 `/actual`

**Method:** `GET`
**Input:**

* `cusip`

**Logic:**

* Retrieve current SecurityMaster and related CallSchedule records for `cusip`
* Return JSON response

**Example Request:**

```http
GET /actual?cusip=9128285Q9
```

**Output Example:**

```json
{
  "cusip": "9128285Q9",
  "instrument_type": "TFI_TREASURY",
  "issuer_name": "US Treasury",
  "coupon_rate": 3.25,
  "maturity_date": "2032-11-15T00:00:00",
  "payment_frequency": 2,
  "face_value": 1000.0,
  "sector": "GENERAL_OBLIGATION",
  "rating_institution": "S&P",
  "rating_value": "AA+",
  "state": "NY",
  "tax_status": "TAX_EXEMPT_FEDERAL",
  "de_minimis_issue": false,
  "bank_qualified": true,
  "debt_service_coverage_ratio": null,
  "is_dsr_covenant_breached": null,
  "call_schedule": [
    {
      "call_date": "2028-11-15T00:00:00",
      "call_price": 102.5,
      "call_type": "CALL"
    }
  ]
}
```

## 5. Audit Trail

* All inserts, updates, deletes mirror the structure of SecurityMaster and CallSchedule tables
* Extra column: `update_datetime`

## 6. Technology Stack

* Python 3.14+
* FastAPI
* SQLAlchemy
* oracledb
* PostgreSQL 17+