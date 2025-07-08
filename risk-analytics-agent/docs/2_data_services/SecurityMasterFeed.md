# Security Info Service – Business Requirements Document

## Part 1: Business & Functional Requirements

This section describes the high-level business objectives and functional capabilities.

### 1. Overview & Purpose

The Security Info Service provides static reference data for fixed-income securities.

Its primary business objectives are:

* Retrieve SecurityMaster and CallSchedule data from the **Caesar Oracle Database**.
* Store historical and incremental records in a local repository for fast access.
* Serve reference data to consumers via API endpoints.
* Maintain a full audit trail of all changes for compliance and traceability.

---

### 2. Core Functional Requirements

* **SI-FR-01:** The system **shall** support historical data loads across a specified date range.
* **SI-FR-02:** The system **shall** support daily incremental updates of new or changed records.
* **SI-FR-03:** The system **shall** provide lookup of SecurityMaster and CallSchedule data by CUSIP.
* **SI-FR-04:** The system **shall** maintain audit logs recording every insert, update, or deletion.
* **SI-FR-05:** The system **shall** return results in structured JSON or CSV format depending on the operation mode.

---

## Part 2: System Interfaces & Data Models

This section defines the data sourcing and output structures.

### 3. Input Data: Oracle Reference Tables

#### 3.1 Data Sourcing

* **SI-DS-01:** Primary SecurityMaster records **shall** be retrieved from the `common_summary` table where:

  * `instr_id_type = 'C'`
  * `wma_product IN ('AGNC', 'CB', 'TRSR', 'MUNI')`
* **SI-DS-02:** CallSchedule records **shall** be retrieved from `call_schedules_munis` and `call_schedules_corps`.

#### 3.2 Data Mapping – SecurityMaster

| Target Field                  | Source Field                         | Description                          |
| ----------------------------- | ------------------------------------ | ------------------------------------ |
| `cusip`                       | `instr_id`                           | Security identifier                  |
| `instrument_type`             | Derived from `wma_product`           | Instrument classification            |
| `issuer_name`                 | `q_isr_name`                         | Issuer name                          |
| `coupon_rate`                 | `qs_coupon_rate`                     | Annual coupon rate                   |
| `maturity_date`               | `qs_matur_date`                      | Maturity datetime                    |
| `payment_frequency`           | Joined via `interest_pay_freq_types` | Coupons per year                     |
| `face_value`                  | `am_matur_denom`                     | Face/par value                       |
| `sector`                      | `pt_purpose_subclass_desc`           | MUNI-specific sector classification  |
| `rating_institution`          | Derived from composite RA_ columns   | Rating agency (Moody's, S\&P, Fitch) |
| `rating_value`                | Derived from composite RA_ columns   | Rating value                         |
| `state`                       | `qs_isr_state_code`                  | State of issuance (MUNI)             |
| `tax_status`                  | Derived from TA_ columns             | Tax treatment                        |
| `de_minimis_issue`            | no candidates (`verify with BA`)     | De minimis tax flag                  |
| `bank_qualified`              | `ta_bank_qualif_stat` = 'Y'          | Bank-qualified bond                  |
| `debt_service_coverage_ratio` | no candidates (`verify with BA`)     | Issuer DSCR                          |
| `is_dsr_covenant_breached`    | no candidates (`verify with BA`)     | DSCR covenant breach flag            |
| `load_date`                   | Derived                              | Load datetime in the local database  |

---

#### 3.3 Data Mapping – CallSchedule

| Target Field            | Source Field               | Description                               |
| ----------------------- | -------------------------- | ----------------------------------------- |
| `security_master_cusip` | join with `common_summary` | Related security identifier               |
| `call_date`             | `call_date`                | Call datetime                             |
| `call_price`            | `call_price`               | Price at which bond can be called         |
| `call_type`             | `call_type`                | Type of call (default: 'CALL' if missing) |
| `load_date`             | Derived                    | Load datetime in the local database       |

---

## Part 3: Core Logic & Methodology

This section describes business rules for transforming attributes.

---

### 4. Instrument Type Classification

* **SI-BR-01:** The `instrument_type` **shall** be derived using the following formula:

```math
\text{InstrumentType} = 
    \begin{cases}
        \text{"TFI\_AGENCY"} & \text{if } WMA\_PRODUCT = "AGNC" \\
        \text{"TFI\_TREASURY"} & \text{if } WMA\_PRODUCT = "TRSR" \\
        \text{"TFI\_CORPORATE"} & \text{if } WMA\_PRODUCT = "CB" \\
        \text{"MUNI"} & \text{if } WMA\_PRODUCT = "MUNI" \\
        \text{WMA\_PRODUCT} & \text{otherwise}
    \end{cases}
```

---

### 5. Payment Frequency Mapping

* **SI-BR-02:** The `payment_frequency` **shall** be derived by:

```math
\text{PaymentFrequency} = interest\_pay\_freq\_types.freq\_per\_year
```

where:

```math
interest\_pay\_freq\_types.int\_freq\_code = common\_summary.in\_int\_pay\_freq\_std\_period
```

---

### 6. Rating Derivation

* **SI-BR-03:** The `rating_institution` and `rating_value` **shall** be derived as follows:

```math
\text{If } RA\_MDY\_LTR \neq NULL \text{ or } RA\_MDY\_STR \neq NULL:
    \begin{cases}
        \text{rating\_institution} = "MOODYS" \\
        \text{rating\_value} = COALESCE(RA\_MDY\_LTR, RA\_MDY\_STR)
    \end{cases}
```
```math
\text{Else if } RA\_SP\_LTR \neq NULL \text{ or } RA\_SP\_STR \neq NULL:
    \begin{cases}
        \text{rating\_institution} = "S\&P" \\
        \text{rating\_value} = COALESCE(RA\_SP\_LTR, RA\_SP\_STR)
    \end{cases}
```
```math
\text{Else if } RA\_FTH\_LTR \neq NULL \text{ or } RA\_FTH\_STR \neq NULL:
    \begin{cases}
        \text{rating\_institution} = "FITCH" \\
        \text{rating\_value} = COALESCE(RA\_FTH\_LTR, RA\_FTH\_STR)
    \end{cases}
```
```math
\text{Else:}
    \begin{cases}
        \text{rating\_institution} = "NO\_RATING" \\
        \text{rating\_value} = NULL
    \end{cases}
```

---

### 7. Tax Status Classification

* **SI-BR-04:** The `tax_status` **shall** be derived using the following formula:

```math
\text{TaxStatus} =
\begin{cases}
AMT & \text{if } TA\_FED\_TAX\_STAT = "Y" \text{ and } TA\_ALT\_MIN\_TAX\_STAT = "Y" \\
AX\_EXEMPT\_FEDERAL\_AND\_STATE & \text{if } TA\_FED\_TAX\_STAT = "Y" \text{ and } TA\_ALT\_MIN\_TAX\_STAT \neq "Y" \text{ and } TA\_STATE\_TAX\_IND = "Y" \\
TAX\_EXEMPT\_FEDERAL & \text{if } TA\_FED\_TAX\_STAT = "Y" \text{ and } TA\_ALT\_MIN\_TAX\_STAT \neq "Y" \\
TAXABLE & \text{otherwise}
\end{cases}
```

---

## Part 4: Modes of Operation

The service **shall** provide three operational modes:

1. **Historical Mode**

   * **Purpose:** Load all SecurityMaster and CallSchedule records over a specified historical date range.
   * **Input:** `start_date`, `end_date`.
   * **Output:** CSV file containing all retrieved records.

2. **Incremental Update Mode**

   * **Purpose:** Load new or changed records for a specific date.
   * **Input:** `input_date`.
   * **Output:** CSV file with incremental updates.

3. **Lookup Mode**

   * **Purpose:** Retrieve current SecurityMaster and CallSchedule data for a specific CUSIP.
   * **Input:** `cusip`.
   * **Output:** JSON payload with combined instrument and call schedule information:

```json
{
  "cusip": "string",
  "instrument_type": "string",
  "issuer_name": "string",
  "coupon_rate": "float",
  "maturity_date": "datetime",
  "payment_frequency": "integer",
  "face_value": "float",
  "sector": "string",
  "rating_institution": "string",
  "rating_value": "string",
  "state": "string",
  "tax_status": "string",
  "de_minimis_issue": "boolean",
  "bank_qualified": "boolean",
  "debt_service_coverage_ratio": "float",
  "is_dsr_covenant_breached": "boolean",
  "call_schedule": [
    {
      "call_date": "datetime",
      "call_price": "float",
      "call_type": "string"
    }
  ]
}
```

---

## Part 5: Non-Functional & Operational Requirements

* **SI-NFR-01:** The system **shall** persist a complete audit trail of any inserts, updates, or deletes.
* **SI-NFR-02:** Each audit table **shall** mirror the structure of the primary tables and include an `update_datetime`.
* **SI-NFR-03:** All outputs (CSV and JSON) **shall** adhere to consistent, documented schemas.

---

## Part 6: Open Items & Assumptions

* **OI-01:** Confirm calculation source for `face_value`.
* **OI-02:** Confirm logic for `de_minimis_issue`, `debt_service_coverage_ratio`, and `is_dsr_covenant_breached` (currently noted as **no candidates**).
* **OI-03:** Validate the tax status logic with compliance stakeholders.