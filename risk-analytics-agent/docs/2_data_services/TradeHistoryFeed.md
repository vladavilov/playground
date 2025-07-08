# Trade History Service – Business Requirements Document

## Part 1: Business & Functional Requirements

This section outlines the high-level business objectives and core functional capabilities of the Trade History Service.

### 1. Overview & Purpose

The Trade History Service provides a unified capability to retrieve, store, and serve historical trade data for fixed-income instruments.

Its primary business goals are:

* Retrieve trade records from the **Caesar Oracle Database**.
* Maintain a local record of historical and incremental trade data.
* Deliver trade information in both machine-readable JSON and CSV formats.
* Ensure a full audit trail of any changes to maintain data integrity and compliance.

---

### 2. Core Functional Requirements

* **TH-FR-01:** The system **shall** support historical bulk loading of trades for a specified date range.
* **TH-FR-02:** The system **shall** support incremental daily updates.
* **TH-FR-03:** The system **shall** enable retrieval of recent trades by instrument and date.
* **TH-FR-04:** The system **shall** maintain an audit log of all inserts, updates, and deletions.
* **TH-FR-05:** The system **shall** provide trade records as JSON or CSV depending on the mode of operation.

---

## Part 2: System Interfaces & Data Models

This section defines data sourcing and the structure of inputs and outputs.

### 3. Input Data: Oracle Trade Table

#### 3.1 Data Sourcing

* **TH-DS-01:** Data **shall** be extracted from the `trades` table in the Caesar Oracle Database.
* **TH-DS-02:** Selection criteria:

  * Use the latest `trade_sequence_num` for each `trade_num`.
  * `price_code` must equal `'P'`.
  * `cxl_correct_ind` must not be 1 or 2.

#### 3.2 Data Mapping

| Target Field          | Source Field     | Description                           |
| --------------------- | ---------------- | ------------------------------------- |
| `instr_id`            | `instrument_id`  | Instrument identifier (CUSIP or ISIN) |
| `trade_datetime`      | `execution_time` | Trade execution timestamp             |
| `price`               | `trade_price`    | Execution price                       |
| `par_volume`          | `par_amount`     | Trade par amount                      |
| `dealer_id`           | `trader_uuname`  | Dealer identifier                     |
| `counterparty_type`   | Derived          | See Counterparty Classification logic |
| `trade_size_category` | Derived          | See Trade Size Classification logic   |
| `load_date`           | Derived          | Date the record was loaded            |

---

## Part 3: Core Logic & Methodology

This section details the business rules applied to derive classifications.

### 4. Trade Size Classification

* **TH-BR-01:** The system **shall** classify trades into categories using the following logic:

```math
\text{TradeCategory} = 
\begin{cases}
\text{BLOCK} & \text{if } \text{ALLOCATION\_TYPE} = 'B' \\
\text{ODD\_LOT} & \text{if } \left( \frac{1000 \times \text{price}}{100} \times \text{par\_volume} \right) < 100{,}000 \\
\text{ROUND\_LOT} & \text{otherwise}
\end{cases}
```

* **TH-BR-02:** Clarification is required whether `price` in the calculation is denominated in thousands USD.

---

### 5. Counterparty Classification

* **TH-BR-03:** The system **shall** derive `counterparty_type` as follows:

```math
\text{Counterparty} = 
\begin{cases}
\text{CUSTOMER BUY} & \text{if } \text{buy\_sell\_ind} = 'S' \\
\text{CUSTOMER SELL} & \text{if } \text{buy\_sell\_ind} = 'B' \\
\text{INTER\_DEALER} & \text{mapping TBD}
\end{cases}
```

* **TH-BR-04:** The mapping of `INTER_DEALER` trades **shall** be confirmed with the business analyst.

---

## Part 4: Modes of Operation

The service **shall** support the following three operational modes:

1. **Historical Mode**

   * **Purpose:** Load all trades over a specified historical date range.
   * **Input:** `start_date` and `end_date`.
   * **Output:** CSV file containing the full set of trades for the period.

2. **Incremental Update Mode**

   * **Purpose:** Load trades for a single specified date (e.g., daily load).
   * **Input:** `input_date`.
   * **Output:** CSV file with the incremental new trades.

3. **Trade Retrieval Mode**

   * **Purpose:** Retrieve trades for a specific instrument over a recent time window.
   * **Input:** `instrument` and `input_date`.
   * **Output:** JSON payload in the following format:

```json
{
  "trades": [
    {
      "instrument_id": "string",
      "trade_datetime": "datetime",
      "price": "float",
      "par_volume": "float",
      "dealer_id": "string",
      "counterparty_type": "string",
      "trade_size_category": "string"
    }
  ]
}
```

---

## Part 5: Non-Functional & Operational Requirements

* **TH-NFR-01:** The system **shall** maintain a complete audit trail capturing all data changes, including the `update_datetime`.
* **TH-NFR-02:** Audit records **shall** mirror the main trade table schema.
* **TH-NFR-03:** All outputs (CSV or JSON) **shall** be consistent in structure and naming across modes.

---

## Part 6: Open Items & Assumptions

* **OI-01:** Confirm price denomination for trade size calculation thresholds.
* **OI-02:** Finalize mapping rules for `INTER_DEALER` counterparty type.
* **OI-03:** Validate the 25-day retrieval window for trade history lookback.
