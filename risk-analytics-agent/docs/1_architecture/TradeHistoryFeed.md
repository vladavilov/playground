# Trade History Service – Technical Requirements and Architecture

## 1. Architecture Overview

**Components:**

* **Oracle Database (Caesar):** Source of truth for trade data
* **PostgreSQL Database:** Local storage and audit trail
* **Python 3** with:

  * FastAPI
  * SQLAlchemy
  * oracledb

---

## 2. Data Mapping

| Target Field          | Source Field     | Data Type | CSV Export |
| --------------------- | ---------------- | --------- | ---------- |
| `instr_id`            | `instrument_id`  | TEXT      | Yes        |
| `trade_datetime`      | `execution_time` | TIMESTAMP | Yes        |
| `price`               | `trade_price`    | REAL      | Yes        |
| `par_volume`          | `par_amount`     | REAL      | Yes        |
| `dealer_id`           | `trader_uuname`  | TEXT      | Yes        |
| `counterparty_type`   | Derived          | TEXT      | Yes        |
| `trade_size_category` | Derived          | TEXT      | Yes        |
| `load_date`           | Derived          | DATE      | No         |

---

## 3. Attribute Derivation Logic

**Trade Size Classification**

```sql
CASE
  WHEN ALLOCATION_TYPE = 'B' THEN 'BLOCK'
  WHEN ((1000 * trade_price / 100) * par_amount) < 100000 THEN 'ODD_LOT'
  ELSE 'ROUND_LOT'
END
```

**Counterparty Classification**

```sql
CASE
  WHEN buy_sell_ind = 'S' THEN 'CUSTOMER BUY'
  WHEN buy_sell_ind = 'B' THEN 'CUSTOMER SELL'
  ELSE 'INTER_DEALER'
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

* Retrieve all records in date range
* Insert into PostgreSQL and audit tables if `reportOnly=false`, replacing records with existing PKs
* Generate CSV

**Example Request:**

```http
POST /historical?reportOnly=true&start_date=2018-01-01&end_date=2025-01-01
```

**Output:**

* `trade_history.csv`

---

### 4.2 `/update`

**Method:** `POST`

**Input:**

* `input_date`
* `provideReport`

**Logic:**

* Retrieve all records for `input_date`
* Insert or update PostgreSQL
* Append audit records
* Generate CSV

**Example Request:**

```http
POST /update?provideReport=true&input_date=2023-01-05
```

**Output:**

* `trade_history.csv`

---

### 4.3 `/actual`

**Method:** `GET`

**Input:**

* `instrument`
* `input_date`

**Logic:**

* Retrieve trades within 25 days before `input_date`
* Return JSON

**Example Request:**

```http
GET /actual?instrument=ABC123&input_date=2023-01-31
```

**Output Example:**

```json
{
  "trades": [
    {
      "instrument_id": "ABC123",
      "trade_datetime": "2023-01-30T14:23:00",
      "price": 101.5,
      "par_volume": 250000,
      "dealer_id": "DLR001",
      "counterparty_type": "CUSTOMER BUY",
      "trade_size_category": "ROUND_LOT"
    }
  ]
}
```

---

## 5. Audit Trail

* Inserts, updates, deletes mirror `trades` table
* Extra column: `update_datetime`

---

## 6. Technology Stack

* Python 3.14+
* FastAPI
* SQLAlchemy
* oracledb
* PostgreSQL 17+