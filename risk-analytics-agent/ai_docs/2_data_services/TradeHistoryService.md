# Trade History Service

## 1. Overview and Purpose

The Trade History Service serves as the top-level component for retrieving trade data from the Caesar Oracle DB. Its primary roles:

- Retrieve historical trade data from an Oracle DB.
- Store results in a local PostgreSQL database for faster lookups and export to CSV.
- Allow periodic (daily) updates of new trade entries.
- Respond to HTTP requests for specified instruments over a recent trade window.
- Record all changes in audit tables that mirror the original schema with an `update_datetime`.

---

## 2. Architecture and Tech Stack

### Key Technologies

- **Oracle Database (Caesar)**: Stores the primary records for trade data. Accessed via the `oracledb` Python library.
- **PostgreSQL**: Local relational DB used to cache results and minimize Oracle queries.
- **Python 3**: Core language for service logic and database integration.
- **FastAPI**: High-performance API framework for Python with automatic OpenAPI documentation.
- **SQLAlchemy**: ORM used for PostgreSQL interaction.
- **oracledb (Python Library)**: Official Oracle database driver for Python.

---

## 3. Data Sources and Relational Tables

### Trade Table Schema

Main source of truth should be table `trades`, grouped by `trade_num` and with the latest `trade_sequence_num`, where `price_code` = 'P', `cxl_correct_ind` is not 1 OR 2

| Column Name          | Data Type         | Description                                                         | Source    | CSV Export |
|----------------------|-------------------|---------------------------------------------------------------------|-----------|------------|
| id                   | SERIAL PK         | Unique row identifier                                               | Derived   | No         |
| instrument_id        | TEXT              | Instrument identifier (e.g., CUSIP or ISIN)                         | instrument_id       | Yes        |
| trade_datetime       | TIMESTAMP         | Date/time when the trade occurred                                   | execution_time       | Yes        |
| price                | REAL              | Price at which the trade executed                                   | trade_price        | Yes        |
| par_volume           | REAL              | Nominal/par volume of the trade                                     | par_amount       | Yes        |
| dealer_id            | TEXT              | Identifier for the dealer executing the trade                       | trader_uuname       | Yes        |
| counterparty_type    | TEXT              | 'CUSTOMER BUY', 'CUSTOMER SELL', or 'INTER_DEALER'                  | define_counterparty()       | Yes        |
| trade_size_category  | TEXT              | 'BLOCK', 'ROUND_LOT' ($100k–$999k), or 'ODD_LOT' (<$100k)   | define_trade_category() | Yes        |
| load_date            | DATE              | Date when the record was loaded                                     | Derived   | No         |

```math
\text{TradeCategory} = 
\begin{cases}
\text{BLOCK} & \text{if } \text{ALLOCATION\_TYPE} = 'B' \\
\text{ODD\_LOT} & \text{if } \left( \frac{1000 \times \text{price}}{100} \times \text{par\_amount} \right) < 100{,}000 \\
\text{ROUND\_LOT} & \text{otherwise}
\end{cases}
```
```python
# Need to clarify price with BA - is it 1000 USD?
```

```math
\text{Counterparty} = 
\begin{cases}
\text{CUSTOMER BUY} & \text{if } \text{buy\_sell\_ind} = 'S' \\
\text{CUSTOMER SELL} & \text{if } \text{buy\_sell\_ind} = 'B' \\
\text{INTER\_DEALER} & \text{(mapping to be confirmed)}
\end{cases}
```
``` python
# 'INTER_DEALER' to be confirmed with BA
```

---

## 4. API Endpoints

### 4.1 `/historical`

**Purpose**: Load all trade data from `start_date` to `end_date` into the local DB and export to a CSV.

**Behavior**:
- Full refresh from source Oracle DB.
- Optionally generate a report-only CSV output.

**Example**:
```http
Request:
POST /historical?reportOnly=true&start_date=2018-01-01&end_date=2025-01-01
Response:
trade_history.csv
```

---

### 4.2 `/update`

**Purpose**: Load trades for a specific date (`input_date`) and append to DB and CSV.

**Behavior**:
- Incremental load for one day.
- Update audit tables with previous state.
- Optional reporting.

**Example**:
```http
Request:
POST /update?proivdeReport=true
Content-Type: application/json

{
  "input_date": "2023-01-05"
}
Response:
trade_history.csv
```

---

### 4.3 `/actual`

**Purpose**: Respond to HTTP requests for trade records based on instrument and date.

**Behavior**:
- Queries PostgreSQL for trades within a fixed window (e.g., 25 days before `input_date`).

**Example**:
```http
GET /actual?instrument=ABC123&input_date=2023-01-31
```

**Response**:
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

## 5. Audit Tables

For every insert, update, or delete on the primary trade table, a new record is written to `trade_audit`:

- Contains all columns of the base `trade` table.
- Includes `update_datetime` indicating when the change occurred.

---

## 6. Setup & Environment Guide

### Prerequisites

- **Python 3.14+**
- **PostgreSQL 17+**
- **Access to Oracle DB (Caesar)**

Install required Python libraries:
```bash
pip install oracledb fastapi uvicorn sqlalchemy psycopg2-binary
```

### Configuration

Create a `config.ini` file:

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
database=trade_history
```