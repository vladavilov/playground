# Requirements: News Sentiment Feature

**Version: 1.0**

## 1. Overview
This document specifies the requirements for a News Sentiment feature that ingests, processes, and serves aggregated news sentiment scores for fixed-income instruments. The system comprises four services for scoring, calculation, historical processing, and on-demand API access. The primary objective is to enhance risk analysis and provide training data for predictive models.

## 2. Data Models & Schemas

### 2.1. Input Model: Raw News Article
- **Description:** The basic input to the News Scoring Service.
- **Schema:**
  - `article_text`: `string`
  - `source_name`: `string`
  - `publication_time`: `datetime`

### 2.2. Core Data Model: Enriched News Event
The standard schema for a single, enriched news article, which serves as the output of the scoring service and input for calculation services.
```json
{
  "source": "string",
  "published_at": "datetime",
  "event_type": "string",
  "entities": {
    "issuer_name": "string",
    "sector": "string",
    "cusips": ["string"]
  },
  "sentiment": {
    "score": "float",
    "magnitude": "float"
  },
  "source_credibility_tier": "string",
  "summary_excerpt": "string"
}
```

### 2.3. API Output Models

#### 2.3.1. Real-Time Sentiment Endpoint Response
```json
{
    "aggregated_sentiment_score": "float",
    "contributing_articles_count": "integer",
    "articles": ["string"]
}
```

#### 2.3.2. Historical Sentiment Endpoint Response
```json
{
    "aggregated_sentiment_score": "float",
    "contributing_articles_count": "integer"
}
```

## 3. Core Logic & Business Rules

### 3.1. Formula: Aggregated Sentiment Score (ASS)
`ASS = Σ(Sᵢ * Mᵢ * W_eventᵢ * W_timeᵢ * W_sourceᵢ) / Σ(Mᵢ * W_eventᵢ * W_timeᵢ * W_sourceᵢ)`
- **Terms:** `S`=Score, `M`=Magnitude, `W_event`=Event Type Weight, `W_time`=Time-Decay Weight, `W_source`=Source Credibility Weight.

### 3.2. Business Rule: Event Type Weights
The `event_type` field is assigned a weight (`W_event`) to reflect the asymmetric impact of market events.

| Event Type                  | Weight (`W_event`) | Description                                                                   |
| --------------------------- | ------------------ | ----------------------------------------------------------------------------- |
| `Default`                   | 20.0               | A failure to meet a debt obligation. The most severe credit event.            |
| `Bankruptcy`                | 18.0               | A legal declaration of inability to pay debts.                                |
| `Credit_Rating_Downgrade`     | 15.0               | A reduction in credit rating by a major agency. The impact is severe and often triggers forced selling. |
| `Bond_Insurer_Downgrade`      | 12.0               | A downgrade of a major bond insurer, affecting the perceived safety of insured securities. |
| `Central_Bank_Policy`       | 9.0                | Announcements related to monetary policy (e.g., interest rate changes).       |
| `State_Budget_Crisis`         | 9.0                | For MUNI bonds, a significant state budget shortfall or fiscal emergency. |
| `Natural_Disaster_Impact`     | 8.0                | For MUNI bonds, a major natural disaster impacting the tax base or a specific project's revenue stream. |
| `Credit_Rating_Upgrade`       | 5.0                | An improvement in credit rating. The impact is less pronounced and often priced in earlier than downgrades. |
| `M&A_Announced`             | 7.0                | Announcement of a merger or acquisition. **Note:** The actual sentiment score `S` must be nuanced, as a debt-financed M&A is negative for bondholders, while an acquisition by a stronger credit is positive. |
| `Pension_Funding_Status_Change` | 7.0              | For MUNI bonds, significant news about the underfunding or improved funding of public pension liabilities. |
| `Credit_Outlook_Negative`   | 6.0                | A rating agency's indication that a future downgrade is possible.             |
| `Regulatory_Investigation`  | 6.0                | An official investigation into an issuer's practices.                         |
| `Macro_Economic_Data`       | 6.0                | Major economic releases (e.g., CPI, GDP, Jobs Report).                        |
| `Guidance_Change`           | 5.0                | A change in the company's forward-looking financial guidance.                 |
| `Lawsuit_Filed`             | 5.0                | A significant lawsuit filed against the issuer. Impact can vary and `S` must reflect the severity. |
| `Credit_Outlook_Positive`   | 4.0                | An indication that a future upgrade is possible.                              |
| `Earnings_Miss`             | 4.0                | Reported earnings that are below analyst expectations.                        |
| `Earnings_Beat`             | 3.0                | Reported earnings that are above analyst expectations.                        |
| `Executive_Change`          | 2.0                | A change in a key executive position (e.g., CEO, CFO).                      |
| `General_News`              | 1.0                | A baseline weight for news that does not fit into a more specific category.   |

### 3.3. Business Rule: Source Credibility Weights
The `W_source` weight is determined by the news source tier.

| Tier                      | Weight (`W_source`) | Examples                                             |
| ------------------------- | ------------------- | ---------------------------------------------------- |
| `TIER_1_REGULATOR`        | 1.5                 | SEC, Moody's, S&P                                    |
| `TIER_2_PREMIUM_FINANCIAL`| 1.2                 | Bloomberg, Wall Street Journal                       |
| `TIER_3_GENERAL_FINANCIAL`| 1.0                 | Other reputable financial news sources               |
| `TIER_4_COMPANY_PR`       | 0.8                 | Official press releases from the issuer              |
| `TIER_5_SYNDICATED`       | 0.6                 | Syndicated news wires, less reputable sources        |

### 3.4. Business Rule: Time-Decay Weight (`W_time`)
- **BR-01:** For **`REALTIME`** mode calculations, the time-decay weight (`W_time`) **shall** be calculated with `Δt` relative to the current time.
- **BR-02:** For **`HISTORICAL`** mode calculations, the time-decay weight (`W_time`) **shall** be calculated with `Δt` relative to the end of the specified date (23:59:59).

## 4. Functional Requirements

### 4.1. Component 1: News Scoring Service
- **NSS-FR-01:** The service **shall** accept a raw news article (text, source, publication time).
- **NSS-FR-02:** The service **shall** extract and assign `issuer_name`, `sector`, and `cusips` based on the following logic:
    - **a.** For articles directly related to a specific entity, the service **shall** extract the `issuer_name`, `sector`, or associated `cusips`.
    - **b.** For articles with broad, market-wide impact not tied to a specific issuer (e.g., central bank policy changes), the `sector` **shall** be set to `'global_market'`. `issuer_name` and `cusips` **shall** be `null`.
    - **c.** If no specific entities can be extracted and the article does not qualify as `'global_market'`, the `sector` **shall** be set to `'global_other'`. `issuer_name` and `cusips` **shall** be `null`.
- **NSS-FR-03:** The service **shall** classify the article with an `event_type` from the list in Section 3.2.
- **NSS-FR-04:** The service **shall** generate a sentiment `score` and `magnitude`.
- **NSS-FR-05:** The service **shall** generate a `summary_excerpt`.
- **NSS-FR-06:** The service **shall** classify the news `source` into a `source_credibility_tier` from Section 3.3.
- **NSS-FR-07:** The service **shall** return a single, populated `Enriched News Event` object.

### 4.2. Component 2: On-Demand Sentiment Service (Public API)
- **SCS-FR-01:** The service **shall** provide a "Real-Time Sentiment Endpoint" for on-demand calculations with the following contract:
    - **Request:** `{ "cusip": "string", "sector": "string", "issuer_name": "string" }`
    - **Response:** The `Real-Time Sentiment Endpoint Response` as defined in section 2.3.1.
- **SCS-FR-02:** The service **shall** provide a "Historical Sentiment Endpoint" for on-demand calculations with the following contract:
    - **Request:** `{ "as_of_date": "date", "cusip": "string (optional)", "issuer_name": "string (optional)", "sector": "string (optional)" }`
    - **Response:** The `Historical Sentiment Endpoint Response` as defined in section 2.3.2.
- **SCS-FR-03:** Upon receiving a request, the service **shall** retrieve all relevant `Enriched News Event` objects from the "Enriched News Store" based on the request parameters.
- **SCS-FR-04:** The service **shall** calculate the Aggregated Sentiment Score (ASS) using the formula in Section 3.1.
- **SCS-FR-05:** The service **shall** determine the calculation mode (`REALTIME` or `HISTORICAL`) from the endpoint being called and calculate the time-decay weight (`W_time`) as per rules BR-01 and BR-02.

### 4.3. Component 3: Historical Processing Service
- **HPS-FR-01:** The service **shall** iterate through a multi-year news archive.
- **HPS-FR-02:** For each article, the service **shall** invoke the News Scoring Service.
- **HPS-FR-03:** The service **shall** store every `Enriched News Event` object with a sector other than `'global_other'` in a permanent "Enriched News Store".
- **HPS-FR-03a:** The service **shall** filter and store all `Enriched News Event` objects where the sector is `'global_other'` in a separate "Audit News Store" for review.
- **HPS-FR-05:** The service **shall** store the resulting daily scores in a permanent "Historical Sentiment Store".

### 4.4. Component 4: Real-Time News Ingestion Service
- **RTN-FR-01:** The service **shall** utilize an internal timer to periodically trigger the ingestion of new articles from external sources.
- **RTN-FR-02:** For each new article, the service **shall** invoke the News Scoring Service (Component 1) to produce an `Enriched News Event`.
- **RTN-FR-03:** The service **shall** route the resulting `Enriched News Event` for persistence according to the following rules:
    - **a.** Events where the `sector` is `'global_other'` **shall** be stored in the "Audit News Store".
    - **b.** All other events **shall** be stored in the "Enriched News Store".

## 5. Non-Functional Requirements

### 5.1. Model Quality
- **NFR-MQ-01 (Critical):** The sentiment analysis model **must** be fine-tuned for credit risk, differentiating between events that are positive for equity but negative for credit (e.g., debt-financed M&A).

### 5.2. Data Consistency
- **NFR-DC-01:** Historical scores for a given day **must** be immutable and reproducible, calculated using all news available up to that day's cutoff (23:59:59).