# Covenant Extraction Service: Requirements Specification

**Version: 1.0**

## 1. Overview & Purpose
The Covenant Extraction Service is a specialized microservice responsible for identifying and extracting predefined, high-impact contractual covenants from unstructured legal documents, specifically bond prospectuses and offering circulars.

This service isolates the complex Natural Language Processing (NLP) task from the core numerical calculations performed by other financial engines. Its single responsibility is to take raw legal text and return a structured list of present covenants, which can then be ingested by downstream systems like the `Risk Synthesis` service for analysis and display.

## 2. System Requirements

### 2.1. Functional Requirements
- **CES-R-01:** The service **shall** accept a CUSIP and the full, unstructured text of the corresponding bond prospectus as input.
- **CES-R-02:** The service **shall** process the input text to identify the presence of a predefined set of key financial covenants.
- **CES-R-03:** The service **shall** return a structured JSON object containing the CUSIP and an array of strings, where each string is the name of a key covenant found in the text.
- **CES-R-04:** If no key covenants from the predefined list are found, the service **shall** return an empty array.

## 3. Data Models

### 3.1. Input Model
```json
{
  "cusip": "string",
  "prospectus_text": "string"
}
```

### 3.2. Output Model
```json
{
  "cusip": "string",
  "key_covenants": ["string"] // e.g., ["Make-Whole Call", "Sinking Fund"]
}
```

## 4. Methodology: Covenant Identification

### 4.1. Extraction Logic
- **CES-R-05:** The system **shall** scan the `prospectus_text` for keywords and phrases related to high-impact covenants. An NLP-based entity recognition model is the recommended implementation approach for robust identification.
- **CES-R-06:** The predefined list of covenants to search for **shall** be externally configurable, but at a minimum, it must include the concepts listed below. The system should be able to recognize various phrasings for the same concept.

### 4.2. Target Covenant Dictionary
The following table outlines the target covenants and the keywords/phrases the system should be designed to identify.

| Covenant Name | Keywords / Phrases to Detect |
|:---|:---|
| Make-Whole Call | "make-whole call", "make whole", "MWC" |
| Sinking Fund | "sinking fund", "sinking fund redemption" |
| Change of Control | "change of control", "put upon change of control" |
| Asset Sale Proceeds | "asset sale", "disposition of assets", "net proceeds" |
| Debt Incurrence Test | "limitation on indebtedness", "debt incurrence", "incur additional debt" |
| Negative Pledge | "negative pledge", "limitation on liens", "no liens" |
| Cross-Default | "cross-default", "cross-acceleration" |

## 5. Non-Functional Requirements
- **NFR-CES-01 (Extensibility):** The list of target covenants and their associated keywords **shall** be easily updatable without requiring a full service redeployment.
- **NFR-CES-02 (Accuracy):** The NLP model should be benchmarked for high precision and recall to minimize both false positives and false negatives. 