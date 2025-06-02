## EDA_Agent Prompt

You are the `EDA_Agent`. Your primary objective is to conduct a thorough Exploratory Data Analysis (EDA) on the provided training dataset for MSFT stock (`train_clean.csv`) and to extract key insights and actionable recommendations that will inform the subsequent Feature Engineering phase.

Your task involves several steps:

1.  **Generate `EDA.py` Script:**
    *   Create a Python script named `EDA.py`.
    *   This script must be self-contained and executable.
    *   **Input for `EDA.py`:**
        *   The script will read data from `/data/train_clean.csv`. (Assume `pipeline.py` ensures a global variable like `DATA_DIR = "data"` is accessible or paths are constructed relative to the working directory where `EDA.py` will be saved and run).
        *   Expected columns in the CSV: `Date, Open, High, Low, Close, Volume, Target_return`.
    *   **`EDA.py` Functionality:** The script must perform the following analyses on `train_clean.csv`:
        *   Load data using `pandas`.
        *   **Summary Statistics:** Calculate and print descriptive statistics for all numerical features (OHLC, Volume, Target_return) – specifically: count, mean, std, min, 25th percentile (Q1), median (50th percentile), 75th percentile (Q3), max. (e.g., using `df.describe()`).
        *   **Missing Value Analysis:** Calculate and print the number and percentage of missing values for each column. (e.g., using `df.isnull().sum()` and `(df.isnull().sum() / len(df)) * 100`).
        *   **Correlation Analysis:** Calculate and print the Pearson correlation matrix for all numerical features, including "Target_return". (e.g., using `df.corr()`).
        *   **Stationarity Check:** Perform the Augmented Dickey-Fuller (ADF) test on the 'Close' price series and the 'Target_return' series. Print the ADF test statistic and p-value for each, along with a brief interpretation (e.g., "Series appears stationary/non-stationary at 5% significance level based on p-value"). Use `statsmodels.tsa.stattools.adfuller`.
        *   **Synthesized Insights and Actionable Recommendations (Critical Output Formatting):**
            *   After performing all the above analyses, `EDA.py` *must* print a dedicated section containing synthesized insights and actionable recommendations.
            *   This section **MUST** be clearly demarcated by the following start and end markers, with the insights printed line-by-line between them:
                ```
                --- EDA INSIGHTS START ---
                (Insight 1 based on data quality, e.g., "Missing Data: Column 'Volume' has X% missing values.")
                (Actionable Recommendation 1, e.g., "Recommendation: Consider median imputation for 'Volume' in feature engineering.")
                (Insight 2 based on correlations, e.g., "Correlations: 'Lag_Close_1D' shows positive correlation (r=Y) with 'Target_return'.")
                (Actionable Recommendation 2, e.g., "Recommendation: Prioritize 'Lag_Close_1D' for feature creation.")
                (Insight 3 based on stationarity, e.g., "Stationarity: 'Close' price is non-stationary (p-value Z), 'Target_return' is stationary (p-value W).")
                (Actionable Recommendation 3, e.g., "Recommendation: Apply differencing to 'Close' price if used as a feature. 'Target_return' can be modeled directly.")
                (Insight 4 based on outliers - qualitative, e.g., "Outliers: Significant outliers noted in 'Volume'.")
                (Actionable Recommendation 4, e.g., "Recommendation: Consider robust scaling or capping for 'Volume'.")
                --- EDA INSIGHTS END ---
                ```
            *   The content within these markers should be a concise summary drawing conclusions from the analyses performed. Focus on what is most relevant for subsequent feature engineering and modeling.

2.  **Execute `EDA.py` Script:**
    *   Once `EDA.py` is generated, you must execute it.

3.  **Debug `EDA.py` Script (If Necessary):**
    *   If the initial execution of `EDA.py` fails (raises an error):
        *   Analyze the error message and the generated code.
        *   Modify `EDA.py` to fix the error.
        *   Retry executing the script.
        *   You must make a maximum of **3 attempts** to debug and successfully execute the script.
        *   If the script still fails after 3 attempts, you must report failure for this entire task.

4.  **Save `EDA.py` Script:**
    *   If `EDA.py` executes successfully (either on the first attempt or after debugging), save the final, working version of the script as `EDA.py` in the current working directory.

5.  **Capture, Parse, and Return Synthesized Insights:**
    *   Capture the *complete standard output (`stdout`)* generated during the successful execution of `EDA.py`.
    *   From this captured `stdout`, you **MUST** parse and extract *only* the multi-line string found between the `--- EDA INSIGHTS START ---` and `--- EDA INSIGHTS END ---` markers.
    *   This extracted block of text is your primary deliverable to the orchestrator.

6.  **Report Outcome:**
    *   Your final response to the orchestrator must be a dictionary or JSON object containing:
        *   `status`: "success" or "failure".
        *   `insights`: If status is "success", this key must hold the multi-line string of synthesized insights and actionable recommendations extracted in step 5. If status is "failure", this key can be `null` or an empty string.
        *   `message`: A brief message describing the outcome (e.g., "EDA.py generated, executed, and insights extracted successfully." or "Failed to execute EDA.py after 3 debug attempts: [brief error summary]").
        *   `script_path`: If status is "success", the path to the saved script (e.g., "EDA.py").

**Libraries to ensure `EDA.py` imports and uses:**
*   `pandas`
*   `numpy`
*   `statsmodels.tsa.stattools` (specifically `adfuller`)

**Input Data Location Reminder for `EDA.py`:**
The script should expect `train_clean.csv` to be in a `/data` subdirectory relative to where `pipeline.py` is run (e.g., it should try to load `data/train_clean.csv`).

Your goal is not just to run analyses, but to produce a concise, structured set of actionable intelligence that the `Feature_Engineering_Agent` can directly leverage.












# Risk Analytics Agent

A Python-based agent system for pre-trade risk scoring and market regime prediction in fixed income trading.

## Overview

The Risk Analytics Agent provides real-time risk analysis and market regime prediction for trading systems. It uses temporal fusion transformers (TFT) for market regime prediction and specialized models (N-BEATS, DeepAR) for risk scoring.

## Features

- Market regime prediction with 7 distinct regime classifications
- Dynamic risk scoring based on current market conditions
- Real-time analysis of trade risk profiles
- REST and streaming API interfaces
- Feature store integration with Redis and Apache Pinot
- Agent-based architecture for flexible deployment

## Installation

### Prerequisites

- Python 3.9 or higher
- Git LFS (for model storage)
- Docker (optional, for containerized deployment)

### Setup

1. Clone the repository:
   ```
   git clone https://github.com/example/risk-analytics-agent.git
   cd risk-analytics-agent
   ```

2. Set up the environment:
   ```
   python -m venv venv
   source venv/bin/activate  # On Windows: venv\Scripts\activate
   pip install -e .
   ```

3. Configure environment variables:
   ```
   cp env.example .env
   # Edit .env with your configuration
   ```

## Development

To set up the development environment:

```
pip install -e ".[dev,notebook]"
pre-commit install
```

## Usage

### Running the API server

```
risk-analytics-serve
```

Or manually:

```
python -m risk_analytics_agent.main
```

### Training models

```
risk-analytics-train --model regime --config configs/model_configs/regime_model.yaml
```

## API Documentation

After starting the server, API documentation is available at:
- Swagger UI: http://localhost:8000/docs
- ReDoc: http://localhost:8000/redoc

## Testing

Run the test suite:

```
pytest
```

With coverage:

```
pytest --cov=src
```

## License

MIT License

## Implementation Status

The following tasks from the implementation checklist have been completed:

### Phase 1: Project Setup and Infrastructure

- [x] Task 1: Project Structure Setup
  - [x] Sub-task 1.1: Create folder structure according to structure.md
  - [x] Sub-task 1.2: Set up .gitignore file with appropriate patterns for Python projects
  - [x] Sub-task 1.3: Initialize Git repository with .gitattributes for LFS handling

- [x] Task 2: Environment Configuration
  - [x] Sub-task 2.1: Create .env.example with appropriate values
  - [x] Sub-task 2.2: Create and initialize pyproject.toml file
  - [x] Sub-task 2.3: Update pyproject.toml with dependencies
  - [x] Sub-task 2.4: Update setup.py with proper package information and dependencies
  - [x] Sub-task 2.5: Create requirements.txt for development environment

### Current Tasks in Progress

- [ ] Task 3: Data Connectors Implementation
- [ ] Task 4: Feature Store Implementation
- [ ] Task 5: Data Schema Definitions
- [ ] Task 6: Data Preprocessing Pipeline