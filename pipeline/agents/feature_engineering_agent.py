from agno.agent import Agent

FEATURE_ENGINEERING_AGENT_BASE_PROMPT = '''Generate, execute, debug, and save `FEATURE.py` for stock market feature engineering.

**`FEATURE.py` Requirements:**
1.  **`DATA_DIR`**: Use global `DATA_DIR = "data"` for all file paths.
2.  **Load Data**: From `DATA_DIR`, load `train_clean.csv`, `val_clean.csv`, `test_clean.csv` using pandas.
    *   **Columns**: Process in specified order: 'Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Target_return'.
    *   **'Date' Column**: Parse 'yyyy-mm-dd' strings into numeric timestamps; set as DataFrame index.
    *   **Parsing**: Ensure fail-safe value parsing; unparseable values must become NaN.
3.  **Feature Creation**: Generate new features.
    *   **EDA-Driven**: Prioritize insights from Exploratory Data Analysis (EDA), provided during invocation, for specific choices (e.g., lag periods, MA windows, indicators). The script must accommodate these EDA-driven parameters or include comments for their integration.
    *   **Examples**: Lagged features ('Close', 'Volume'), Moving Averages ('Close'), volatility (rolling std dev of 'Close' or returns), momentum (ROC, RSI for 'Close'), date-based (day/week/month/quarter/year), interaction terms (e.g., price * volume).
4.  **Missing Value Imputation**:
    *   Handle NaNs resulting from new feature creation.
    *   Use a suitable imputation strategy (e.g., fill with 0/mean/median, `sklearn.impute.SimpleImputer`, `KNNImputer`).
    *   **CRITICAL**: Fit any imputer or statistics (mean/median) *only* on `train_featured.csv` data. Apply the *same fitted* imputer/statistics to transform `val_featured.csv` and `test_featured.csv`.
5.  **Feature Scaling (Recommended)**:
    *   Apply scaling to all numerical features (excluding 'Target_return') using `sklearn.preprocessing` (e.g., `StandardScaler`, `MinMaxScaler`).
    *   **CRITICAL**: Fit the scaler *only* on `train_featured.csv` data. Apply the *same fitted* scaler to transform `val_featured.csv` and `test_featured.csv`.
6.  **Save Processed Data**: Save resulting DataFrames as `train_featured.csv`, `val_featured.csv`, `test_featured.csv` into `DATA_DIR`. Preserve the date index.
7.  **Libraries**: Primarily use `pandas`, `numpy`, `sklearn.preprocessing`, and `sklearn.impute` (if imputation tools are used).

**Agent Operational Flow:**
1.  **Execute `FEATURE.py`**: Run the generated script.
2.  **Debug `FEATURE.py`**: If execution fails, attempt to debug and correct (max 3 retries). Report failure if unfixable.
3.  **Save `FEATURE.py`**: After successful execution, save the final working `FEATURE.py` to the current working directory.
'''

FEATURE_ENGINEERING_AGENT_EXPECTED_OUTPUT = """Return 'SUCCESS' as a string if all steps (generation, execution, debugging, saving of `FEATURE.py`) are successful. 
If any step fails definitively (e.g., script cannot be fixed after retries, or a critical error occurs), return a clear failure message as a string, starting with 'FAILURE:'."""

def create_feature_engineering_agent(model, description):
    """Factory function to create a Feature Engineering Agent with proper configuration.
    
    Args:
        model: The Azure OpenAI model instance
        description: The agent description
    
    Returns:
        Agent: Configured Feature Engineering Agent instance
    """
    return Agent(
        name="Feature_Engineering_Agent",
        model=model,
        description=description,
        instructions=None,  # Will be set dynamically in run()
        expected_outcome=FEATURE_ENGINEERING_AGENT_EXPECTED_OUTPUT
    ) 