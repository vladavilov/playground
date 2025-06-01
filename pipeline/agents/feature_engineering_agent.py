from agno.agent import Agent

FEATURE_ENGINEERING_AGENT_BASE_PROMPT = '''Your primary task is to generate, execute, debug, and save a Python script named `FEATURE.py`.
This script will perform feature engineering on stock market data.

**Instructions for `FEATURE.py` Generation:**
1.  **Global `DATA_DIR`**: The script MUST use a global variable `DATA_DIR` (which will be "data") to locate input and output files.
2.  **Load Data**: Load `train_clean.csv`, `val_clean.csv`, and `test_clean.csv` from the `DATA_DIR` using pandas.
3.  **Feature Creation**: Based on the loaded data, create new features. Consider the following categories:
    *   **Lagged Features**: Create lagged versions of relevant columns (e.g., 'Close' price, 'Volume') for several previous time steps (e.g., 1, 2, 3, 5, 10 days).
    *   **Moving Averages (MAs)**: Calculate MAs for 'Close' price over different windows (e.g., 7-day, 14-day, 30-day).
    *   **Volatility Measures**: Calculate rolling standard deviation of 'Close' price or daily returns for different windows.
    *   **Momentum Indicators**: Implement indicators like Rate of Change (ROC) or Relative Strength Index (RSI) for 'Close' price.
    *   **Date-Based Features**: Extract features from the date index, such as day of the week, month, quarter, or year.
    *   **Interaction Features**: Consider creating interaction terms if suggested by prior analysis (e.g., price * volume).
    *   **Guidance from EDA**: IMPORTANT - The specific features to implement, especially choices of lag periods, MA windows, or particular indicators, should be significantly influenced by insights provided from a preceding Exploratory Data Analysis (EDA) step. These EDA insights will be provided to you as part of your overall instructions when you are invoked. Your generated script should be flexible enough or include comments where such insights would guide the choices.
4.  **Missing Value Handling**: After creating new features (especially time-based ones like lags or rolling MAs), missing values (NaNs) might appear at the beginning of the datasets.
    *   Identify columns with NaNs.
    *   Use a suitable imputation strategy (e.g., filling with a specific value like 0, mean, median, or using `sklearn.impute.SimpleImputer` or `KNNImputer`).
    *   **CRITICAL**: Any imputer or statistics (like mean/median) used for imputation MUST be fitted *only* on the `train_featured.csv` data. The *same fitted imputer/statistics* must then be used to transform `val_featured.csv` and `test_featured.csv`.
5.  **Feature Scaling (Recommended)**:
    *   Apply feature scaling to all numerical features (excluding the target variable, 'Target_return').
    *   Choose a scaler from `sklearn.preprocessing` (e.g., `StandardScaler`, `MinMaxScaler`).
    *   **CRITICAL**: Fit the scaler *only* on the `train_featured.csv` data. Apply the *same fitted scaler* to transform `val_featured.csv` and `test_featured.csv`.
6.  **Save Processed Data**: Save the resulting pandas DataFrames (with new features, imputed values, and scaled data) as `train_featured.csv`, `val_featured.csv`, and `test_featured.csv` into the `DATA_DIR`. Ensure the index is preserved if it's a date index.
7.  **Libraries**: The `FEATURE.py` script should primarily use `pandas`, `numpy`, and `sklearn.preprocessing` (and `sklearn.impute` if used).

**Agent's Operational Requirements (beyond script generation):**
1.  **Execute `FEATURE.py`**: Run the generated script.
2.  **Debug `FEATURE.py`**: If execution fails, attempt to debug and correct the script (maximum 3 retries). Report failure if still unsuccessful.
3.  **Save `FEATURE.py`**: After successful execution, save the final working version of `FEATURE.py` to the current working directory.
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