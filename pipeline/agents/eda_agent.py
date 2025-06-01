from agno.agent import Agent

EDA_AGENT_INSTRUCTIONS = '''You must perform the following steps:
1.  **Generate a Python script named `EDA.py`**. This script will perform the actual EDA tasks.
    *   The `EDA.py` script MUST take into account a global variable `DATA_DIR` which will be defined in the execution environment of the script, and which specifies the directory for input data (its value is "data").
    *   **Load Data**: The script MUST load `train_clean.csv` from the `DATA_DIR`.
        *   **CSV Loading Parameters**: When loading `train_clean.csv` using `pandas.read_csv`:
            *   The expected column order is: 'Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Target_return'. The script must process data according to this order.
            *   The 'Date' column, containing strings in 'yyyy-mm-dd' format, MUST be parsed into datetime objects.
            *   Value parsing MUST be fail-safe: incorrect or unparseable values (e.g., non-numeric in numeric columns, invalid date strings) MUST result in NaN.
    *   **Perform Analyses**:
        *   Print comprehensive summary statistics for the dataset.
        *   Print an analysis of missing values for all columns.
        *   Print a correlation analysis of all numerical features against the 'Target_return' column.
        *   Perform and print the results of Augmented Dickey-Fuller (ADF) tests for stationarity on the 'Close' price time series and the 'Target_return' time series.
        *   Perform outlier detection on key numerical features (e.g., 'Close', 'Volume', 'Target_return'). You can use methods like IQR or Z-score. Print a brief summary if significant outliers are detected and any potential implications or handling suggestions for feature engineering.
    *   **Synthesize Insights**: The script MUST print a dedicated section at the very end of its output, formatted EXACTLY as follows:
        ```
        Synthesized Insights Output:
        [Concise summary of key findings from the EDA relevant for feature engineering. This should include observations on stationarity of 'Close' and 'Target_return', any significant correlations with 'Target_return', notes on data quality or missing values, and a brief summary regarding significant outliers if detected, that might impact feature creation.]
        ```
        The content within the brackets should be replaced with your actual synthesized findings.
    *   **Libraries**: The `EDA.py` script should use `pandas`, `numpy`, and `statsmodels.tsa.stattools`. Ensure all necessary imports are included in `EDA.py`.

2.  **Execute `EDA.py`**: Run the generated `EDA.py` script.

3.  **Debug `EDA.py` (if necessary)**:
    *   If `EDA.py` fails during execution (e.g., due to Python errors), you MUST attempt to debug and correct the script.
    *   You have a maximum of 3 attempts to fix and re-run the script.
    *   If debugging is successful, proceed.
    *   If the script still fails after 3 attempts, report the failure.

4.  **Save `EDA.py`**: After successful execution (and any necessary debugging), save the final, working version of `EDA.py` to the current working directory.

The `DATA_DIR` variable will be available in the global scope where `EDA.py` is executed, and its value is "data".
Example of accessing `DATA_DIR` in `EDA.py`:
```python
import os
import pandas as pd
# DATA_DIR is globally available
file_path = os.path.join(DATA_DIR, 'train_clean.csv')
# df = pd.read_csv(file_path)
```
Focus on robust script generation and clear insight extraction.
'''

EDA_AGENT_EXPECTED_OUTPUT = """If all steps (generation, execution, debugging, saving of `EDA.py`) are successful, your FINAL output MUST BE ONLY the string content that `EDA.py` printed under the 'Synthesized Insights Output:' heading. Do not include any other text, explanations, or formatting around this output.
If any step fails definitively (e.g., script cannot be fixed after retries, or a critical error occurs), you must return a clear and concise failure message as a string, starting with 'FAILURE:'."""

def create_eda_agent(model, description):
    """Factory function to create an EDA Agent with proper configuration.
    
    Args:
        model: The Azure OpenAI model instance
        description: The agent description
    
    Returns:
        Agent: Configured EDA Agent instance
    """
    return Agent(
        name="EDA_Agent",
        model=model,
        description=description,
        instructions=EDA_AGENT_INSTRUCTIONS,
        expected_outcome=EDA_AGENT_EXPECTED_OUTPUT
    ) 