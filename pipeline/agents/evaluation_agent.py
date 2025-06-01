from agno.agent import Agent

EVALUATION_AGENT_INSTRUCTIONS = '''Your primary task is to generate, execute, debug, and save a Python script named `EVAL.py`.
This script will evaluate the trained model on the test dataset.

**Instructions for `EVAL.py` Generation:**
1.  **Global `DATA_DIR`**: The script MUST use a global variable `DATA_DIR` (which will be "data") to locate input files.
2.  **Load Data and Model**:
    *   Load `test_featured.csv` from `DATA_DIR` using pandas.
    *   Load the trained model from `best_model.joblib` located in the current working directory using `joblib.load`.
3.  **Prepare Data for Evaluation**:
    *   Identify the target variable (e.g., 'Target_return').
    *   Separate features (X_test) from the target (y_test) in the test dataset.
4.  **Make Predictions**:
    *   Use the loaded model to make predictions on X_test.
5.  **Calculate Metrics**:
    *   Calculate the Root Mean Squared Error (RMSE) between the predictions and y_test.
    *   Use `sklearn.metrics.mean_squared_error` and then take the square root (`numpy.sqrt`).
6.  **Save Score**:
    *   Save the calculated RMSE to a file named `MSFT Score.txt` in the current working directory.
    *   The content of this file MUST be formatted EXACTLY as: `RMSE: <float_value>` (e.g., `RMSE: 0.01234`).
7.  **Output Confirmation**: The script MUST print the exact string "EVAL.py execution successful." to standard output upon successful completion of all its internal steps (loading data, loading model, prediction, calculation, saving score).
8.  **Libraries**: The `EVAL.py` script should use `pandas`, `numpy`, `sklearn.metrics`, and `joblib`. Ensure all necessary imports are included in `EVAL.py`.

**Agent's Operational Requirements (beyond script generation):**
1.  **Execute `EVAL.py`**: Run the generated script.
2.  **Debug `EVAL.py`**: If execution fails (e.g., Python errors, file not found for the score, incorrect score format, missing print output), you MUST attempt to debug and correct the script. You have a maximum of 3 attempts to fix and re-run the script.
3.  **Save `EVAL.py`**: After successful execution (and any necessary debugging), save the final, working version of `EVAL.py` to the current working directory.
'''

EVALUATION_AGENT_EXPECTED_OUTPUT = """Return 'SUCCESS' as a string if all steps (generation, execution, debugging, saving of `EVAL.py`, and `EVAL.py` prints 'EVAL.py execution successful.' and `MSFT Score.txt` is correctly created) are successful. 
If any step fails definitively (e.g., script cannot be fixed after retries, critical error, or required print output/score file not found or incorrect), return a clear failure message as a string, starting with 'FAILURE:'."""

def create_evaluation_agent(model, description):
    """Factory function to create an Evaluation Agent with proper configuration.
    
    Args:
        model: The Azure OpenAI model instance
        description: The agent description
    
    Returns:
        Agent: Configured Evaluation Agent instance
    """
    return Agent(
        name="Evaluation_Agent",
        model=model,
        description=description,
        instructions=EVALUATION_AGENT_INSTRUCTIONS,
        expected_outcome=EVALUATION_AGENT_EXPECTED_OUTPUT
    ) 