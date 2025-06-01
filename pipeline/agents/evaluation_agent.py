from agno.agent import Agent

EVALUATION_AGENT_INSTRUCTIONS = '''Task: Generate, execute, debug, and save `EVAL.py` for model evaluation on the test dataset.

**`EVAL.py` Requirements:**
1.  Global `DATA_DIR = "data"` for input file paths.
2.  Load:
    *   `test_featured.csv` from `DATA_DIR` via `pandas`.
    *   `best_model.joblib` from CWD via `joblib.load`.
3.  Data Prep: Identify target variable (e.g., 'Target_return'); separate features (X_test) and target (y_test) from test data.
4.  Predictions: Use loaded model to predict on X_test.
5.  Metrics: Calculate RMSE using `numpy.sqrt(sklearn.metrics.mean_squared_error(y_test, predictions))`.
6.  Save Score: Write RMSE to `MSFT Score.txt` (in CWD) with content formatted EXACTLY as `RMSE: <float_value>` (e.g., `RMSE: 0.01234`).
7.  Output Confirmation: On successful completion of all internal steps, print EXACTLY "EVAL.py execution successful." to stdout.
8.  Libraries: `EVAL.py` MUST include imports for `pandas`, `numpy`, `sklearn.metrics`, `joblib`.

**Agent Operational Requirements:**
1.  Execute: Run the generated `EVAL.py`.
2.  Debug: If `EVAL.py` execution fails, attempt to debug and correct the script. Max 3 attempts to fix and re-run.
3.  Save: After successful execution (including any debugging), save the final, working `EVAL.py` to CWD.
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