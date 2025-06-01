from agno.agent import Agent

MODELING_AGENT_INSTRUCTIONS = '''Objective: Generate, execute, debug, and save `MODEL.py` for predictive modeling.

**`MODEL.py` Requirements:**

1.  **Config & Imports:**
    *   Define global `DATA_DIR = "data"`.
    *   Import necessary libraries: `pandas`, `numpy`, `joblib`, `sklearn.model_selection` (for `GridSearchCV` or `RandomizedSearchCV`), `sklearn.metrics` (for `mean_squared_error` to calculate RMSE). Also, model-specific libraries: `xgboost`, `lightgbm`, `sklearn.neural_network.MLPRegressor`, or `tensorflow.keras` based on selection.

2.  **Data Prep:**
    *   Load `train_featured.csv` and `val_featured.csv` from `DATA_DIR` into pandas DataFrames.
    *   Identify the target variable. Separate features (X) and target (y) to create `X_train, y_train, X_val, y_val`.

3.  **Model & Tuning:**
    *   Select one regression model: `xgb.XGBRegressor`, `lgb.LGBMRegressor`, `sklearn.neural_network.MLPRegressor`, or a basic `tensorflow.keras.Sequential` model.
    *   Tune hyperparameters using `GridSearchCV` or `RandomizedSearchCV`:
        *   Train candidate models on `X_train, y_train`.
        *   Evaluate candidates on `X_val, y_val` using RMSE (minimize, `scoring='neg_root_mean_squared_error'`).
    *   Determine the best model configuration (model instance with optimal hyperparameters).

4.  **Final Train & Save:**
    *   Re-train the determined best model on the entire `train_featured.csv` dataset (`X_train, y_train`).
    *   Save this re-trained model as `best_model.joblib` using `joblib.dump`.

5.  **Output & Confirmation:**
    *   Using the best model/hyperparameters, calculate its RMSE on `val_featured.csv` (`X_val, y_val`).
    *   Print to console: `Best Validation RMSE: <float_value>`.
    *   After all preceding steps complete successfully (including model saving and RMSE print), print: "MODEL.py execution successful.".

**Agent Protocol:**
1.  Execute the generated `MODEL.py`.
2.  If execution fails: Attempt to debug and correct `MODEL.py` (maximum 3 retries). Report if unfixable.
3.  If `MODEL.py` execution is successful (confirmed by its "MODEL.py execution successful." console output): Save the final, working `MODEL.py`.
'''

MODELING_AGENT_EXPECTED_OUTPUT = """Return 'SUCCESS' as a string if all steps (generation, execution, debugging, saving of `MODEL.py`, and `MODEL.py` prints 'MODEL.py execution successful.') are successful. 
If any step fails definitively (e.g., script cannot be fixed, critical error, or required print output not found), return a clear failure message as a string, starting with 'FAILURE:'."""

def create_modeling_agent(model, description):
    """Factory function to create a Modeling Agent with proper configuration.
    
    Args:
        model: The Azure OpenAI model instance
        description: The agent description
    
    Returns:
        Agent: Configured Modeling Agent instance
    """
    return Agent(
        name="Modeling_Agent",
        model=model,
        description=description,
        instructions=MODELING_AGENT_INSTRUCTIONS,
        expected_outcome=MODELING_AGENT_EXPECTED_OUTPUT
    ) 