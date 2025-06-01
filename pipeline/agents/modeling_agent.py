from agno.agent import Agent

MODELING_AGENT_INSTRUCTIONS = '''Your primary task is to generate, execute, debug, and save a Python script named `MODEL.py`.
This script will train a predictive model on the engineered features and generate predictions.

**Instructions for `MODEL.py` Generation:**
1.  **Global `DATA_DIR`**: The script MUST use a global variable `DATA_DIR` (which will be "data") to locate input files and save outputs.
2.  **Load Data**: Load `train_featured.csv` and `val_featured.csv` from `DATA_DIR` using pandas.
3.  **Prepare Data for Modeling**: 
    *   Identify the target variable (e.g., 'Target_return').
    *   Separate features (X) from the target (y) for each dataset (train, val).
4.  **Model Selection and Training**:
    *   Choose **one** appropriate regression model to predict the target variable. Focus on modern and effective models such as:
        *   `XGBoost` (e.g., `xgb.XGBRegressor`)
        *   `LightGBM` (e.g., `lgb.LGBMRegressor`)
        *   A simple Neural Network (e.g., using `sklearn.neural_network.MLPRegressor` or a basic `tensorflow.keras.Sequential` model).
    *   The script should implement the chosen model.
    *   The chosen model MUST be trained ONLY on `train_featured.csv` data (X_train, y_train).
    *   Perform comprehensive hyperparameter tuning for this single chosen model. You MUST use `sklearn.model_selection.GridSearchCV` or `sklearn.model_selection.RandomizedSearchCV`.
        *   Define a relevant parameter grid for the chosen model.
        *   The tuning process must use the **validation set** (`val_featured.csv`) to evaluate different hyperparameter combinations. This means training candidate models/parameters on X_train, y_train, and evaluating them (using RMSE) on X_val, y_val.
        *   The scoring metric to optimize for (minimize) is Root Mean Squared Error (RMSE). When using scikit-learn search CV objects, you MUST use `neg_root_mean_squared_error` as the `scoring` parameter.
    *   The model with the optimal hyperparameters found during tuning is considered the 'best model'. This best model (instance with best hyperparameters) should then be re-trained on the **entire `train_featured.csv` data** before being saved as `best_model.joblib`.
5.  **Validation Set Evaluation**:
    *   After hyperparameter tuning, the script MUST obtain the best RMSE achieved on the validation set (`val_featured.csv`) with the best hyperparameters.
    *   The script MUST print this best validation RMSE to the console, formatted EXACTLY as: `Best Validation RMSE: <float_value>`
6.  **Save Trained Model**: 
    *   Save the **single, best-tuned model** to a file named `best_model.joblib` in the current working directory (e.g., using `joblib.dump`).
7.  **Output Confirmation**: The script MUST print the best validation RMSE to console as specified. After all steps, including saving the model, the script MUST print the exact string "MODEL.py execution successful." to standard output upon successful completion.
8.  **Libraries**: Use appropriate libraries such as `pandas`, `numpy`, `sklearn` (specifically `sklearn.model_selection` for `GridSearchCV`/`RandomizedSearchCV`, `sklearn.metrics` for `mean_squared_error` to calculate RMSE, `sklearn.neural_network.MLPRegressor` if chosen), `joblib` for model saving. Depending on the chosen model, also include `xgboost` (for `xgb.XGBRegressor`) or `lightgbm` (for `lgb.LGBMRegressor`), or `tensorflow` (if a Keras Sequential model is chosen). Ensure all necessary imports are included in `MODEL.py`.

**Agent's Operational Requirements (beyond script generation):**
1.  **Execute `MODEL.py`**: Run the generated script.
2.  **Debug `MODEL.py`**: If execution fails, attempt to debug and correct the script (maximum 3 retries). Report failure if still unsuccessful.
3.  **Save `MODEL.py`**: After successful execution, save the final working version of `MODEL.py` to the current working directory.
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