# Project Title: MSFT Next Day Log Return Prediction via Agentic Workflow Pipeline

## 1. Primary Project Objective:
To develop a Python script, `pipeline.py`, that utilizes the AGNO framework to define, configure, and orchestrate an agent-based workflow. This workflow will predict the next day's log return for Microsoft (MSFT) stock by sequentially invoking four specialized agents, each responsible for generating a specific Python script (`EDA.py`, `FEATURE.py`, `MODEL.py`, `EVAL.py`). `pipeline.py` will define an AGNO `Workflow` class to manage this process.

## 2. Core `pipeline.py` Script Requirements:

### 2.1. Agent Definition & Configuration:
    2.1.1. **Framework:** `pipeline.py` must define an AGNO `Workflow` class (e.g., `MSFTLogReturnWorkflow(agno.workflow.Workflow)`). This class will encapsulate the agents and orchestration logic.
    2.1.2. **Agent Instantiation:** The `Workflow` class in `pipeline.py` must define and instantiate exactly four AGNO `Agent` instances (e.g., `agno.agent.Agent`) as attributes.
    2.1.3. **Agent Naming:** The agents instantiated within `pipeline.py` must be named precisely:
        2.1.3.1. `EDA_Agent`
        2.1.3.2. `Feature_Engineering_Agent`
        2.1.3.3. `Modeling_Agent`
        2.1.3.4. `Evaluation_Agent`
    2.1.4. **Prompt-Based Agent Configuration:**
        2.1.4.1. `pipeline.py` will contain the master prompts used to configure each AGNO agent.
        2.1.4.2. These prompts are the *sole* method for instructing agents on the functionality, content, *execution, debugging (with limited retries),* and saving of the Python scripts they are to generate (i.e., `EDA.py`, `FEATURE.py`, `MODEL.py`, `EVAL.py`).
        2.1.4.3. Passing pre-defined code blocks or functions to agents during their construction or operation via `pipeline.py` is strictly prohibited.
    2.1.5. **AI Model Integration:**
        2.1.5.1. `pipeline.py` will configure each agent to use an Azure OpenAI model.
        2.1.5.2. The Azure OpenAI model object (e.g., an instance of an AGNO model class configured for Azure, or a compatible dictionary) will be passed as an argument to a function like `build_workflow(azure_model)`. This function will instantiate the main AGNO `Workflow` class defined in `pipeline.py`, passing the `azure_model` to its constructor. The `Workflow` class's constructor will then use this `azure_model` to configure the `model` parameter of each of the four AGNO `Agent` instances. These model details should not be hardcoded within agent prompts or pipeline/workflow.

### 2.2. Workflow Orchestration by `pipeline.py` (within the AGNO `Workflow` class's `run` method):
    2.2.1. **Sequential Invocation:** The `run` method of the AGNO `Workflow` class in `pipeline.py` must manage the sequential invocation of the agents. Each agent is responsible for the full lifecycle (generation, execution, debugging, saving) of its respective Python script.
    2.2.2. **EDA Phase:**
        2.2.2.1. Invoke `EDA_Agent` with its configured prompt. The prompt will instruct the agent to generate `EDA.py`, execute it, debug any errors (e.g., up to 2-3 attempts), save the final working script as `EDA.py`, and return the synthesized insights.
        2.2.2.2. The `Workflow.run()` method in `pipeline.py` must capture the synthesized insights and actionable recommendations returned by `EDA_Agent` upon successful completion of its task.
    2.2.3. **Feature Engineering Phase:**
        2.2.3.1. The `Workflow.run()` method in `pipeline.py` must dynamically construct or adjust the prompt for the `Feature_Engineering_Agent`, incorporating the insights received from `EDA_Agent`.
        2.2.3.2. Invoke `Feature_Engineering_Agent` with this dynamic prompt. The prompt will instruct the agent to generate `FEATURE.py`, execute it, debug any errors (e.g., up to 2-3 attempts), and save the final working script as `FEATURE.py`.
    2.2.4. **Modeling Phase:**
        2.2.4.1. Invoke `Modeling_Agent` with its configured prompt. The prompt will instruct the agent to generate `MODEL.py`, execute it, debug any errors (e.g., up to 2-3 attempts), and save the final working script as `MODEL.py`.
    2.2.5. **Evaluation Phase:**
        2.2.5.1. Invoke `Evaluation_Agent` with its configured prompt. The prompt will instruct the agent to generate `EVAL.py`, execute it, debug any errors (e.g., up to 2-3 attempts), and save the final working script as `EVAL.py`.
    2.2.6. **Error Handling (General Guidance):** `pipeline.py` (specifically the `Workflow.run()` method) should include a `try...except` block for each agent invocation. If an agent signals failure (e.g., after exhausting debug attempts or encountering an unrecoverable issue), the workflow should log the error to `stdout` and halt. Prompts will instruct agents to manage their internal script execution and debugging loops. Basic logging of major workflow steps and agent invocations should be printed to `stdout` by `pipeline.py`.

### 2.3. File and Data Management by `pipeline.py`:
    2.3.1. **Input Data Location:** `pipeline.py` will use a hardcoded global variable (e.g., `DATA_DIR = "data"`) to define the path to input CSV files (`test_clean.csv`, `train_clean.csv`, `val_clean.csv`), relative to `pipeline.py`'s execution path.
    2.3.2. **Output Script Location:** All Python scripts generated, executed, debugged, and finalized by the agents (`EDA.py`, `FEATURE.py`, `MODEL.py`, `EVAL.py`) must be saved by the agents (as per their prompts) into the root working directory where `pipeline.py` is executed. This saving action occurs after the agent has successfully executed and debugged its script.
    2.3.3. **Output Data & Model Location:**
        2.3.3.1. `FEATURE.py` should save `train_featured.csv`, `val_featured.csv`, `test_featured.csv` (e.g., in the `/data` directory or working directory, ensuring accessibility for subsequent agents).
        2.3.3.2. `MODEL.py` should save `best_model.joblib` (or `.pkl`) in the working directory.
        2.3.3.3. `EVAL.py` should create/overwrite `MSFT Score.txt` in the working directory.

## 3. Data:
    3.1. **Input Data:**
        3.1.1. Source: Provided datasets located in the `/data` folder: test_clean.csv, train_clean.csv, val_clean.csv.
        3.1.2. Features (Predictors): OHLC (Open, High, Low, Close), Date and Target_return.
        3.1.3. Time Span: 2016-01-01 to 2024-12-30.
    3.2. **Target Variable:**
        3.2.1. Name: "Target_return"
        3.2.2. Definition: `ln(closing_price[t+1] / closing_price[t])`, where `t` represents a specific day.
    3.3. **Data Splits:**
        3.3.1. The data is pre-split into Train, Validation, and Test sets.
        3.3.2. These splits must be respected by the corresponding agents and their generated scripts.

## 4. Guidance for Agent Prompts (To be implemented in `pipeline.py`):

This section outlines the core requirements for the Python scripts (`EDA.py`, `FEATURE.py`, `MODEL.py`, `EVAL.py`) that each AGNO agent must be prompted to generate, execute, debug, and save. `pipeline.py` will contain these detailed instructions within its prompt definitions for each agent.

### 4.1. General Instructions for All Agent Prompts:
Each agent's prompt in `pipeline.py` must instruct it to perform the following sequence:
    4.1.1. **Generate Script:** Generate a Python script with a specific name (detailed below for each agent).
    4.1.2. **Execute Script:** Execute the generated Python script.
    4.1.3. **Debug Script (If Necessary):**
        4.1.3.1. If the script execution fails, attempt to debug the script by analyzing the error and modifying the generated code.
        4.1.3.2. Retry execution after debugging.
        4.1.3.3. Make a maximum of [e.g., 3] attempts to debug and successfully execute the script.
        4.1.3.4. If, after the allowed attempts, the script still fails, the agent should report failure to the orchestrator.
    4.1.4. **Save Script:** If the script executes successfully (either initially or after debugging), save the final, working Python script to the specified filename in the working directory.
    4.1.5. **Script Requirements:**
        4.1.5.1. The generated script should be capable of being executed independently if run manually.
        4.1.5.2. Include all necessary library imports in the generated script (e.g., pandas, numpy, scikit-learn, statsmodels, joblib).
        4.1.5.3. Ensure all file paths used in the generated scripts (for data input and output) are consistent with the locations specified in Section 2.3.
    4.1.6. **Reporting:** The agent's final response to `pipeline.py` should clearly indicate success or failure of this entire process.

### 4.2. `EDA_Agent` -> `EDA.py` Script Guidance:
The prompt for `EDA_Agent` in `pipeline.py` must instruct it to generate, execute, debug, and save `EDA.py` with the following characteristics:

*   **4.2.1. Purpose of Task:** The agent's overall task is to produce and validate `EDA.py`, and upon its successful execution, return key insights. The `EDA.py` script itself should conduct a thorough Exploratory Data Analysis on the **training dataset** (`train_clean.csv`) to uncover patterns, distributions, anomalies, missing data, and relationships between features (OHLCV) and "Target_return".
*   **4.2.2. Input for `EDA.py` script:**
    *   Training dataset: `/data/train_clean.csv`.
    *   Expected columns: Date, Open, High, Low, Close, Volume, "Target_return".
*   **4.2.3. `EDA.py` Script Functionality (to be requested by the prompt):**
    *   Load data using `pandas`.
    *   **Summary Statistics:** For all numerical features (OHLCV, target return) - count, mean, std, min, Q1, median, Q3, max. (e.g., using `df.describe()`).
    *   **Missing Value Analysis:** Report number and percentage of missing values per column. (e.g., using `df.isnull().sum()` and `(df.isnull().sum() / len(df)) * 100`).
    *   **Correlation Analysis:** Pearson correlation matrix for numerical features including "Target_return". (e.g., using `df.corr()`).
    *   **Outlier Discussion (Qualitative Print Output):** Brief summary if significant outliers are detected.
    *   **Stationarity Check:** Augmented Dickey-Fuller (ADF) test for 'Close' price and 'Target_return'. Interpret and print p-value. (e.g., using `statsmodels.tsa.stattools.adfuller`).
    *   **Synthesized Insights Output (by `EDA.py` script):** The script must print a structured set of insights and actionable recommendations based on the analysis.
*   **4.2.4. Agent Output to `pipeline.py`:** Upon successful generation, execution, debugging, and saving of `EDA.py`, the `EDA_Agent` must return the "Synthesized Insights Output" (from 4.2.3) as a string in its response to `pipeline.py`. If the agent fails this overall task after its debug attempts, it should report failure.
*   **4.2.5. Libraries to specify in prompt for `EDA.py`:** `pandas`, `numpy`, `statsmodels`.

### 4.3. `Feature_Engineering_Agent` -> `FEATURE.py` Script Guidance:
The prompt for `Feature_Engineering_Agent` in `pipeline.py` (dynamically adjusted with EDA insights) must instruct it to generate, execute, debug, and save `FEATURE.py`:

*   **4.3.1. Purpose of Task:** The agent's overall task is to produce and validate `FEATURE.py`. The `FEATURE.py` script itself should create new features from OHLCV data, handle missing value imputation, and apply feature scaling. Operations must be consistent across Train, Validation, and Test datasets.
*   **4.3.2. Input for `FEATURE.py` script:**
    *   Raw datasets: `/data/train_clean.csv`, `/data/val_clean.csv`, `/data/test_clean.csv`.
    *   The prompt will include insights from `EDA.py` to guide feature selection/creation.
*   **4.3.3. `FEATURE.py` Script Functionality (to be requested by the prompt):**
    *   Load all three datasets using `pandas`.
    *   **Feature Creation Examples (Prompt to consider these and others based on EDA):**
        *   Lagged features (e.g., `df['target_return'].shift(1)`).
        *   Moving Averages (SMA, EMA for 'Close', 'Volume').
        *   Volatility Measures (Std dev of returns, ATR).
        *   Momentum Indicators (RSI, MACD).
        *   Other price-based features (daily price range, open-close percent change).
    *   **Handling Missing Values:**
        *   Address NaNs from lags/rolling windows (e.g., `fillna` (bfill then ffill) or `dropna`).
        *   Imputation (if any, e.g., mean/median) must use statistics from the **training set only** and apply to all splits.
    *   **Feature Scaling (Optional, if EDA suggests):**
        *   If used (e.g., `StandardScaler`, `MinMaxScaler` from `sklearn.preprocessing`), fit **only** on training data, then `transform` train, validation, and test sets.
    *   **Output Transformed Datasets:**
        *   Save `train_featured.csv`, `val_featured.csv`, `test_featured.csv` (e.g., to `/data` or working directory). These files should contain original selected features, new features, and "Target_return".
*   **4.3.4. Agent Output to `pipeline.py`:** The agent should report overall success or failure (after debug attempts) to `pipeline.py`.
*   **4.3.5. Libraries to specify in prompt for `FEATURE.py`:** `pandas`, `numpy`, `sklearn.preprocessing`.

### 4.4. `Modeling_Agent` -> `MODEL.py` Script Guidance:
The prompt for `Modeling_Agent` in `pipeline.py` must instruct it to generate, execute, debug, and save `MODEL.py`:

*   **4.4.1. Purpose of Task:** The agent's overall task is to produce and validate `MODEL.py`. The `MODEL.py` script itself should train predictive models, perform hyperparameter tuning using the validation set (RMSE), select the best model, and save it.
*   **4.4.2. Input for `MODEL.py` script:**
    *   Feature-engineered data: `train_featured.csv`, `val_featured.csv` (from location specified in 2.3.3.1).
*   **4.4.3. `MODEL.py` Script Functionality (to be requested by the prompt):**
    *   Load `train_featured.csv` and `val_featured.csv`.
    *   Separate features (X) from target (y).
    *   **Model Selection (Prompt to try one or more):**
        *   `sklearn.linear_model.LinearRegression`, `Ridge`, `Lasso`.
        *   `sklearn.ensemble.RandomForestRegressor`, `GradientBoostingRegressor`.
        *   `sklearn.svm.SVR`.
    *   **Hyperparameter Tuning:**
        *   Use `sklearn.model_selection.GridSearchCV` or `RandomizedSearchCV`.
        *   Define a parameter grid for chosen model(s).
        *   Scoring: `neg_root_mean_squared_error` or `neg_mean_squared_error` (and then sqrt).
    *   Train on `X_train, y_train`.
    *   **Model Evaluation (Validation Set):** Best model chosen based on validation RMSE.
    *   Print the best validation RMSE to console.
    *   **Save Best Model:** Use `joblib.dump(best_estimator, 'best_model.joblib')` in the working directory.
*   **4.4.4. Agent Output to `pipeline.py`:** The agent should report overall success or failure (after debug attempts) to `pipeline.py`.
*   **4.4.5. Libraries to specify in prompt for `MODEL.py`:** `pandas`, `numpy`, `sklearn.linear_model`, `sklearn.ensemble`, `sklearn.svm`, `sklearn.model_selection`, `sklearn.metrics`, `joblib`.

### 4.5. `Evaluation_Agent` -> `EVAL.py` Script Guidance:
The prompt for `Evaluation_Agent` in `pipeline.py` must instruct it to generate, execute, debug, and save `EVAL.py`:

*   **4.5.1. Purpose of Task:** The agent's overall task is to produce and validate `EVAL.py`. The `EVAL.py` script itself should load the best model and the feature-engineered test set, make predictions, calculate final RMSE on test data, and save the score.
*   **4.5.2. Input for `EVAL.py` script:**
    *   Feature-engineered test data: `test_featured.csv` (from location specified in 2.3.3.1).
    *   Saved model: `best_model.joblib` (from working directory).
*   **4.5.3. `EVAL.py` Script Functionality (to be requested by the prompt):**
    *   Load `test_featured.csv`.
    *   Load `best_model.joblib` using `joblib.load()`.
    *   Separate features (X_test) from target (y_test).
    *   Make predictions: `y_pred_test = loaded_model.predict(X_test)`.
    *   **RMSE Calculation:** Use `sklearn.metrics.mean_squared_error(y_test, y_pred_test, squared=False)` or `np.sqrt(mean_squared_error(...))`.
    *   **Save Score:** Create/overwrite `MSFT Score.txt` in the working directory.
        *   Content: Single line: `RMSE: <Calculated RMSE float value>` (e.g., `RMSE: 0.002345`). Format float to reasonable precision (e.g., 8 decimal places).
*   **4.5.4. Agent Output to `pipeline.py`:** The agent should report overall success or failure (after debug attempts) to `pipeline.py`.
*   **4.5.5. Libraries to specify in prompt for `EVAL.py`:** `pandas`, `numpy`, `sklearn.metrics`, `joblib`.

## 5. Model Lifecycle (To be enforced by `pipeline.py` orchestration and agent prompts):
    5.1. **Training:** Models trained exclusively on the **Train set** (via `MODEL.py`).
    5.2. **Validation/Tuning:** Model selection and hyperparameter tuning exclusively on the **Validation set** (via `MODEL.py`).
    5.3. **Testing:** Final RMSE reporting exclusively on predictions on the **Test set** (via `EVAL.py`).

## 6. Deliverables of the Project:

### 6.1. Primary Deliverable:
    6.1.1. `pipeline.py`: The Python script that defines, configures (via prompts), and orchestrates the AGNO agent workflow.

### 6.2. Secondary Deliverables (Outputs from executing `pipeline.py`):
    6.2.1. Generated Python Scripts (saved in working directory):
        6.2.1.1. `EDA.py`
        6.2.1.2. `FEATURE.py`
        6.2.1.3. `MODEL.py`
        6.2.1.4. `EVAL.py`
    6.2.2. Data Files (saved in `/data` or working directory as configured):
        6.2.2.1. `train_featured.csv`
        6.2.2.2. `val_featured.csv`
        6.2.2.3. `test_featured.csv`
    6.2.3. Model File (saved in working directory):
        6.2.3.1. `best_model.joblib` (or `.pkl`)
    6.2.4. Performance Score File (saved in working directory):
        6.2.4.1. `MSFT Score.txt` (formatted as `RMSE: <value>`).

## 7. Constraints & Adherence (Governing `pipeline.py` and its managed components):
    7.1. **Agent Naming:** Strict adherence to agent names in `pipeline.py` (`EDA_Agent`, `Feature_Engineering_Agent`, `Modeling_Agent`, `Evaluation_Agent`).
    7.2. **Generated Script Naming:** Strict adherence to output script names (`EDA.py`, `FEATURE.py`, `MODEL.py`, `EVAL.py`).
    7.3. **Prompt-Only Configuration:** AGNO Agents within `pipeline.py` must be configured *only* through prompts.
    7.4. **`MSFT Score.txt` Format:** The format and content must be exactly `RMSE: <float_value>`.
    7.5. **Data Directory:** Input data location will be managed by a hardcoded global variable in `pipeline.py` pointing to a `/data` subdirectory.
    7.6. **Original Requirements Preservation:** All functional details for the generated scripts (`EDA.py`, etc.) from the original version of this document (`msft_log_return_prediction_pipeline_requirements.md`) should be covered by the prompts defined in `pipeline.py` for each AGNO agent.
    7.7. **AGNO Agent Script Lifecycle:** AGNO Agents are responsible for the full lifecycle: generating the Python script, executing it, debugging it (with a specified number of retries), and then saving the final working version to a file in the working directory. The agent's final response to `pipeline.py` must clearly indicate overall success or failure. For the `EDA_Agent`, if successful, its response must also include the synthesized insights string.
    7.8. **Script Execution Environment:** It is assumed that all necessary Python libraries (pandas, scikit-learn, etc.) for the generated scripts are available in the environment where `pipeline.py` is executed, allowing agents to successfully run their generated scripts.