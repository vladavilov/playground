# Implementation Plan: MSFT Next Day Log Return Prediction via Agentic Workflow Pipeline

## 1. Introduction

This document outlines the implementation plan for the `MSFT Next Day Log Return Prediction via Agentic Workflow Pipeline` project. The primary objective is to create a Python script, `pipeline.py`, that leverages the AGNO framework to orchestrate an agent-based workflow. This workflow will predict Microsoft (MSFT) stock's next-day log return by sequentially managing four specialized agents. Each agent is responsible for generating, executing, debugging, and saving a specific Python script: `EDA.py`, `FEATURE.py`, `MODEL.py`, and `EVAL.py`.

This plan is based on the detailed requirements provided in `msft_log_return_prediction_pipeline_requirements.md`.

## 2. High-Level Architecture Overview

The system will be centered around `pipeline.py`, which will contain:

*   A **`LogReturnWorkflow(agno.workflow.Workflow)` class**: This class will encapsulate the entire logic for agent definition, prompt configuration, and sequential orchestration.
*   **Four AGNO Agent Instances**: `EDA_Agent`, `Feature_Engineering_Agent`, `Modeling_Agent`, and `Evaluation_Agent`, instantiated as attributes within the `Workflow` class. Each agent will be configured with an Azure OpenAI model instance.
*   **Master Prompts**: Defined within `pipeline.py` as strings, these prompts will instruct each agent on its specific tasks, including script generation, execution, debugging (with a fixed number of retries, e.g., 3), and saving.
*   **A `build_workflow(azure_model)` function**: This utility function will instantiate and return the `LogReturnWorkflow`, passing the Azure OpenAI model object to it.
*   **A `DATA_DIR` global variable**: Hardcoded to `"data"`, defining the relative path for input CSV files and featured data output.
*   **Orchestration Logic**: Residing in the `Workflow.run()` method, this logic will manage sequential agent invocation, transfer of insights from `EDA_Agent` to `Feature_Engineering_Agent`, basic `stdout` logging, and error handling for agent failures.

The agents, guided by their prompts, will generate the following Python scripts in the working directory:
*   `EDA.py`
*   `FEATURE.py`
*   `MODEL.py`
*   `EVAL.py`

These scripts will perform their respective tasks using data from the `DATA_DIR` and save outputs as specified (featured data to `DATA_DIR`, model and score to the working directory).

## 3. Project Phases and Implementation Steps

### Phase 1: Project Setup & `pipeline.py` Skeleton

*   **[x] Task 1.1: Directory Structure**
    *   Create the main project directory.
    *   Create `src/` directory for `pipeline.py`. Let's assume root for `pipeline.py` for simplicity with output script paths.
*   **[x] Task 1.2: `pipeline.py` Initial Setup**
    *   Create `pipeline.py`.
    *   Define the global `DATA_DIR = "data"`.
    *   Import necessary AGNO modules (`agno.workflow.Workflow`, `agno.agent.Agent`) and other standard libraries (e.g., `os`).
*   **[x] Task 1.3: `LogReturnWorkflow` Class Definition**
    *   Define the `LogReturnWorkflow(agno.workflow.Workflow)` class.
    *   Implement the constructor (`__init__`) to accept an `azure_model` argument.
    *   Inside the constructor, initialize the four agent attributes (`EDA_Agent`, `Feature_Engineering_Agent`, `Modeling_Agent`, `Evaluation_Agent`) as `None` initially. These will be fully instantiated with prompts and the `azure_model` later.
    *   Define placeholders for agent prompt strings within the class or module scope.
*   **[x] Task 1.4: `build_workflow` Function**
    *   Implement the `build_workflow(azure_model)` function that instantiates `LogReturnWorkflow` with the `azure_model` and returns the instance.
*   **[x] Task 1.5: Azure OpenAI Model Integration (Placeholder)**
    *   Add a comment or a placeholder function in `pipeline.py` (e.g., in `if __name__ == "__main__":`) demonstrating how the `azure_model` object is expected to be configured and passed to `build_workflow`. Actual credential management will be external.
    *   The `Workflow` constructor will assign this `azure_model` to each agent instance.
*   **[x] Task 1.6: `Workflow.run()` Method Outline**
    *   Define the `run(self)` method within `LogReturnWorkflow`.
    *   Outline the sequence of agent calls with basic logging print statements (e.g., "Starting EDA_Agent...").
    *   Include `try...except` blocks around each planned agent invocation.

### Phase 2: `EDA_Agent` Implementation and Prompt Engineering

*   **[x] Task 2.1: Define `EDA_Agent` Prompt Components**
    *   In `pipeline.py`, create a new string variable `SHARED_AGENT_DESCRIPTION`. This will contain the general role applicable to all agents that generate and execute Python scripts (e.g., "You are an expert Python programming assistant...").
    *   In `pipeline.py`, define `EDA_AGENT_INSTRUCTIONS`. This variable will detail the tasks for creating the `EDA.py` script, including:
        *   Generation of `EDA.py`.
        *   Functionality of `EDA.py`: Load `train_clean.csv` from `DATA_DIR`, perform summary statistics, missing value analysis, correlation analysis, ADF tests for 'Close' and 'Target_return', and print synthesized insights in a specific format. (Reference section 4.2 of requirements).
        *   Execution of `EDA.py`.
        *   Debugging of `EDA.py` (e.g., up to 3 retries) if execution fails.
        *   Saving the working `EDA.py` to the root directory.
        *   Specification of required libraries for `EDA.py`: `pandas`, `numpy`, `statsmodels`.
    *   In `pipeline.py`, define `EDA_AGENT_EXPECTED_OUTPUT`. This string will describe the exact output the `EDA_Agent` itself should return: the "Synthesized Insights Output" string upon success, or a specific failure signal/message.
*   **[x] Task 2.2: Instantiate `EDA_Agent`**
    *   In the `LogReturnWorkflow.__init__` method, instantiate `self.EDA_Agent = agno.agent.Agent(name="EDA_Agent", model=self.azure_model, description=SHARED_AGENT_DESCRIPTION, instructions=EDA_AGENT_INSTRUCTIONS)`.
*   **[x] Task 2.3: Implement `EDA_Agent` Invocation**
    *   In the `Workflow.run()` method, implement the call to `self.EDA_Agent.run()` (or equivalent AGNO method).
    *   Capture the response, expecting the synthesized insights string.
    *   Log success/failure and the insights. If failure, halt the workflow.

### Phase 3: `Feature_Engineering_Agent` Implementation and Prompt Engineering

*   **[x] Task 3.1: Define `Feature_Engineering_Agent` Base Prompt**
    *   In `pipeline.py`, create `FEATURE_ENGINEERING_AGENT_BASE_PROMPT`.
    *   This prompt must instruct the `Feature_Engineering_Agent` to:
        *   Generate `FEATURE.py`.
        *   Functionality of `FEATURE.py`: Load `train_clean.csv`, `val_clean.csv`, `test_clean.csv` from `DATA_DIR`. Create new features (lags, MAs, volatility, momentum, etc.). Handle missing values (fit on train, apply to all). Optionally apply feature scaling (fit on train, apply to all). Save `train_featured.csv`, `val_featured.csv`, `test_featured.csv` to `DATA_DIR`. (Reference section 4.3).
        *   Execute `FEATURE.py`.
        *   Debug `FEATURE.py` (e.g., up to 3 retries) if execution fails.
        *   Save the working `FEATURE.py` to the root directory.
        *   Return a success/failure signal.
        *   Specify required libraries for `FEATURE.py`: `pandas`, `numpy`, `sklearn.preprocessing`.
*   **[x] Task 3.2: Instantiate `Feature_Engineering_Agent`**
    *   In `LogReturnWorkflow.__init__`, instantiate `self.Feature_Engineering_Agent = agno.agent.Agent(model=self.azure_model, prompt=None, name="Feature_Engineering_Agent")`. The prompt will be set dynamically.
*   **[x] Task 3.3: Implement Dynamic Prompt Adjustment and Invocation**
    *   In `Workflow.run()`, after `EDA_Agent` succeeds:
        *   Dynamically construct the full prompt for `Feature_Engineering_Agent` by incorporating the EDA insights into `FEATURE_ENGINEERING_AGENT_BASE_PROMPT`.
        *   Update the agent's prompt: `self.Feature_Engineering_Agent.prompt = dynamic_fe_prompt` (or pass it during the run call if AGNO supports it).
        *   Invoke `self.Feature_Engineering_Agent.run()`.
        *   Log success/failure. If failure, halt the workflow.

### Phase 4: `Modeling_Agent` Implementation and Prompt Engineering

*   **[x] Task 4.1: Define `Modeling_Agent` Prompt**
    *   In `pipeline.py`, create `MODELING_AGENT_PROMPT`.
    *   Instruct `Modeling_Agent` to:
        *   Generate `MODEL.py`.
        *   Functionality of `MODEL.py`: Load `train_featured.csv`, `val_featured.csv` from `DATA_DIR`. Train various regression models (Linear, Ridge, Lasso, RF, GBR, SVR). Perform hyperparameter tuning (GridSearchCV/RandomizedSearchCV) using validation set (RMSE). Select best model. Print best validation RMSE. Save best model as `best_model.joblib` in the working directory. (Reference section 4.4).
        *   Execute `MODEL.py`.
        *   Debug `MODEL.py` (e.g., up to 3 retries).
        *   Save working `MODEL.py` to root directory.
        *   Return success/failure signal.
        *   Specify libraries for `MODEL.py`: `pandas`, `numpy`, `sklearn.linear_model`, `sklearn.ensemble`, `sklearn.svm`, `sklearn.model_selection`, `sklearn.metrics`, `joblib`.
*   **[x] Task 4.2: Instantiate `Modeling_Agent`**
    *   In `LogReturnWorkflow.__init__`, instantiate `self.Modeling_Agent = agno.agent.Agent(model=self.azure_model, prompt=MODELING_AGENT_PROMPT, name="Modeling_Agent")`.
*   **[x] Task 4.3: Implement `Modeling_Agent` Invocation**
    *   In `Workflow.run()`, invoke `self.Modeling_Agent.run()`.
    *   Log success/failure. If failure, halt the workflow.

### Phase 5: `Evaluation_Agent` Implementation and Prompt Engineering

*   **[x] Task 5.1: Define `Evaluation_Agent` Prompt**
    *   In `pipeline.py`, create `EVALUATION_AGENT_PROMPT`.
    *   Instruct `Evaluation_Agent` to:
        *   Generate `EVAL.py`.
        *   Functionality of `EVAL.py`: Load `test_featured.csv` (from `DATA_DIR`) and `best_model.joblib` (from working directory). Make predictions. Calculate RMSE on test data. Save score to `MSFT Score.txt` in working directory (format: `RMSE: <float_value>`). (Reference section 4.5).
        *   Execute `EVAL.py`.
        *   Debug `EVAL.py` (e.g., up to 3 retries).
        *   Save working `EVAL.py` to root directory.
        *   Return success/failure signal.
        *   Specify libraries for `EVAL.py`: `pandas`, `numpy`, `sklearn.metrics`, `joblib`.
*   **[x] Task 5.2: Instantiate `Evaluation_Agent`**
    *   In `LogReturnWorkflow.__init__`, instantiate `self.Evaluation_Agent = agno.agent.Agent(model=self.azure_model, prompt=EVALUATION_AGENT_PROMPT, name="Evaluation_Agent")`.
*   **[x] Task 5.3: Implement `Evaluation_Agent` Invocation**
    *   In `Workflow.run()`, invoke `self.Evaluation_Agent.run()`.
    *   Log success/failure.

### Phase 6: Finalize Workflow Orchestration, Error Handling, and Logging

*   **[x] Task 6.1: Complete `Workflow.run()` Logic**
    *   Ensure all agent calls are sequential and correctly chained.
    *   Verify that EDA insights are passed correctly.
    *   Implement robust `try...except` blocks for each agent call, logging errors to `stdout` and halting the workflow on agent-reported failure.
*   **[x] Task 6.2: Implement Main Execution Block**
    *   In `pipeline.py`, add `if __name__ == "__main__":` block.
    *   Include example setup for `azure_model` (as discussed in Phase 1.5).
    *   Instantiate the workflow: `workflow = build_workflow(azure_model)`.
    *   Run the workflow: `workflow.run()`.
    *   Add a final print statement indicating workflow completion or failure.

### Phase 7: Testing and Refinement

*   **[x] Task 7.1: Unit Testing (Conceptual)**
    *   While full unit tests for LLM outputs are complex, test the structural integrity of `pipeline.py` (e.g., class instantiation, method calls).
*   **[x] Task 7.2: Integration Testing (Mocked LLM)**
    *   If feasible, create a mock AGNO Agent or mock Azure OpenAI model that returns predefined script content or success/failure signals to test the orchestration logic in `pipeline.py` without actual LLM calls.
*   **[x] Task 7.3: End-to-End Testing (Actual LLM)**
    *   Execute `pipeline.py` with a configured Azure OpenAI model.
    *   Verify:
        *   Correct sequential execution of agents.
        *   Generation of all four Python scripts (`EDA.py`, `FEATURE.py`, `MODEL.py`, `EVAL.py`) in the working directory.
        *   Successful execution of these generated scripts by the agents.
        *   Debugging attempts by agents (observe logs/agent behavior if possible).
        *   Correct creation and content of output data files: `train_featured.csv`, `val_featured.csv`, `test_featured.csv` in `DATA_DIR`.
        *   Correct creation of `best_model.joblib` in the working directory.
        *   Correct creation and format of `MSFT Score.txt` in the working directory.
        *   Adherence to all file naming and location conventions.
*   **[x] Task 7.4: Prompt Refinement**
    *   Based on testing, iterate on the agent prompts in `pipeline.py` to improve the quality, correctness, and robustness of the generated scripts. This is expected to be an iterative process.
    *   Ensure generated scripts are independently executable as per requirement 4.1.5.1.

### Phase 8: Documentation and Final Review

*   **[x] Task 8.1: Code Comments**
    *   Add clear, concise comments to `pipeline.py` explaining the `Workflow` class, agent configurations, prompt strategies, and orchestration logic.
*   **[x] Task 8.2: README Update**
    *   Update or create a `README.md` for the project, explaining how to set up the environment (dependencies, Azure OpenAI configuration via environment variables), and how to run `pipeline.py`.
*   **[x] Task 8.3: Final Review**
    *   Review all code and documentation against the original requirements (`msft_log_return_prediction_pipeline_requirements.md`) to ensure all constraints and deliverables are met.
    *   Verify adherence to naming conventions for agents and scripts.

## 4. Key Technologies & Dependencies

*   **Python 3.x**
*   **AGNO Framework**
*   **Azure OpenAI Service SDK** (or compatible library for interacting with the model)
*   **Pandas**: For data manipulation in generated scripts.
*   **NumPy**: For numerical operations in generated scripts.
*   **Scikit-learn**: For machine learning tasks (preprocessing, models, metrics, model selection) in generated scripts.
*   **Statsmodels**: For statistical tests (e.g., ADF test) in `EDA.py`.
*   **Joblib**: For saving/loading the trained model.

## 5. Success Criteria

*   `pipeline.py` successfully orchestrates the four agents in sequence.
*   Each agent successfully generates, executes (with debugging if necessary), and saves its respective Python script (`EDA.py`, `FEATURE.py`, `MODEL.py`, `EVAL.py`) in the working directory.
*   The `EDA_Agent` successfully returns synthesized insights, which are then used to guide the `Feature_Engineering_Agent`.
*   All specified output data files (`train_featured.csv`, `val_featured.csv`, `test_featured.csv`) are correctly generated in the `DATA_DIR`.
*   `best_model.joblib` is saved in the working directory.
*   `MSFT Score.txt` is created in the working directory with the correct format (`RMSE: <float_value>`).
*   The workflow handles agent failures gracefully (logs error, halts).
*   All constraints from the requirements document are met.

This implementation plan provides a structured approach to developing the agentic workflow. Flexibility will be required, especially during prompt engineering and testing phases. 