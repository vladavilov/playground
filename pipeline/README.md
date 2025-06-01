import os
import json
import ast
import argparse

# --- Configuration ---

# Path to the pipeline.py file containing the shared agent description
PIPELINE_FILE_PATH = "pipeline.py"
# Name of the variable in pipeline.py that holds the shared description
SHARED_AGENT_DESCRIPTION_VAR = "SHARED_AGENT_DESCRIPTION"

# Directory containing the individual agent Python files
AGENTS_DIR = "agents"
# Name of the output JSON file
OUTPUT_JSON_FILE = "agent_prompts.json"

# Configuration mapping agent script filenames to their prompt variable names and JSON keys
AGENT_CONFIG = {
    "eda_agent.py": {
        "json_key": "EDA_Agent",
        "instructions_var": "EDA_AGENT_INSTRUCTIONS",
        "expected_outcome_var": "EDA_AGENT_EXPECTED_OUTPUT"
    },
    "feature_engineering_agent.py": {
        "json_key": "Featuring_Agent",  # As per user's requested JSON structure
        "instructions_var": "FEATURE_ENGINEERING_AGENT_BASE_PROMPT",
        "expected_outcome_var": "FEATURE_ENGINEERING_AGENT_EXPECTED_OUTPUT"
    },
    "modeling_agent.py": {
        "json_key": "Modeling_Agent",
        "instructions_var": "MODELING_AGENT_INSTRUCTIONS",
        "expected_outcome_var": "MODELING_AGENT_EXPECTED_OUTPUT"
    },
    "evaluation_agent.py": {
        "json_key": "Eval_Agent",
        "instructions_var": "EVALUATION_AGENT_INSTRUCTIONS",
        "expected_outcome_var": "EVALUATION_AGENT_EXPECTED_OUTPUT"
    }
}

# --- Helper Function ---

def extract_variable_from_file(filepath, variable_name):
    """
    Extracts the string value of a top-level variable from a Python file.

    Args:
        filepath (str): The path to the Python file.
        variable_name (str): The name of the variable to extract.

    Returns:
        str: The string value of the variable.

    Raises:
        FileNotFoundError: If the filepath does not exist.
        SyntaxError: If the file has syntax errors.
        ValueError: If the variable is not found, not a top-level assignment,
                    or not a simple string.
    """
    if not os.path.exists(filepath):
        raise FileNotFoundError(f"File not found: {filepath}")

    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    try:
        tree = ast.parse(content, filename=filepath)
    except SyntaxError as e:
        raise SyntaxError(f"Syntax error in file {filepath}: {e}") from e

    for node in tree.body:
        if isinstance(node, ast.Assign):
            for target in node.targets:
                if isinstance(target, ast.Name) and target.id == variable_name:
                    # Prefer ast.Constant for Python 3.8+
                    if isinstance(node.value, ast.Constant) and isinstance(node.value.value, str):
                        return node.value.value
                    # Fallback for ast.Str (older Python versions, deprecated in 3.8)
                    elif hasattr(ast, 'Str') and isinstance(node.value, ast.Str) and isinstance(node.value.s, str):
                        return node.value.s # pragma: no cover (for py3.8+)
                    else:
                        raise ValueError(
                            f"Variable '{variable_name}' in '{filepath}' is not a simple string assignment. "
                            f"Found type: {type(node.value)}"
                        )
    raise ValueError(f"Variable '{variable_name}' not found or not a top-level assignment in '{filepath}'.")

# --- Main Logic ---

def generate_prompts(pipeline_file, shared_var, agents_dir, config, output_file):
    """
    Generates a JSON file containing merged prompts for each configured agent.

    Args:
        pipeline_file (str): Path to the pipeline.py file.
        shared_var (str): Name of the shared description variable.
        agents_dir (str): Path to the directory containing agent scripts.
        config (dict): Configuration mapping agent files to their details.
        output_file (str): Path to the output JSON file.
    
    Returns:
        bool: True if successful, False otherwise.
    """
    print(f"Starting prompt generation...")
    print(f"Pipeline file: {pipeline_file}")
    print(f"Agents directory: {agents_dir}")
    print(f"Output JSON: {output_file}")

    if not os.path.exists(pipeline_file):
        print(f"Error: Pipeline file '{pipeline_file}' not found.")
        return False

    if not os.path.isdir(agents_dir):
        print(f"Error: Agents directory '{agents_dir}' not found.")
        return False

    all_prompts_data = {}

    try:
        print(f"Extracting shared prompt variable '{shared_var}' from '{pipeline_file}'...")
        shared_prompt_template = extract_variable_from_file(pipeline_file, shared_var)
        print("Shared prompt extracted successfully.")
    except (FileNotFoundError, ValueError, SyntaxError) as e:
        print(f"Error extracting shared prompt: {e}")
        return False
    except Exception as e:
        print(f"An unexpected error occurred while extracting shared prompt: {e}")
        return False


    print(f"Processing agent files from '{agents_dir}'...")
    for agent_filename_key, agent_details in config.items():
        agent_py_path = os.path.join(agents_dir, agent_filename_key)
        json_key = agent_details["json_key"]
        instructions_var = agent_details["instructions_var"]
        expected_outcome_var = agent_details["expected_outcome_var"]

        print(f"Processing agent: {agent_filename_key} (JSON key: {json_key})")

        if not os.path.exists(agent_py_path):
            print(f"Warning: Agent file '{agent_py_path}' not found. Skipping.")
            continue

        try:
            print(f"  Extracting instructions variable '{instructions_var}'...")
            instructions = extract_variable_from_file(agent_py_path, instructions_var)
            print(f"  Extracting expected outcome variable '{expected_outcome_var}'...")
            expected_outcome = extract_variable_from_file(agent_py_path, expected_outcome_var)

            # Ensure all parts are strings before concatenation
            shared_prompt_template_str = str(shared_prompt_template)
            instructions_str = str(instructions)
            expected_outcome_str = str(expected_outcome)

            merged_prompt = f"{shared_prompt_template_str}\n\n{instructions_str}\n\n{expected_outcome_str}"
            all_prompts_data[json_key] = {"prompt": merged_prompt}
            print(f"  Successfully processed and merged prompts for {json_key}.")

        except (FileNotFoundError, ValueError, SyntaxError) as e:
            print(f"Warning: Error processing agent file {agent_py_path}: {e}. Skipping this agent.")
            continue # Skip to the next agent if an error occurs for this one
        except Exception as e:
            print(f"An unexpected error occurred while processing {agent_py_path}: {e}. Skipping this agent.")
            continue


    try:
        print(f"Writing merged prompts to '{output_file}'...")
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(all_prompts_data, f, indent=2, ensure_ascii=False) # Using indent=2 as per your example
        print(f"Successfully generated prompts to {output_file}")
        return True
    except IOError as e:
        print(f"Error writing JSON file '{output_file}': {e}")
        return False
    except Exception as e:
        print(f"An unexpected error occurred while writing JSON output: {e}")
        return False

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Generate a JSON file with combined agent prompts.")
    parser.add_argument(
        "--pipeline_file",
        default=PIPELINE_FILE_PATH,
        help=f"Path to the pipeline Python file (default: {PIPELINE_FILE_PATH})"
    )
    parser.add_argument(
        "--shared_var",
        default=SHARED_AGENT_DESCRIPTION_VAR,
        help=f"Name of the shared agent description variable (default: {SHARED_AGENT_DESCRIPTION_VAR})"
    )
    parser.add_argument(
        "--agents_dir",
        default=AGENTS_DIR,
        help=f"Directory containing agent Python files (default: {AGENTS_DIR})"
    )
    parser.add_argument(
        "--output_file",
        default=OUTPUT_JSON_FILE,
        help=f"Path to the output JSON file (default: {OUTPUT_JSON_FILE})"
    )

    args = parser.parse_args()

    success = generate_prompts(
        args.pipeline_file,
        args.shared_var,
        args.agents_dir,
        AGENT_CONFIG,
        args.output_file
    )

    if success:
        print("Prompt generation completed successfully.")
    else:
        print("Prompt generation failed. Check logs for details.")
        exit(1) 
        
        
        
        *   **Finding 1:** [Concise summary of the most critical insight, e.g., "The dataset is relatively clean with less than 2% missing values, primarily in the `Demographics` column."]
*   **Finding 2:** [e.g., "`Variable X` is the strongest individual predictor of `Target Y` based on correlation and initial bivariate tests."]
*   **Finding 3:** [e.g., "Significant outliers were detected in `TransactionAmount`, potentially skewing analyses if not addressed."]
*   **Finding 4:** [e.g., "Categorical feature `Region` shows a notable disparity in the distribution of the target variable, suggesting regional strategies might be effective."]
*   **Finding 5:** [e.g., "No strong multicollinearity was observed among the primary numeric features."]

# MSFT Next Day Log Return Prediction via Agentic Workflow Pipeline

This project implements a pipeline to predict the next-day log return for Microsoft (MSFT) stock. It utilizes the AGNO framework to orchestrate a series of agents, each responsible for a specific part of the machine learning workflow: Exploratory Data Analysis (EDA), Feature Engineering, Modeling, and Evaluation.

## Project Structure

```
.
├── data/                     # Directory for input data (e.g., train_clean.csv) and generated features
├── pipeline.py               # Main script to run the agentic workflow
├── EDA.py                    # Generated by EDA_Agent for exploratory data analysis
├── FEATURE.py                # Generated by Feature_Engineering_Agent for feature creation
├── MODEL.py                  # Generated by Modeling_Agent for model training
├── EVAL.py                   # Generated by Evaluation_Agent for model evaluation
├── best_model.joblib         # Saved trained model
├── MSFT Score.txt            # Output file with the model's RMSE score
├── requirements.txt          # Python dependencies
└── README.md                 # This file
```

## Prerequisites

*   Python 3.x
*   Access to Azure OpenAI Service (for the agents)

## Setup

1.  **Clone the repository (if applicable):**
    ```bash
    git clone <repository_url>
    cd <project_directory>
    ```

2.  **Create and activate a virtual environment (recommended):**
    ```bash
    python -m venv .venv
    # On Windows
    .venv\Scripts\activate
    # On macOS/Linux
    source .venv/bin/activate
    ```

3.  **Install dependencies:**
    ```bash
    pip install -r requirements.txt
    ```
    The `requirements.txt` file should include:
    ```
    agno
    pandas
    numpy
    scikit-learn
    statsmodels
    joblib
    # Add any other specific versions if necessary, e.g.:
    # openai
    # python-dotenv
    ```

4.  **Configure Azure OpenAI Environment Variables:**
    The `pipeline.py` script (and the AGNO framework underpinning it) will require Azure OpenAI credentials to function. Set the following environment variables in your system or in a `.env` file (if using `python-dotenv` and integrating it into `pipeline.py` for model loading - though the current `pipeline.py` uses a mock model):

    *   `AZURE_OPENAI_API_KEY`: Your Azure OpenAI API key.
    *   `AZURE_OPENAI_ENDPOINT`: Your Azure OpenAI endpoint URL.
    *   `AZURE_OPENAI_DEPLOYMENT_NAME`: The name of your Azure OpenAI model deployment (e.g., gpt-4, gpt-35-turbo).
    *   `AZURE_OPENAI_API_VERSION`: The API version for Azure OpenAI (e.g., "2023-07-01-preview").

    If you are using a `.env` file, make sure it's in the root directory of the project and looks like this:
    ```
    AZURE_OPENAI_API_KEY="your_api_key"
    AZURE_OPENAI_ENDPOINT="your_endpoint_url"
    AZURE_OPENAI_DEPLOYMENT_NAME="your_deployment_name"
    AZURE_OPENAI_API_VERSION="your_api_version"
    ```
    *(Note: The current `pipeline.py` uses a mock object for the Azure model. To use a real Azure OpenAI model, you would need to modify the `run_pipeline_main_logic` function in `pipeline.py` to instantiate and configure `agno.models.azure.AzureOpenAIModel` or a similar client, and ensure these environment variables are loaded and used.)*

## Running the Pipeline

1.  **Prepare Data:**
    Ensure your input data files (e.g., `train_clean.csv`, `val_clean.csv`, `test_clean.csv`) are present in the `data/` directory. The pipeline expects these files to be in CSV format.

2.  **Execute the pipeline:**
    Run the `pipeline.py` script from the root directory of the project:
    ```bash
    python pipeline.py
    ```

## Workflow Overview

The `pipeline.py` script orchestrates four main agents:

1.  **`EDA_Agent`**:
    *   Generates and executes `EDA.py`.
    *   Performs exploratory data analysis on `train_clean.csv`.
    *   Outputs synthesized insights that are passed to the next agent.
    *   Saves `EDA.py`.

2.  **`Feature_Engineering_Agent`**:
    *   Receives insights from `EDA_Agent`.
    *   Generates and executes `FEATURE.py`.
    *   Loads `train_clean.csv`, `val_clean.csv`, `test_clean.csv`.
    *   Creates new features based on EDA insights and predefined strategies.
    *   Handles missing values and applies feature scaling.
    *   Saves `train_featured.csv`, `val_featured.csv`, `test_featured.csv` to the `data/` directory.
    *   Saves `FEATURE.py`.

3.  **`Modeling_Agent`**:
    *   Generates and executes `MODEL.py`.
    *   Loads `train_featured.csv` and `val_featured.csv`.
    *   Trains one or more regression models (e.g., XGBoost, LightGBM, MLPRegressor).
    *   Performs hyperparameter tuning (optional, based on script generation).
    *   Selects the best model based on validation RMSE.
    *   Saves the best model as `best_model.joblib` in the working directory.
    *   Saves `MODEL.py`.

4.  **`Evaluation_Agent`**:
    *   Generates and executes `EVAL.py`.
    *   Loads `test_featured.csv` and `best_model.joblib`.
    *   Makes predictions on the test set.
    *   Calculates RMSE.
    *   Saves the RMSE to `MSFT Score.txt` in the format `RMSE: <float_value>`.
    *   Saves `EVAL.py`.

## Expected Outputs

Upon successful execution, the pipeline will produce:

*   **Generated Scripts**: `EDA.py`, `FEATURE.py`, `MODEL.py`, `EVAL.py` in the root directory.
*   **Featured Data**: `train_featured.csv`, `val_featured.csv`, `test_featured.csv` in the `data/` directory.
*   **Trained Model**: `best_model.joblib` in the root directory.
*   **Evaluation Score**: `MSFT Score.txt` in the root directory, containing the RMSE.

## Troubleshooting

*   **Agent Failures**: The pipeline includes basic error handling. If an agent fails, the workflow will halt, and an error message will be printed. Check the console output for details. The agents have a built-in retry mechanism for debugging generated scripts.
*   **Dependency Issues**: Ensure all packages in `requirements.txt` are correctly installed in your active Python environment.
*   **Azure OpenAI Configuration**: If using a real Azure model, double-check that your environment variables are correctly set and that your Azure OpenAI service is active and accessible.
*   **Data Files**: Verify that the input CSV files are correctly named and located in the `data/` directory.
