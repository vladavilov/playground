DATA_DIR = "data"
import os
import json # Added for JSON operations
from agno.workflow import Workflow
from agno.run.response import RunResponse

from agents.eda_agent import create_eda_agent
from agents.feature_engineering_agent import create_feature_engineering_agent, FEATURE_ENGINEERING_AGENT_BASE_PROMPT
from agents.modeling_agent import create_modeling_agent
from agents.evaluation_agent import create_evaluation_agent

SHARED_AGENT_DESCRIPTION = """## Persona
You are an autonomous, expert Python AI. Your **sole mission**: generate **correct Python code** that **strictly adheres to all provided requirements**. Operate with extreme precision, diligence, and apply best practices supporting correctness and maintainability. Prioritize **functional correctness** and **requirement fulfillment**.

## Autonomous Workflow

### 1. Understand Requirements & Plan for Correctness
*   **Analyze Requirements**: Thoroughly analyze instructions and project context, focusing on **all requirements** for a complete understanding of expected correct behavior and outputs.
*   **Resolve Ambiguities**: If ambiguity critical to correct implementation persists after checking existing code/context, document the impasse and halt. Otherwise, proceed with the most reasonable interpretation supporting requirement fulfillment, documenting assumptions.
*   **Strategic Code Reuse**: Search for reusable code *only if it precisely meets requirements and contributes to a correct solution*.
*   **Requirement-Driven Plan**: Detail sub-tasks and code changes, explicitly linking each to specific requirements and how correctness will be achieved. No external confirmation is sought.
*   **Anticipate Enhancements**: Proactively identify improvements for subsequent iterations that enhance correctness or requirement coverage.

### 2. Test-Driven Development (TDD) with `pytest`
*   **Write Failing `pytest` Tests First**: Develop comprehensive, isolated Python tests **using `pytest` exclusively**. Tests must define expected behavior per requirements, covering normal/edge cases and verifying functional correctness.
*   **Verify `pytest` Test Failure**: Execute new `pytest` tests to confirm they fail as expected. If not, revise tests to accurately reflect requirements and re-verify failure before implementation.

### 3. Implement & Verify Correct Solution
*   **Write Minimal Code to Pass Tests (Post-Verification)**:
    *   **Dependencies**: If new packages are required for a correct solution, install and verify them.
    *   **Targeted Implementation**: If correct reusable code was identified, adapt it. Otherwise, write minimal new Python code **to pass verified failing `pytest` tests and fulfill requirements**.
*   **Uphold Best Practices for Correctness**:
    *   **DRY Principle**.
    *   **Pythonic & Clear Code**: PEP 8, modular design, docstrings, type hinting for readability and error reduction.
    *   **Code Style Consistency**: Strictly follow project style; pass linters or replicate existing style.
*   **Verify Implementation Correctness**: Execute `pytest` tests. Confirm all pass, indicating the implementation correctly meets tested requirements. Debug until correctness is achieved.
*   **Scope Management**: Use `TODO` comments for out-of-scope logic, noting impact on correctness or requirement coverage.

### 4. Validate Increment
*   **Comprehensive `pytest` Testing**: Run all `pytest` tests (unit, integration) ensuring no regressions and functional correctness.
*   **Proceed on Green & Correct**: Advance only when all `pytest` tests pass and features are confirmed correct and meet requirements.
*   Internally document changes, confirming how acceptance criteria for correctness and requirements are met.

### 5. Iterate Towards Full Correctness
*   Select the next task based on project goals, focusing on achieving full requirement coverage and system correctness.
*   Repeat cycle: "Understand Requirements & Plan" -> "Validate Increment."

### 6. Final Correctness & Requirement Review (Before Major Task Completion)
*   Conduct internal review, focusing on:
    *   **Functional Correctness**: Code is robust, error-free, and performs per requirements.
    *   **Requirement Adherence**: **All requirements are verifiably addressed and correctly implemented.**
    *   **Supportive Best Practices**: Adherence to practices and style supporting long-term correctness.
    *   **No Duplication**.
    *   **Validation Summary**: Note outcomes, confirming overall correctness and requirement satisfaction.
"""

# Defines the main workflow for the MSFT log return prediction pipeline.
# This workflow orchestrates multiple agents to perform tasks such as EDA, feature engineering, modeling, and evaluation.
class LogReturnWorkflow(Workflow):
    # Initializes the LogReturnWorkflow.
    # Args:
    # azure_model: The Azure OpenAI model instance to be used by the agents.
    def __init__(self, azure_model):
        super().__init__()
        self.azure_model = azure_model # Stores the Azure OpenAI model instance.
        self.agent_prompts = {}
        self.agent_run_logs = {} # Stores a list of strings for each agent's log

        # EDA_Agent: Responsible for performing exploratory data analysis.
        self.EDA_Agent = create_eda_agent(
            model=self.azure_model,
            description=SHARED_AGENT_DESCRIPTION
        )
        # Feature_Engineering_Agent: Responsible for creating features from the data.
        # Its instructions are set dynamically in the run() method based on EDA insights.
        self.Feature_Engineering_Agent = create_feature_engineering_agent(
            model=self.azure_model,
            description=SHARED_AGENT_DESCRIPTION
        )
        # Modeling_Agent: Responsible for training a predictive model.
        self.Modeling_Agent = create_modeling_agent(
            model=self.azure_model,
            description=SHARED_AGENT_DESCRIPTION
        )
        # Evaluation_Agent: Responsible for evaluating the trained model.
        self.Evaluation_Agent = create_evaluation_agent(
            model=self.azure_model,
            description=SHARED_AGENT_DESCRIPTION
        )
        self.eda_insights = None # Attribute to store insights from the EDA_Agent.

    def _get_agent_whole_prompt(self, agent):
        desc_str = str(agent.description) if agent.description is not None else "No description provided."
        instr_str = str(agent.instructions) if agent.instructions is not None else "No instructions provided (may be set dynamically)."
        outcome_str = str(agent.expected_output) if agent.expected_output is not None else "No expected outcome provided."
        
        return f"Description:\\n{desc_str}\\n\\nInstructions:\\n{instr_str}\\n\\nExpected Outcome:\\n{outcome_str}"

    # Runs the entire workflow by invoking agents sequentially.
    # Yields:
    # RunResponse: Objects indicating the status or output of each step.
    def run(self):
        # This method orchestrates the sequential execution of agents: EDA, Feature Engineering, Modeling, and Evaluation.
        # It captures insights from the EDA agent and passes them to the Feature Engineering agent.
        # It also captures prompts and logs for each agent.
        # The method yields RunResponse objects to provide real-time feedback on the workflow's progress and handles agent failures by halting the workflow.

        # --- EDA Agent ---
        agent_obj = self.EDA_Agent
        agent_placeholder_name = "EDA_Agent" # Used if agent_obj is None

        if agent_obj:
            agent_name = agent_obj.name
            self.agent_prompts[agent_name] = self._get_agent_whole_prompt(agent_obj)
            self.agent_run_logs[agent_name] = []
            current_log_list = self.agent_run_logs[agent_name]

            message = f"Starting {agent_name}..."
            current_log_list.append(message)
            yield RunResponse(content=message)
            
            try:
                eda_agent_response = agent_obj.run()
                response_content_str = str(eda_agent_response.content) if eda_agent_response and hasattr(eda_agent_response, 'content') else "N/A"
                current_log_list.append(f"{agent_name} Raw Response: {response_content_str}")

                status_message = "" 
                if eda_agent_response and hasattr(eda_agent_response, 'content') and eda_agent_response.content and not str(eda_agent_response.content).startswith("FAILURE:"):
                    self.eda_insights = eda_agent_response.content
                    status_message = f"{agent_name} finished successfully. Insights captured: {self.eda_insights}"
                elif eda_agent_response and hasattr(eda_agent_response, 'content') and eda_agent_response.content and str(eda_agent_response.content).startswith("FAILURE:"):
                    status_message = f"{agent_name} failed. Halting workflow. Response: {eda_agent_response.content}"
                else:
                    response_content_for_msg = eda_agent_response.content if eda_agent_response and hasattr(eda_agent_response, 'content') else "No/Malformed response"
                    status_message = f"{agent_name} failed. Halting workflow. Response: {response_content_for_msg}"
                
                current_log_list.append(status_message)
                yield RunResponse(content=status_message)
                if "failed." in status_message or "halting" in status_message.lower():
                    return 
            except Exception as e:
                error_message = f"Error during {agent_name} execution: {e}"
                current_log_list.append(error_message)
                yield RunResponse(content=error_message)
                raise
        else: 
            message = f"{agent_placeholder_name} not initialized. Halting workflow."
            self.agent_prompts[agent_placeholder_name] = "Agent not initialized."
            self.agent_run_logs[agent_placeholder_name] = [message]
            yield RunResponse(content=message)
            return

        # --- Feature Engineering Agent ---
        agent_obj = self.Feature_Engineering_Agent
        agent_placeholder_name = "Feature_Engineering_Agent"

        if agent_obj:
            agent_name = agent_obj.name
            # Prompt for FE is set after EDA insights, so capture it just before run
            self.agent_run_logs[agent_name] = [] 
            current_log_list = self.agent_run_logs[agent_name]
            
            message = f"Starting {agent_name}..."
            current_log_list.append(message)
            yield RunResponse(content=message)

            try:
                dynamic_fe_instructions = f"{FEATURE_ENGINEERING_AGENT_BASE_PROMPT}\\n\\n--- EDA Insights for Feature Engineering ---\\n{self.eda_insights if self.eda_insights else 'No EDA insights available.'}"
                agent_obj.instructions = dynamic_fe_instructions 
                self.agent_prompts[agent_name] = self._get_agent_whole_prompt(agent_obj) # Capture finalized prompt
                
                fe_response = agent_obj.run()
                response_content_str = str(fe_response.content) if fe_response and hasattr(fe_response, 'content') else "N/A"
                current_log_list.append(f"{agent_name} Raw Response: {response_content_str}")

                status_message = ""
                if fe_response and hasattr(fe_response, 'content') and isinstance(fe_response.content, str):
                    if fe_response.content == "SUCCESS":
                        status_message = f"{agent_name} finished successfully."
                    elif fe_response.content.startswith("FAILURE:"):
                        status_message = f"{agent_name} failed. Halting workflow. Response: {fe_response.content}"
                    else:
                        status_message = f"{agent_name} returned unexpected content. Halting workflow. Response: {fe_response.content}"
                else:
                    response_content_for_msg = str(fe_response.content if fe_response and hasattr(fe_response, 'content') else fe_response)
                    status_message = f"{agent_name} did not return a valid response. Halting workflow. Response: {response_content_for_msg}"
                
                current_log_list.append(status_message)
                yield RunResponse(content=status_message)
                if "failed." in status_message or "halting" in status_message.lower():
                    return
            except Exception as e:
                error_message = f"Error during {agent_name} execution: {str(e)}"
                current_log_list.append(error_message)
                yield RunResponse(content=error_message)
                raise
        else:
            message = f"{agent_placeholder_name} not initialized. Skipping and Halting."
            self.agent_prompts[agent_placeholder_name] = "Agent not initialized."
            self.agent_run_logs[agent_placeholder_name] = [message]
            yield RunResponse(content=message)
            return

        # --- Modeling Agent ---
        agent_obj = self.Modeling_Agent
        agent_placeholder_name = "Modeling_Agent"
        
        if agent_obj:
            agent_name = agent_obj.name
            self.agent_prompts[agent_name] = self._get_agent_whole_prompt(agent_obj)
            self.agent_run_logs[agent_name] = []
            current_log_list = self.agent_run_logs[agent_name]
            
            message = f"Starting {agent_name}..."
            current_log_list.append(message)
            yield RunResponse(content=message)
            
            try:
                model_response = agent_obj.run()
                response_content_str = str(model_response.content) if model_response and hasattr(model_response, 'content') else "N/A"
                current_log_list.append(f"{agent_name} Raw Response: {response_content_str}")

                status_message = ""
                if model_response and hasattr(model_response, 'content') and isinstance(model_response.content, str):
                    if model_response.content == "SUCCESS": 
                        status_message = f"{agent_name} finished successfully."
                    elif model_response.content.startswith("FAILURE:"):
                        status_message = f"{agent_name} failed. Halting workflow. Response: {model_response.content}"
                    else:
                        status_message = f"{agent_name} returned unexpected content. Halting workflow. Response: {model_response.content}"
                else:
                    response_content_for_msg = str(model_response.content if model_response and hasattr(model_response, 'content') else model_response)
                    status_message = f"{agent_name} did not return a valid response. Halting workflow. Response: {response_content_for_msg}"

                current_log_list.append(status_message)
                yield RunResponse(content=status_message)
                if "failed." in status_message or "halting" in status_message.lower():
                    return
            except Exception as e:
                error_message = f"Error during {agent_name} execution: {str(e)}"
                current_log_list.append(error_message)
                yield RunResponse(content=error_message)
                raise
        else:
            message = f"{agent_placeholder_name} not initialized. Skipping and Halting."
            self.agent_prompts[agent_placeholder_name] = "Agent not initialized."
            self.agent_run_logs[agent_placeholder_name] = [message]
            yield RunResponse(content=message)
            return

        # --- Evaluation Agent ---
        agent_obj = self.Evaluation_Agent
        agent_placeholder_name = "Evaluation_Agent"

        if agent_obj:
            agent_name = agent_obj.name
            self.agent_prompts[agent_name] = self._get_agent_whole_prompt(agent_obj)
            self.agent_run_logs[agent_name] = []
            current_log_list = self.agent_run_logs[agent_name]

            message = f"Starting {agent_name}..."
            current_log_list.append(message)
            yield RunResponse(content=message)

            try:
                eval_response = agent_obj.run()
                response_content_str = str(eval_response.content) if eval_response and hasattr(eval_response, 'content') else "N/A"
                current_log_list.append(f"{agent_name} Raw Response: {response_content_str}")
                
                status_message = ""
                if eval_response and hasattr(eval_response, 'content') and isinstance(eval_response.content, str):
                    if eval_response.content == "SUCCESS": 
                        status_message = f"{agent_name} finished successfully."
                    elif eval_response.content.startswith("FAILURE:"):
                        status_message = f"{agent_name} failed. Halting workflow. Response: {eval_response.content}"
                    else:
                        status_message = f"{agent_name} returned unexpected content. Halting workflow. Response: {eval_response.content}"
                else:
                    response_content_for_msg = str(eval_response.content if eval_response and hasattr(eval_response, 'content') else eval_response)
                    status_message = f"{agent_name} did not return a valid response. Halting workflow. Response: {response_content_for_msg}"

                current_log_list.append(status_message)
                yield RunResponse(content=status_message)
                if "failed." in status_message or "halting" in status_message.lower():
                    return
            except Exception as e:
                error_message = f"Error during {agent_name} execution: {str(e)}"
                current_log_list.append(error_message)
                yield RunResponse(content=error_message)
                raise
        else:
            message = f"{agent_placeholder_name} not initialized. Skipping. Halting workflow as this is a critical step."
            self.agent_prompts[agent_placeholder_name] = "Agent not initialized." # Ensure these are set for JSON summary
            self.agent_run_logs[agent_placeholder_name] = [message]
            yield RunResponse(content=message)
            return
        
        yield RunResponse(content="Workflow finished successfully.")

# Builds and returns an instance of the LogReturnWorkflow.
# Args:
# azure_model: The Azure OpenAI model instance to be used by the workflow's agents.
# Returns:
# LogReturnWorkflow: An instance of the LogReturnWorkflow.
def build_workflow(azure_model):
    workflow = LogReturnWorkflow(azure_model)
    return workflow

# Main logic for running the MSFT log return prediction pipeline.
# This function sets up a mock Azure OpenAI model, builds the workflow, and runs it.
# It prints the progress and status messages yielded by the workflow and saves a JSON summary.
def run_pipeline_main_logic():
   
    # Using a simple object as a placeholder for the Azure OpenAI model.
    # In a real application, this would be an instance of the actual model client.
    mock_azure_model = object() 

    print("Building workflow...")
    # Build the workflow instance, passing the (mock) Azure model.
    workflow = build_workflow(azure_model=mock_azure_model) 
    print("Workflow built.")
    
    print("Running workflow...")
    workflow_console_log = [] # To capture what's printed to console during workflow run
    final_status_message = "Main block: Workflow started."
    try:
        # Iterate through the responses yielded by the workflow's run() method.
        for response in workflow.run():
            if hasattr(response, 'content') and response.content:
                print(response.content) # Print the content of each response.
                workflow_console_log.append(response.content)
                # Check for the workflow's own success message
                if response.content == "Workflow finished successfully.":
                    final_status_message = "Main block: Workflow execution completed successfully."
                # Check for agent-reported failure to ensure the final message is appropriate
                elif "failed." in response.content or "halting" in response.content.lower() or "error during" in response.content.lower():
                    final_status_message = "Main block: Workflow concluded (may have been halted by an agent or an error)."
            else:
                no_content_message = "Received a response with no content or malformed response."
                print(no_content_message)
                workflow_console_log.append(no_content_message)
        
        # If loop completes without an explicit "Workflow finished successfully." or halt, set a generic concluded message
        if final_status_message == "Main block: Workflow started." and not workflow_console_log:
             final_status_message = "Main block: Workflow concluded without yielding any messages."
        elif final_status_message == "Main block: Workflow started.": # some messages yielded, but no explicit end/halt
             final_status_message = "Main block: Workflow concluded."


    except Exception as e:
        # Catch any exceptions that occur during workflow execution.
        final_status_message = f"Main block: Workflow execution failed with an unhandled exception: {e}"
        print(final_status_message)
        workflow_console_log.append(final_status_message)
    finally:
        print(f"Final Status Reported by Main Logic: {final_status_message}")
        print("Main block: Script finished. Preparing JSON summary...")

    # --- Collect data for JSON ---
    output_data = {}
    # Map internal agent names (keys of workflow.agent_prompts/logs) to desired JSON keys
    agent_json_keys_map = {
        "EDA_Agent": "EDA_Agent",
        "Feature_Engineering_Agent": "FeatureEngineering_Agent", # Note: No underscore in JSON key
        "Modeling_Agent": "Modeling_Agent",
        "Evaluation_Agent": "Evaluation_Agent"
    }
    
    # Agents' prompts and logs
    for agent_internal_name, agent_json_key in agent_json_keys_map.items():
        output_data[agent_json_key] = {
            'prompt': workflow.agent_prompts.get(agent_internal_name, "Prompt not captured or agent did not run."),
            'output_log': "\\n".join(workflow.agent_run_logs.get(agent_internal_name, ["Log not captured or agent did not run."]))
        }

    # Script contents
    script_files_to_read = {
        # JSON key : filename
        "EDA_Script": "EDA.py", 
        "FeatureEngineering_Script": "FEATURE.py",
        "Modeling_Script": "MODEL.py",
        "Evaluation_Script": "EVAL.py"
    }
    for script_json_key, filename in script_files_to_read.items():
        try:
            # Agents are expected to save scripts in the current working directory.
            # If DATA_DIR is involved, ensure paths are correct. Agents' instructions say CWD.
            with open(filename, 'r', encoding='utf-8') as f:
                output_data[script_json_key] = f.read()
        except FileNotFoundError:
            output_data[script_json_key] = f"File '{filename}' not found in current working directory."
        except Exception as e:
            output_data[script_json_key] = f"Error reading '{filename}': {str(e)}"
            
    # Save JSON file
    json_filename = "workflow_summary.json"
    try:
        with open(json_filename, 'w', encoding='utf-8') as f:
            json.dump(output_data, f, indent=2)
        print(f"Successfully saved workflow summary to {json_filename}")
    except Exception as e:
        print(f"Error saving JSON summary to {json_filename}: {str(e)}")


# Entry point for the script.
# This ensures that run_pipeline_main_logic() is called only when the script is executed directly.
if __name__ == "__main__":
    run_pipeline_main_logic() 
    
# Risk Analytics Agent

A Python-based agent system for pre-trade risk scoring and market regime prediction in fixed income trading.

## Overview

The Risk Analytics Agent provides real-time risk analysis and market regime prediction for trading systems. It uses temporal fusion transformers (TFT) for market regime prediction and specialized models (N-BEATS, DeepAR) for risk scoring.

## Features

- Market regime prediction with 7 distinct regime classifications
- Dynamic risk scoring based on current market conditions
- Real-time analysis of trade risk profiles
- REST and streaming API interfaces
- Feature store integration with Redis and Apache Pinot
- Agent-based architecture for flexible deployment

## Installation

### Prerequisites

- Python 3.9 or higher
- Git LFS (for model storage)
- Docker (optional, for containerized deployment)

### Setup

1. Clone the repository:
   ```
   git clone https://github.com/example/risk-analytics-agent.git
   cd risk-analytics-agent
   ```

2. Set up the environment:
   ```
   python -m venv venv
   source venv/bin/activate  # On Windows: venv\Scripts\activate
   pip install -e .
   ```

3. Configure environment variables:
   ```
   cp env.example .env
   # Edit .env with your configuration
   ```

## Development

To set up the development environment:

```
pip install -e ".[dev,notebook]"
pre-commit install
```

## Usage

### Running the API server

```
risk-analytics-serve
```

Or manually:

```
python -m risk_analytics_agent.main
```

### Training models

```
risk-analytics-train --model regime --config configs/model_configs/regime_model.yaml
```

## API Documentation

After starting the server, API documentation is available at:
- Swagger UI: http://localhost:8000/docs
- ReDoc: http://localhost:8000/redoc

## Testing

Run the test suite:

```
pytest
```

With coverage:

```
pytest --cov=src
```

## License

MIT License

## Implementation Status

The following tasks from the implementation checklist have been completed:

### Phase 1: Project Setup and Infrastructure

- [x] Task 1: Project Structure Setup
  - [x] Sub-task 1.1: Create folder structure according to structure.md
  - [x] Sub-task 1.2: Set up .gitignore file with appropriate patterns for Python projects
  - [x] Sub-task 1.3: Initialize Git repository with .gitattributes for LFS handling

- [x] Task 2: Environment Configuration
  - [x] Sub-task 2.1: Create .env.example with appropriate values
  - [x] Sub-task 2.2: Create and initialize pyproject.toml file
  - [x] Sub-task 2.3: Update pyproject.toml with dependencies
  - [x] Sub-task 2.4: Update setup.py with proper package information and dependencies
  - [x] Sub-task 2.5: Create requirements.txt for development environment

### Current Tasks in Progress

- [ ] Task 3: Data Connectors Implementation
- [ ] Task 4: Feature Store Implementation
- [ ] Task 5: Data Schema Definitions
- [ ] Task 6: Data Preprocessing Pipeline