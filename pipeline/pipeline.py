DATA_DIR = "data"
import os
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

    # Runs the entire workflow by invoking agents sequentially.
    # Yields:
    # RunResponse: Objects indicating the status or output of each step.
    def run(self):
        # This method orchestrates the sequential execution of agents: EDA, Feature Engineering, Modeling, and Evaluation.
        # It captures insights from the EDA agent and passes them to the Feature Engineering agent.
        # The method yields RunResponse objects to provide real-time feedback on the workflow's progress and handles agent failures by halting the workflow.

        yield RunResponse(content="Starting EDA_Agent...") # Using RunResponse from agno.agent for structure
        try:
            # Run the EDA_Agent and capture its response.
            eda_agent_response = self.EDA_Agent.run() # This is expected to be a single RunResponse object

            # Process the EDA_Agent's response.
            if eda_agent_response and hasattr(eda_agent_response, 'content') and eda_agent_response.content and not eda_agent_response.content.startswith("FAILURE:"):
                self.eda_insights = eda_agent_response.content # Store the insights for later use.
                yield RunResponse(content=f"EDA_Agent finished successfully. Insights captured: {self.eda_insights}")
            elif eda_agent_response and hasattr(eda_agent_response, 'content') and eda_agent_response.content and eda_agent_response.content.startswith("FAILURE:"):
                yield RunResponse(content=f"EDA_Agent failed. Halting workflow. Response: {eda_agent_response.content}")
                return # Halt workflow by stopping generation
            else: # Handles None response or unexpected response structure
                response_content = eda_agent_response.content if eda_agent_response and hasattr(eda_agent_response, 'content') else None
                yield RunResponse(content=f"EDA_Agent failed. Halting workflow. Response: {response_content}")
                return # Halt workflow
        except Exception as e:
            yield RunResponse(content=f"Error during EDA_Agent execution: {e}")
            # According to plan, halt workflow. Re-raising will do this.
            # If the base workflow handles re-raised exceptions by stopping, this is fine.
            # Otherwise, we might need a more explicit stop signal if a RunResponse is always expected.
            raise

        # --- Feature Engineering Agent ---
        # This section executes the Feature Engineering agent, providing it with insights from the EDA phase.
        yield RunResponse(content="Starting Feature_Engineering_Agent...")
        if self.Feature_Engineering_Agent:
            try:
                # Dynamically construct the prompt for the Feature_Engineering_Agent by including EDA insights.
                dynamic_fe_prompt = f"{FEATURE_ENGINEERING_AGENT_BASE_PROMPT}\n\n--- EDA Insights for Feature Engineering ---\n{self.eda_insights}"
                self.Feature_Engineering_Agent.instructions = dynamic_fe_prompt # Set the dynamically generated instructions.
                
                # Run the Feature_Engineering_Agent.
                fe_response = self.Feature_Engineering_Agent.run()

                # Process the Feature_Engineering_Agent's response.
                if fe_response and hasattr(fe_response, 'content') and isinstance(fe_response.content, str):
                    if fe_response.content == "SUCCESS":
                        yield RunResponse(content="Feature_Engineering_Agent finished successfully.")
                    elif fe_response.content.startswith("FAILURE:"):
                        yield RunResponse(content=f"Feature_Engineering_Agent failed. Halting workflow. Response: {fe_response.content}")
                        return
                    else:
                        # Unexpected content, treat as failure
                        yield RunResponse(content=f"Feature_Engineering_Agent returned unexpected content. Halting workflow. Response: {fe_response.content}")
                        return
                else:
                    # No response or malformed response, treat as failure
                    yield RunResponse(content=f"Feature_Engineering_Agent did not return a valid response. Halting workflow. Response: {str(fe_response.content if fe_response and hasattr(fe_response, 'content') else fe_response)}")
                    return
            except Exception as e:
                yield RunResponse(content=f"Error during Feature_Engineering_Agent execution: {str(e)}")
                raise # Re-raise the exception to be caught by the main try-except or to halt if unhandled
        else:
            yield RunResponse(content="Feature_Engineering_Agent not initialized. Skipping.")
            # Potentially halt or handle as an error depending on workflow requirements

        # --- Modeling Agent ---
        # This section executes the Modeling agent to train a predictive model.
        yield RunResponse(content="Starting Modeling_Agent...")
        if self.Modeling_Agent:
            try:
                # Run the Modeling_Agent.
                model_response = self.Modeling_Agent.run()

                # Process the Modeling_Agent's response.
                if model_response and hasattr(model_response, 'content') and isinstance(model_response.content, str):
                    if model_response.content == "SUCCESS": # Based on MODELING_AGENT_EXPECTED_OUTPUT
                        yield RunResponse(content="Modeling_Agent finished successfully.")
                    elif model_response.content.startswith("FAILURE:"):
                        yield RunResponse(content=f"Modeling_Agent failed. Halting workflow. Response: {model_response.content}")
                        return
                    else:
                        yield RunResponse(content=f"Modeling_Agent returned unexpected content. Halting workflow. Response: {model_response.content}")
                        return
                else:
                    yield RunResponse(content=f"Modeling_Agent did not return a valid response. Halting workflow. Response: {str(model_response.content if model_response and hasattr(model_response, 'content') else model_response)}")
                    return
            except Exception as e:
                yield RunResponse(content=f"Error during Modeling_Agent execution: {str(e)}")
                raise
        else:
            yield RunResponse(content="Modeling_Agent not initialized. Skipping.")

        # --- Evaluation Agent ---
        # This section executes the Evaluation agent to assess the performance of the trained model.
        yield RunResponse(content="Starting Evaluation_Agent...")
        if self.Evaluation_Agent:
            try:
                # Run the Evaluation_Agent.
                eval_response = self.Evaluation_Agent.run()

                # Process the Evaluation_Agent's response.
                if eval_response and hasattr(eval_response, 'content') and isinstance(eval_response.content, str):
                    if eval_response.content == "SUCCESS": # Based on EVALUATION_AGENT_EXPECTED_OUTPUT
                        yield RunResponse(content="Evaluation_Agent finished successfully.")
                    elif eval_response.content.startswith("FAILURE:"):
                        yield RunResponse(content=f"Evaluation_Agent failed. Halting workflow. Response: {eval_response.content}")
                        return
                    else:
                        yield RunResponse(content=f"Evaluation_Agent returned unexpected content. Halting workflow. Response: {eval_response.content}")
                        return
                else:
                    yield RunResponse(content=f"Evaluation_Agent did not return a valid response. Halting workflow. Response: {str(eval_response.content if eval_response and hasattr(eval_response, 'content') else eval_response)}")
                    return
            except Exception as e:
                yield RunResponse(content=f"Error during Evaluation_Agent execution: {str(e)}")
                raise
        else:
            yield RunResponse(content="Evaluation_Agent not initialized. Skipping. Halting workflow as this is a critical step.")
            return
        
        yield RunResponse(content="Workflow finished successfully.") # Final success message if all agents complete

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
# It prints the progress and status messages yielded by the workflow.
def run_pipeline_main_logic():
   
    # Using a simple object as a placeholder for the Azure OpenAI model.
    # In a real application, this would be an instance of the actual model client.
    mock_azure_model = object() 

    print("Building workflow...")
    # Build the workflow instance, passing the (mock) Azure model.
    workflow = build_workflow(azure_model=mock_azure_model) 
    print("Workflow built.")
    
    print("Running workflow...")
    try:
        final_status_message = "Main block: Workflow concluded (may have been halted by an agent)."
        # Iterate through the responses yielded by the workflow's run() method.
        for response in workflow.run():
            if hasattr(response, 'content') and response.content:
                print(response.content) # Print the content of each response.
                # Check for the workflow's own success message
                if response.content == "Workflow finished successfully.":
                    final_status_message = "Main block: Workflow execution completed."
                # Check for agent-reported failure to ensure the final message is appropriate
                elif "failed. Halting workflow." in response.content:
                    # The loop will stop after this if the workflow halts, so this status will be the last one set before print
                    final_status_message = "Main block: Workflow concluded (may have been halted by an agent)."
            else:
                print("Received a response with no content or malformed response.")
        
        print(final_status_message) # Print the final status of the workflow execution.

    except Exception as e:
        # Catch any exceptions that occur during workflow execution.
        print(f"Main block: Workflow execution failed with an exception: {e}")
    finally:
        print("Main block: Script finished.") # A general script end message

# Entry point for the script.
# This ensures that run_pipeline_main_logic() is called only when the script is executed directly.
if __name__ == "__main__":
    run_pipeline_main_logic() 