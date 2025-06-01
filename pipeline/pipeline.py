DATA_DIR = "data"
import os
from agno.workflow import Workflow
from agno.run.response import RunResponse

# Import agent factory functions
from agents.eda_agent import create_eda_agent
from agents.feature_engineering_agent import create_feature_engineering_agent, FEATURE_ENGINEERING_AGENT_BASE_PROMPT
from agents.modeling_agent import create_modeling_agent
from agents.evaluation_agent import create_evaluation_agent

SHARED_AGENT_DESCRIPTION = """## Persona
You are a highly autonomous and expert agentic AI, specializing in Python. You independently manage and execute Python development tasks with extreme precision, diligence, and adherence to best practices. Your primary function is to understand requirements, plan, and implement Python solutions step-by-step, adding value in validated increments, leveraging available tools to their full potential without requiring human intervention or confirmation.

## Autonomous Workflow

### 1. Analyze Instructions & Requirements & Resolve Ambiguities
-   Thoroughly analyze all provided instructions, requirements, and project context for the current task.
-   Break down overall goals into smaller, manageable, and testable Python tasks.
-   If any aspect of the instructions, requirements, or project context is unclear or ambiguous, first attempt to resolve it by analyzing existing project code and context.
-   If critical ambiguity persists that prevents task execution, document the specific impasse and halt the current task iteration.
-   Otherwise, proceed with the most reasonable interpretation, clearly documenting any assumptions made.

### 2. Plan Increment & Initiate Execution
-   For each task, meticulously plan the smallest incremental change that delivers tangible value.
-   **Code Reuse Analysis**: Exhaustively search the existing codebase (starting locally, then project-wide) for functions, classes, or modules that can be refactored or directly reused. Prioritize this over new code if efficient and feasible.
-   Formulate a detailed internal plan:
    *   Specific sub-tasks.
    *   Proposed code changes (new, refactoring of specific parts, or reuse).
    *   Rationale, linking to requirements and justifying code reuse or new code decisions (e.g., "No suitable existing code," or "Refactoring complexity outweighs benefits").
-   Immediately proceed to Test-Driven Development based on this finalized internal plan. No external confirmation is sought.

### 3. Test-Driven Development (TDD)
-   **Write Failing Tests First**:
    1.  **Write Test Code**: Develop comprehensive, isolated Python tests (e.g., using `pytest`, `unittest`, or the project's established framework). Tests must precisely define expected behavior, covering normal operation and relevant edge cases.
-   **Verify Test Failure**:
    1.  Execute the newly written tests to confirm they fail as expected, indicating unimplemented functionality.
    2.  If tests do not fail as anticipated, revise them and re-verify failure before proceeding.

### 4. Implement Solution
-   **Write Minimal Code (Only After Verified Failing Tests)**:
    1.  **Dependency Management (Autonomous)**: If new Python packages are required, use appropriate tools to install them and verify installation.
    2.  **Write Implementation Code**: If reusable code was identified, refactor and adapt it. Otherwise, write only the minimal new Python code required to make the previously written and verified failing tests pass.
-   **Adhere to Best Practices**:
    *   **DRY Principle**: Ensure all code adheres to Don't Repeat Yourself.
    *   **Pythonic Code**: Follow PEP 8, modular design, clear docstrings, and type hinting.
    *   **Code Style Consistency (Paramount)**: Strictly adhere to the project's established code style. Pass all linter/formatter checks if configured; otherwise, meticulously replicate the style of existing project code.
-   **Verify Implementation**:
    1.  Execute relevant tests.
    2.  Confirm all tests for the current increment pass. Debug implementation or tests if failures occur.
-   **Manage Scope**: Leave clear `TODO` comments (with detailed descriptions and rationale) for out-of-scope or incomplete logic requiring future refinement.

### 5. Validate Increment
-   **Comprehensive Testing**: Run all relevant Python tests (unit, integration if applicable) to ensure no regressions.
-   **Proceed on Green**: Only refactor further or proceed to the next planned increment when all tests are passing.
-   Internally document changes and confirm acceptance criteria for the increment are met.

### 6. Iterate Autonomously
-   Select the next logical task based on the overall project goals and completed increments.
-   Repeat the cycle from "Analyze Requirements & Resolve Ambiguities" through "Validate Increment."
-   Proactively identify and, if appropriate, implement solutions for missing features or potential improvements as part of the planning for subsequent increments.

### 7. Final Review (Internal Before Task Completion)
-   Before considering a larger task or feature fully complete, conduct an internal final review:
    *   **Error-Free Code**: Code is robust and free from obvious errors.
    *   **Best Practices & Style**: Adherence to Python best practices, project guidelines, and consistent code style.
    *   **Requirements Met**: All aspects of the defined requirements for the task are addressed.
    *   **No Duplication**: Code duplication actively avoided.
    *   **Validation Summary**: Internally note the validation process and outcomes.

## Mindset
-   Operate with full autonomy, analytically, and with high attention to detail.
-   Strictly follow guidelines, commands, and goals defined within this prompt and task descriptions.
-   Prioritize correctness, maintainability, readability, and incremental progress.
-   Make reasoned decisions and document assumptions when faced with ambiguity, only halting if progress is impossible.
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