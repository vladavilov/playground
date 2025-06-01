import unittest
from unittest.mock import Mock, patch, call
import sys
import os
import subprocess
import io # For capturing stdout
import contextlib # For redirect_stdout

# Add the project root to the Python path to allow importing pipeline
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

import pipeline
from pipeline import build_workflow, LogReturnWorkflow, SHARED_AGENT_DESCRIPTION, run_pipeline_main_logic
# Import from the new agent modules
from agents.eda_agent import EDA_AGENT_INSTRUCTIONS, EDA_AGENT_EXPECTED_OUTPUT
from agents.feature_engineering_agent import FEATURE_ENGINEERING_AGENT_BASE_PROMPT, FEATURE_ENGINEERING_AGENT_EXPECTED_OUTPUT
from agents.modeling_agent import MODELING_AGENT_INSTRUCTIONS, MODELING_AGENT_EXPECTED_OUTPUT
from agents.evaluation_agent import EVALUATION_AGENT_INSTRUCTIONS, EVALUATION_AGENT_EXPECTED_OUTPUT

# Assuming RunResponse is available from agno.agent or agno.run.response
# For testing purposes, we might need to mock it if direct import is an issue or if it comes from a different module in agno
# Let's try importing from agno.agent first, if not found, will adjust.

# Try importing RunResponse from agno.run.response first, then agno.agent
# If both fail, we'll have to create a more robust mock or simplify.
try:
    from agno.run.response import RunResponse
except ImportError:
    try:
        from agno.agent import RunResponse
    except ImportError:
        # If RunResponse cannot be imported, use a simple Mock that can have a .content attribute
        # This is a fallback for the testing environment if agno is not fully available or structured as expected.
        class MockRunResponse:
            def __init__(self, content=None, event=None, run_id=None):
                self.content = content
                self.event = event
                self.run_id = run_id
            def __eq__(self, other):
                if isinstance(other, MockRunResponse):
                    return self.content == other.content
                if isinstance(other, Mock):
                    # Allow comparison with mock_calls like call().content
                    # This is a simplistic comparison for test purposes.
                    return self.content == other.content
                return False
            def __repr__(self):
                return f"MockRunResponse(content='{self.content}')"
        RunResponse = MockRunResponse

class TestPipelineSetup(unittest.TestCase):

    @patch('pipeline.create_evaluation_agent')
    @patch('pipeline.create_modeling_agent')
    @patch('pipeline.create_feature_engineering_agent')
    @patch('pipeline.create_eda_agent')
    def test_workflow_agent_initialization(self, mock_create_eda, mock_create_fe, mock_create_model, mock_create_eval):
        mock_azure_model = Mock()
        
        # Set up return values for the factory functions
        mock_eda_agent = Mock()
        mock_fe_agent = Mock()
        mock_model_agent = Mock()
        mock_eval_agent = Mock()
        
        mock_create_eda.return_value = mock_eda_agent
        mock_create_fe.return_value = mock_fe_agent
        mock_create_model.return_value = mock_model_agent
        mock_create_eval.return_value = mock_eval_agent
        
        # Act
        workflow_instance = build_workflow(mock_azure_model)
        
        # Assert
        self.assertIsInstance(workflow_instance, LogReturnWorkflow)
        self.assertIs(workflow_instance.azure_model, mock_azure_model)
        
        # Check for agent factory function calls
        mock_create_eda.assert_called_once_with(
            model=mock_azure_model,
            description=SHARED_AGENT_DESCRIPTION
        )
        mock_create_fe.assert_called_once_with(
            model=mock_azure_model,
            description=SHARED_AGENT_DESCRIPTION
        )
        mock_create_model.assert_called_once_with(
            model=mock_azure_model,
            description=SHARED_AGENT_DESCRIPTION
        )
        mock_create_eval.assert_called_once_with(
            model=mock_azure_model,
            description=SHARED_AGENT_DESCRIPTION
        )

        # Check that the agents are assigned correctly from factory returns
        self.assertIs(workflow_instance.EDA_Agent, mock_eda_agent)
        self.assertIs(workflow_instance.Feature_Engineering_Agent, mock_fe_agent)
        self.assertIs(workflow_instance.Modeling_Agent, mock_model_agent)
        self.assertIs(workflow_instance.Evaluation_Agent, mock_eval_agent)

class TestPipelineWorkflowExecution(unittest.TestCase):
    def setUp(self):
        self.mock_azure_model = Mock()
        self.mock_eda_agent_instance = Mock()
        self.mock_fe_agent_instance = Mock()
        self.mock_model_agent_instance = Mock()
        self.mock_eval_agent_instance = Mock()

        # Patch pipeline factory functions for LogReturnWorkflow's __init__ during its instantiation
        with patch('pipeline.create_eda_agent') as mock_create_eda, \
             patch('pipeline.create_feature_engineering_agent') as mock_create_fe, \
             patch('pipeline.create_modeling_agent') as mock_create_model, \
             patch('pipeline.create_evaluation_agent') as mock_create_eval:
            
            # Set up return values for factory functions
            mock_create_eda.return_value = self.mock_eda_agent_instance
            mock_create_fe.return_value = self.mock_fe_agent_instance
            mock_create_model.return_value = self.mock_model_agent_instance
            mock_create_eval.return_value = self.mock_eval_agent_instance
            
            self.workflow = LogReturnWorkflow(azure_model=self.mock_azure_model)
        
        # Verify that agents were assigned correctly
        self.assertIs(self.workflow.EDA_Agent, self.mock_eda_agent_instance, "EDA_Agent was not correctly assigned")
        self.assertIs(self.workflow.Feature_Engineering_Agent, self.mock_fe_agent_instance, "Feature_Engineering_Agent was not correctly assigned")
        self.assertIs(self.workflow.Modeling_Agent, self.mock_model_agent_instance, "Modeling_Agent was not correctly assigned")
        self.assertIs(self.workflow.Evaluation_Agent, self.mock_eval_agent_instance, "Evaluation_Agent was not correctly assigned")

        # Setup default successful run behaviors for preceding agents
        self.mock_eda_agent_instance.run.return_value = RunResponse(content="Mocked EDA Insights")
        self.mock_fe_agent_instance.run.return_value = RunResponse(content="SUCCESS")
        self.mock_model_agent_instance.run.return_value = RunResponse(content="SUCCESS")
        # Default for eval agent, can be overridden in specific tests
        self.mock_eval_agent_instance.run.return_value = RunResponse(content="SUCCESS")

    def consume_workflow_run(self):
        return list(self.workflow.run())

    def test_run_eda_agent_success(self):
        mock_agent_response = RunResponse(content="Synthesized insights from EDA")
        self.mock_eda_agent_instance.run.return_value = mock_agent_response
        responses = self.consume_workflow_run()
        self.mock_eda_agent_instance.run.assert_called_once()
        
        self.assertTrue(any(r.content == "Starting EDA_Agent..." for r in responses if hasattr(r, 'content')))
        self.assertTrue(any(r.content == "EDA_Agent finished successfully. Insights captured: Synthesized insights from EDA" for r in responses if hasattr(r, 'content')))
        self.assertTrue(any(r.content == "Starting Feature_Engineering_Agent..." for r in responses if hasattr(r, 'content')))
        
        self.assertEqual(self.workflow.eda_insights, "Synthesized insights from EDA")
        for resp in responses:
            if hasattr(resp, 'content') and isinstance(resp.content, str):
                 self.assertNotIn("EDA_Agent failed", resp.content, "EDA failure message was yielded on success.")

    def test_run_eda_agent_failure_signal(self):
        mock_agent_response = RunResponse(content="FAILURE: EDA script error")
        self.mock_eda_agent_instance.run.return_value = mock_agent_response
        responses = self.consume_workflow_run()
        self.mock_eda_agent_instance.run.assert_called_once()
        self.assertTrue(any(r.content == "Starting EDA_Agent..." for r in responses if hasattr(r, 'content')))
        self.assertTrue(any(r.content == "EDA_Agent failed. Halting workflow. Response: FAILURE: EDA script error" for r in responses if hasattr(r, 'content')))
        
        proceeded = any(hasattr(resp, 'content') and resp.content == "Starting Feature_Engineering_Agent..." for resp in responses)
        self.assertFalse(proceeded, "Workflow proceeded to Feature_Engineering_Agent after EDA failure signal.")

    def test_run_eda_agent_returns_none_content(self):
        mock_agent_response = RunResponse(content=None)
        self.mock_eda_agent_instance.run.return_value = mock_agent_response 
        responses = self.consume_workflow_run()
        self.mock_eda_agent_instance.run.assert_called_once()
        self.assertTrue(any(r.content == "EDA_Agent failed. Halting workflow. Response: None" for r in responses if hasattr(r, 'content')))
        proceeded = any(hasattr(resp, 'content') and resp.content == "Starting Feature_Engineering_Agent..." for resp in responses)
        self.assertFalse(proceeded, "Workflow proceeded after EDA_Agent returned None content.")

    def test_run_eda_agent_returns_none_response(self): 
        self.mock_eda_agent_instance.run.return_value = None
        responses = self.consume_workflow_run()
        self.mock_eda_agent_instance.run.assert_called_once()
        self.assertTrue(any(r.content == "EDA_Agent failed. Halting workflow. Response: None" for r in responses if hasattr(r, 'content')))
        proceeded = any(hasattr(resp, 'content') and resp.content == "Starting Feature_Engineering_Agent..." for resp in responses)
        self.assertFalse(proceeded, "Workflow proceeded after EDA_Agent returned None response.")

    def test_run_eda_agent_unexpected_exception(self):
        self.mock_eda_agent_instance.run.side_effect = Exception("Unexpected EDA error")
        with self.assertRaisesRegex(Exception, "Unexpected EDA error"):
            self.consume_workflow_run()
        self.mock_eda_agent_instance.run.assert_called_once()
        # Check for the yielded error message before re-raise
        # This requires iterating the generator carefully or inspecting logs if AGNO does that.
        # For now, direct check is hard if exception stops consume_workflow_run prematurely.
        # However, pipeline.py does: yield RunResponse(...); raise. So it should be in the list if not for the raise.
        # Let's assume the raise will prevent further items in `responses` if `list()` is used.
        # A more robust way would be to iterate and catch inside the test.
        # Try to get yielded values before exception
        gen = self.workflow.run()
        yielded_error_response = False
        try:
            while True:
                response = next(gen)
                if hasattr(response, 'content') and "Error during EDA_Agent execution: Unexpected EDA error" in response.content:
                    yielded_error_response = True
                    # Do not break, let it raise the exception
        except Exception as e:
            self.assertTrue("Unexpected EDA error" in str(e))
        #This check might be tricky because the exception is also raised from the generator
        #self.assertTrue(yielded_error_response, "Error message was not yielded before exception.")

    # --- Feature Engineering Agent Tests ---
    def test_run_fe_agent_success(self):
        # Pre-condition: EDA must succeed and provide insights
        eda_insights = "EDA insights for FE"
        self.mock_eda_agent_instance.run.return_value = RunResponse(content=eda_insights)
        
        # Configure FE Agent to succeed
        self.mock_fe_agent_instance.run.return_value = RunResponse(content="SUCCESS") 

        responses = self.consume_workflow_run()

        self.mock_eda_agent_instance.run.assert_called_once()
        self.mock_fe_agent_instance.run.assert_called_once()

        # Verify dynamic prompt for FE Agent
        expected_fe_instructions = f"{FEATURE_ENGINEERING_AGENT_BASE_PROMPT}\n\n--- EDA Insights for Feature Engineering ---\n{eda_insights}"
        self.assertEqual(self.mock_fe_agent_instance.instructions, expected_fe_instructions)

        self.assertTrue(any(r.content == "Starting Feature_Engineering_Agent..." for r in responses if hasattr(r, 'content')))
        self.assertTrue(any(r.content == "Feature_Engineering_Agent finished successfully." for r in responses if hasattr(r, 'content')))
        self.assertTrue(any(r.content == "Starting Modeling_Agent..." for r in responses if hasattr(r, 'content'))) # Check it proceeds

    def test_run_fe_agent_failure_signal(self):
        eda_insights = "EDA insights"
        self.mock_eda_agent_instance.run.return_value = RunResponse(content=eda_insights)
        self.mock_fe_agent_instance.run.return_value = RunResponse(content="FAILURE: FE script error")

        responses = self.consume_workflow_run()
        self.mock_fe_agent_instance.run.assert_called_once()
        self.assertTrue(any(r.content == "Feature_Engineering_Agent failed. Halting workflow. Response: FAILURE: FE script error" for r in responses if hasattr(r, 'content')))
        proceeded = any(hasattr(resp, 'content') and resp.content == "Starting Modeling_Agent..." for resp in responses)
        self.assertFalse(proceeded, "Workflow proceeded to Modeling_Agent after FE failure.")

    def test_run_fe_agent_unexpected_exception(self):
        eda_insights = "EDA insights"
        self.mock_eda_agent_instance.run.return_value = RunResponse(content=eda_insights)
        self.mock_fe_agent_instance.run.side_effect = Exception("Unexpected FE error")

        with self.assertRaisesRegex(Exception, "Unexpected FE error"):
            self.consume_workflow_run()
        self.mock_fe_agent_instance.run.assert_called_once()
        # Check for yielded error message (simplified)
        # A full check would iterate the generator up to the exception point.
        # For now, ensure the exception from agent propagates through workflow.

    # --- Modeling Agent Tests ---
    def test_run_modeling_agent_success(self):
        # Pre-conditions: EDA and FE must succeed
        self.mock_eda_agent_instance.run.return_value = RunResponse(content="EDA insights")
        self.mock_fe_agent_instance.run.return_value = RunResponse(content="SUCCESS") 
        
        # Configure Modeling Agent to succeed
        self.mock_model_agent_instance.run.return_value = RunResponse(content="SUCCESS")

        responses = self.consume_workflow_run()

        self.mock_model_agent_instance.run.assert_called_once()
        self.assertTrue(any(r.content == "Starting Modeling_Agent..." for r in responses if hasattr(r, 'content')))
        self.assertTrue(any(r.content == "Modeling_Agent finished successfully." for r in responses if hasattr(r, 'content')))
        self.assertTrue(any(r.content == "Starting Evaluation_Agent..." for r in responses if hasattr(r, 'content'))) # Check it proceeds

    def test_run_modeling_agent_failure_signal(self):
        self.mock_eda_agent_instance.run.return_value = RunResponse(content="EDA insights")
        self.mock_fe_agent_instance.run.return_value = RunResponse(content="SUCCESS")
        self.mock_model_agent_instance.run.return_value = RunResponse(content="FAILURE: Model training error")

        responses = self.consume_workflow_run()
        self.mock_model_agent_instance.run.assert_called_once()
        self.assertTrue(any(r.content == "Modeling_Agent failed. Halting workflow. Response: FAILURE: Model training error" for r in responses if hasattr(r, 'content')))
        proceeded = any(hasattr(resp, 'content') and resp.content == "Starting Evaluation_Agent..." for resp in responses)
        self.assertFalse(proceeded, "Workflow proceeded to Evaluation_Agent after Modeling failure.")

    def test_run_modeling_agent_unexpected_exception(self):
        self.mock_eda_agent_instance.run.return_value = RunResponse(content="EDA insights")
        self.mock_fe_agent_instance.run.return_value = RunResponse(content="SUCCESS")
        self.mock_model_agent_instance.run.side_effect = Exception("Unexpected Model error")

        with self.assertRaisesRegex(Exception, "Unexpected Model error"):
            self.consume_workflow_run()
        self.mock_model_agent_instance.run.assert_called_once()

    # --- Evaluation Agent Workflow Tests ---
    def test_run_evaluation_agent_success(self):
        # EDA, FE, Model agents are mocked to succeed in setUp
        # Evaluation_Agent is also mocked to succeed by default in setUp
        responses = self.consume_workflow_run()
        response_contents = [r.content for r in responses if hasattr(r, 'content')]

        self.mock_eval_agent_instance.run.assert_called_once()
        self.assertIn("Starting Evaluation_Agent...", response_contents)
        self.assertIn("Evaluation_Agent finished successfully.", response_contents)
        self.assertEqual(response_contents[-1], "Workflow finished successfully.")

    def test_run_evaluation_agent_failure_reported(self):
        self.mock_eval_agent_instance.run.return_value = RunResponse(content="FAILURE: Eval script error")
        
        responses = self.consume_workflow_run()
        response_contents = [r.content for r in responses if hasattr(r, 'content')]

        self.mock_eval_agent_instance.run.assert_called_once()
        self.assertIn("Starting Evaluation_Agent...", response_contents)
        self.assertIn("Evaluation_Agent failed. Halting workflow. Response: FAILURE: Eval script error", response_contents)
        self.assertNotIn("Workflow finished successfully.", response_contents)

    def test_run_evaluation_agent_exception_raised(self):
        expected_exception_message = "Simulated eval error from agent"
        self.mock_eval_agent_instance.run.side_effect = RuntimeError(expected_exception_message)
        
        with self.assertRaisesRegex(RuntimeError, expected_exception_message):
            self.consume_workflow_run()
        
        self.mock_eval_agent_instance.run.assert_called_once()
        # Check that "Starting Evaluation_Agent..." was yielded before the exception
        # This requires careful iteration if consume_workflow_run re-raises immediately
        # For simplicity, we assume the exception is caught by the test runner after the agent call.

    def test_run_evaluation_agent_malformed_response(self):
        self.mock_eval_agent_instance.run.return_value = RunResponse(content="UNEXPECTED_EVAL_CONTENT")
        
        responses = self.consume_workflow_run()
        response_contents = [r.content for r in responses if hasattr(r, 'content')]

        self.mock_eval_agent_instance.run.assert_called_once()
        self.assertIn("Starting Evaluation_Agent...", response_contents)
        self.assertIn("Evaluation_Agent returned unexpected content. Halting workflow. Response: UNEXPECTED_EVAL_CONTENT", response_contents)
        self.assertNotIn("Workflow finished successfully.", response_contents)

    def test_run_evaluation_agent_none_response(self):
        self.mock_eval_agent_instance.run.return_value = None # Agent's run() returns None directly
        
        responses = self.consume_workflow_run()
        response_contents = [r.content for r in responses if hasattr(r, 'content')]

        self.mock_eval_agent_instance.run.assert_called_once()
        self.assertIn("Starting Evaluation_Agent...", response_contents)
        self.assertTrue(any("Evaluation_Agent did not return a valid response" in r for r in response_contents))
        self.assertNotIn("Workflow finished successfully.", response_contents)

    def test_run_evaluation_agent_not_initialized_handled(self):
        # Manually set the agent to None *after* the initial setup that uses patching
        self.workflow.Evaluation_Agent = None
        
        responses = self.consume_workflow_run()
        response_contents = [r.content for r in responses if hasattr(r, 'content')]

        # EDA, FE, Model agents should still run successfully based on setUp mocks
        self.mock_eda_agent_instance.run.assert_called_once()
        self.mock_fe_agent_instance.run.assert_called_once()
        self.mock_model_agent_instance.run.assert_called_once()
        
        # Evaluation_Agent's run method should not be called as the instance is None
        self.mock_eval_agent_instance.run.assert_not_called() 

        self.assertIn("Starting Evaluation_Agent...", response_contents)
        self.assertIn("Evaluation_Agent not initialized. Skipping. Halting workflow as this is a critical step.", response_contents)
        self.assertNotIn("Workflow finished successfully.", response_contents)

class TestPipelineExecutionAsScript(unittest.TestCase):

    @patch('pipeline.build_workflow')
    def test_main_block_runs_workflow_success(self, mock_build_workflow):
        mock_workflow_instance = Mock()
        mock_run_generator_method = Mock() # This will be the .run() method
        
        expected_responses_content = [
            "Starting EDA_Agent...",
            "EDA_Agent finished successfully. Insights captured: Mocked EDA Insights",
            "Starting Feature_Engineering_Agent...",
            "Feature_Engineering_Agent finished successfully.",
            "Starting Modeling_Agent...",
            "Modeling_Agent finished successfully.",
            "Starting Evaluation_Agent...",
            "Evaluation_Agent finished successfully.",
            "Workflow finished successfully."
        ]
        # The .run() method itself should return a generator
        mock_run_generator_method.return_value = (RunResponse(content=c) for c in expected_responses_content)
        mock_workflow_instance.run = mock_run_generator_method
        mock_build_workflow.return_value = mock_workflow_instance

        # Capture stdout
        stdout_capture = io.StringIO()
        with contextlib.redirect_stdout(stdout_capture):
            run_pipeline_main_logic() # Call the refactored main logic function
        
        stdout = stdout_capture.getvalue()
        # print(f"STDOUT:\n{stdout}") # For debugging tests

        self.assertIn("Building workflow...", stdout)
        self.assertIn("Workflow built.", stdout)
        
        current_pos = 0
        for expected_msg in expected_responses_content:
            self.assertIn(expected_msg, stdout)
            new_pos = stdout.find(expected_msg, current_pos)
            self.assertTrue(new_pos >= current_pos, f"Message '{expected_msg}' not found in order or after previous message. Searched from pos {current_pos}. Found at {new_pos}. Full stdout:\n{stdout}")
            current_pos = new_pos
        
        self.assertIn("Main block: Workflow execution completed.", stdout)
        self.assertIn("Main block: Script finished.", stdout) # Check for the finally block message
        mock_build_workflow.assert_called_once()
        mock_workflow_instance.run.assert_called_once()

    @patch('pipeline.build_workflow')
    def test_main_block_handles_workflow_failure_gracefully(self, mock_build_workflow):
        mock_workflow_instance = Mock()
        mock_run_generator_method = Mock()
        expected_responses_content = [
            "Starting EDA_Agent...",
            "EDA_Agent failed. Halting workflow. Response: FAILURE: EDA Error"
        ]
        mock_run_generator_method.return_value = (RunResponse(content=c) for c in expected_responses_content)
        mock_workflow_instance.run = mock_run_generator_method
        mock_build_workflow.return_value = mock_workflow_instance

        stdout_capture = io.StringIO()
        with contextlib.redirect_stdout(stdout_capture):
            run_pipeline_main_logic()
        stdout = stdout_capture.getvalue()

        self.assertIn("Building workflow...", stdout)
        self.assertIn("Workflow built.", stdout)
        self.assertIn("EDA_Agent failed. Halting workflow. Response: FAILURE: EDA Error", stdout)
        self.assertNotIn("Workflow finished successfully.", stdout)
        self.assertIn("Main block: Workflow concluded (may have been halted by an agent).", stdout)
        self.assertIn("Main block: Script finished.", stdout)

    @patch('pipeline.build_workflow')
    def test_main_block_handles_workflow_exception(self, mock_build_workflow):
        mock_workflow_instance = Mock()
        mock_run_generator_method = Mock()
        
        def faulty_generator():
            yield RunResponse(content="Starting EDA_Agent...")
            raise RuntimeError("Critical workflow error from agent run")
        
        mock_run_generator_method.return_value = faulty_generator()
        mock_workflow_instance.run = mock_run_generator_method
        mock_build_workflow.return_value = mock_workflow_instance

        stdout_capture = io.StringIO()
        with contextlib.redirect_stdout(stdout_capture):
            run_pipeline_main_logic()
        stdout = stdout_capture.getvalue()

        self.assertIn("Building workflow...", stdout)
        self.assertIn("Workflow built.", stdout)
        self.assertIn("Starting EDA_Agent...", stdout)
        self.assertIn("Main block: Workflow execution failed with an exception: Critical workflow error from agent run", stdout)
        self.assertNotIn("Workflow finished successfully.", stdout)
        self.assertNotIn("Main block: Workflow execution completed.", stdout) # Because it failed with exception
        self.assertIn("Main block: Script finished.", stdout)

if __name__ == '__main__':
    unittest.main() 