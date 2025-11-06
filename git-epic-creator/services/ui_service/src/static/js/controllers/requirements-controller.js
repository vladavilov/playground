/**
 * Requirements Screen Controller
 * 
 * Extends ChatBaseController to provide requirements generation interface.
 * Handles requirements-specific rendering, API calls, and SSE events.
 * 
 * @module requirements
 */

'use strict';

import { ChatBaseController } from '../core/base-controller.js';
import { RequirementsRenderer } from '../renderers/requirements-renderer.js';
import { RequirementsEditor } from '../editors/requirements-editor.js';
import { ApiClient } from '../services/api-client.js';

/**
 * Requirements screen controller.
 * Extends ChatBaseController for common chat functionality.
 */
class RequirementsController extends ChatBaseController {
  constructor() {
    super({
      chatOutputId: 'chatOutput',
      chatFormId: 'chatForm',
      chatInputId: 'chatInput',
      projectTitleId: 'chatProjectTitle',
      projectStatusId: 'chatProjectStatus'
    });
    
    // Resolve screen-specific DOM elements
    this.requirementsContent = document.getElementById('requirementsContent');
    this.requirementsSummary = document.getElementById('requirementsSummary');
    this.editRequirementsBtn = document.getElementById('editRequirementsBtn');
    this.confirmRequirementsBtn = document.getElementById('confirmRequirementsBtn');
    
    // Initialize renderer
    this.renderer = new RequirementsRenderer(
      this.requirementsContent,
      this.requirementsSummary,
      this.editRequirementsBtn,
      this.confirmRequirementsBtn
    );
    
    // Initialize enhanced editor (will be created when needed)
    this.enhancedEditor = null;
    
    // API client will be initialized after config is loaded
    this.apiClient = null;
  }
  
  /**
   * Creates initial state with screen-specific properties.
   * @override
   */
  createInitialState() {
    return {
      ...super.createInitialState(),
      promptId: null,  // Track conversation thread ID
      currentQuestions: null  // Store current clarification questions
    };
  }
  
  
  /**
   * Sends requirements generation request to API.
   * @override
   */
  async sendRequest(text) {
    const base = this.state.config.aiWorkflowApiBase.replace(/\/$/, '');
    
    try {
      const payload = {
        project_id: this.state.projectId,
        prompt: text
      };
      
      // Include prompt_id for follow-up conversations to maintain context
      if (this.state.promptId) {
        payload.prompt_id = this.state.promptId;
      }
      
      const bundle = await this.apiClient.post(`${base}/requirements`, payload);
      
      this.state.currentBundle = bundle;
      this.state.promptId = bundle.prompt_id;  // Track prompt_id for conversation continuity
      
      // Render requirements
      await this.renderer.render(bundle);
      
      // Enable enhanced inline editing
      this.enableEnhancedEditing();
      
      // Show success message
      const businessCount = bundle.business_requirements?.length || 0;
      const functionalCount = bundle.functional_requirements?.length || 0;
      const scorePercent = Math.round(bundle.score * 100);
      this.chatUI.appendAssistantMessage(
        `<div class="text-sm text-indigo-700">✓ Generated ${businessCount + functionalCount} requirement(s) (${businessCount} business, ${functionalCount} functional) • Score: ${scorePercent}%</div>`,
        'Assistant'
      );
      
      // Display clarification questions in chat if score is low
      if (bundle.clarification_questions?.length > 0 && bundle.score < 0.7) {
        this.displayQuestionsInChat(bundle.clarification_questions, bundle.score);
        this.showAnswerButton();
      } else {
        this.hideAnswerButton();
      }
      
      // Link pending box to prompt ID
      const pid = bundle.prompt_id;
      if (this.boxManager.pendingBox && !this.boxManager.pendingBox.promptId && pid) {
        this.boxManager.pendingBox.setPromptId(pid);
        this.boxManager.clearPending();
      }
      const box = pid ? this.getOrCreateBoxForPromptId(pid) : this.boxManager.activeBox;
      if (box) box.finish('ok');
      
    } catch (error) {
      const errorMsg = ApiClient.formatError(error);
      this.chatUI.appendSystemMessage(`Error: ${errorMsg}`);
      
      // Close all thinking boxes with error state
      this.finishThinkingBoxesWithError();
    }
  }
  
  /**
   * Gets SSE event handlers for requirements screen.
   * Merges base class handlers (retrieval_progress) with requirements-specific handlers.
   * @override
   */
  getSSEEventHandlers() {
    return {
      ...super.getSSEEventHandlers(),
      'ai_requirements_progress': (evt) => {
        try {
          const msg = JSON.parse(evt.data);
          if (!this.isCurrentProjectEvent(msg)) {
            return;
          }
          
          // Check if this is an enhancement progress message
          if (msg.enhancement_mode && msg.item_id) {
            // Route to enhancement progress handler
            this.handleEnhancementProgress(msg);
            return;
          }
          
          // Standard full workflow progress (thinking box)
          this.updateProjectStatus(msg.status);
          
          const md = msg.details_md || msg.thought_summary || '...';
          const pid = msg.prompt_id || null;
          const box = this.getOrCreateBoxForPromptId(pid);
          
          if (box) {
            box.appendMarkdown(md);
            
            const status = (msg.status || '').toLowerCase();
            if (status === 'completed') {
              box.finish('ok');
            } else if (status.includes('error') || status.includes('failed')) {
              box.finish('error');
            }
          }
        } catch (err) {
          console.error('Failed to process ai_requirements_progress event:', err);
        }
      }
    };
  }
  
  /**
   * Sets up page-specific event handlers.
   * @override
   */
  setupPageSpecificHandlers() {
    // Edit requirements button - scrolls to requirements and shows hint
    if (this.editRequirementsBtn) {
      this.editRequirementsBtn.addEventListener('click', () => {
        this.requirementsContent.scrollIntoView({ behavior: 'smooth', block: 'start' });
        this.chatUI.appendSystemMessage('Click any field to edit, or use the expand button for focus mode');
      });
    }
    
    // Confirm requirements button
    if (this.confirmRequirementsBtn) {
      this.confirmRequirementsBtn.addEventListener('click', () => {
        if (!this.state.currentBundle) return;
        
        // Store requirements bundle in sessionStorage to pass to tasks page
        // (URL params would be too long for full requirements data)
        try {
          const requirementsText = this.formatRequirementsForTasks(this.state.currentBundle);
          sessionStorage.setItem('pendingRequirements', requirementsText);
          
          const params = new URLSearchParams({
            project_id: this.state.projectId,
            from_requirements: 'true'
          });
          window.location.href = `/pages/tasks.html?${params.toString()}`;
        } catch (error) {
          console.error('Failed to prepare requirements for tasks:', error);
          this.chatUI.appendSystemMessage('Error: Failed to prepare requirements for task generation');
        }
      });
    }
  }
  
  /**
   * Enables enhanced inline editing for requirements.
   */
  enableEnhancedEditing() {
    if (!this.state.currentBundle) return;
    
    // Create enhanced editor instance
    this.enhancedEditor = new RequirementsEditor(
      this.state.currentBundle,
      () => this.handleEnhancedEditorSave()
    );
    
    // Set controller reference for AI enhancement callbacks
    this.enhancedEditor.setController(this);
    
    // Enable inline editing mode
    this.enhancedEditor.enableInlineEditing();
  }
  
  /**
   * Handles save from enhanced editor.
   */
  async handleEnhancedEditorSave() {
    // Re-render requirements with updated data
    await this.renderer.render(this.state.currentBundle);
    
    // Re-enable inline editing
    this.enableEnhancedEditing();
  }
  
  /**
   * Displays clarification questions in the chat panel.
   * @param {Array<Object>} questions - Clarification questions
   * @param {number} score - Current quality score
   */
  displayQuestionsInChat(questions, score) {
    if (!questions || questions.length === 0) return;
    
    const scorePercent = Math.round(score * 100);
    const sortedQuestions = questions.sort((a, b) => (a.priority || 99) - (b.priority || 99));
    
    // Build questions list HTML
    let questionsHtml = `
      <div class="p-4 bg-amber-50 border-l-4 border-amber-400 rounded">
        <div class="flex items-start gap-3 mb-3">
          <svg class="w-5 h-5 text-amber-600 flex-shrink-0 mt-0.5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"></path>
          </svg>
          <div>
            <h4 class="font-semibold text-amber-900">Quality Score: ${scorePercent}%</h4>
            <p class="text-sm text-amber-700 mt-1">Please answer these questions to improve requirements quality:</p>
          </div>
        </div>
        <ol class="space-y-2 ml-8 mt-3">
    `;
    
    sortedQuestions.forEach((q, idx) => {
      const optionsHint = q.options ? ` <span class="text-xs text-slate-500">(${q.options.join(' / ')})</span>` : '';
      const scoreGain = q.expected_score_gain ? ` <span class="ml-1 px-1.5 py-0.5 bg-emerald-100 text-emerald-700 rounded text-xs font-medium">+${Math.round(q.expected_score_gain * 100)}%</span>` : '';
      questionsHtml += `
        <li class="text-sm text-slate-800">
          <span class="font-medium">${q.text}</span>${optionsHint}${scoreGain}
        </li>
      `;
    });
    
    questionsHtml += `
        </ol>
      </div>
    `;
    
    this.chatUI.appendAssistantMessage(questionsHtml, 'Assistant');
    
    // Store questions for button click
    this.state.currentQuestions = sortedQuestions;
  }
  
  /**
   * Shows the "Answer Questions" button above the input.
   */
  showAnswerButton() {
    // Check if button already exists
    let btn = document.getElementById('answerQuestionsBtn');
    if (btn) {
      btn.classList.remove('hidden');
      return;
    }
    
    // Create button above the input area
    const inputArea = this.domElements.chatForm.parentElement;
    btn = document.createElement('div');
    btn.id = 'answerQuestionsBtn';
    btn.className = 'px-4 py-2 mb-2';
    btn.innerHTML = `
      <button class="w-full flex items-center justify-center gap-2 px-4 py-3 bg-gradient-to-r from-amber-500 to-amber-600 hover:from-amber-600 hover:to-amber-700 text-white rounded-lg font-medium shadow-md hover:shadow-lg transition-all transform hover:scale-105">
        <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z"></path>
        </svg>
        <span>Answer Questions</span>
      </button>
    `;
    
    // Insert before the form
    inputArea.insertBefore(btn, this.domElements.chatForm);
    
    // Add click handler
    btn.querySelector('button').addEventListener('click', () => {
      this.prepopulateAnswerTemplate();
    });
  }
  
  /**
   * Hides the "Answer Questions" button.
   */
  hideAnswerButton() {
    const btn = document.getElementById('answerQuestionsBtn');
    if (btn) {
      btn.classList.add('hidden');
    }
  }
  
  /**
   * Prepopulates chat input with question answer template.
   */
  prepopulateAnswerTemplate() {
    const questions = this.state.currentQuestions;
    if (!questions || questions.length === 0) return;
    
    // Build template
    let template = 'Here are my answers:\n\n';
    questions.forEach((q, idx) => {
      const optionsHint = q.options ? ` (${q.options.join(' / ')})` : '';
      template += `${idx + 1}. ${q.text}${optionsHint}\n   [Your answer here]\n\n`;
    });
    
    // Populate input
    if (this.domElements.chatInput) {
      this.domElements.chatInput.value = template;
      this.domElements.chatInput.focus();
      // Move cursor to first placeholder
      const firstPlaceholder = template.indexOf('[Your answer here]');
      if (firstPlaceholder !== -1) {
        this.domElements.chatInput.setSelectionRange(firstPlaceholder, firstPlaceholder + 17);
      }
      // Auto-expand textarea if needed
      this.domElements.chatInput.style.height = 'auto';
      this.domElements.chatInput.style.height = Math.min(this.domElements.chatInput.scrollHeight, 300) + 'px';
    }
  }
  
  /**
   * Formats requirements bundle as structured text for AI tasks service.
   * @param {Object} bundle - Requirements bundle
   * @returns {string} Formatted requirements text
   */
  formatRequirementsForTasks(bundle) {
    let text = '# Requirements for Task Generation\n\n';
    
    // Business Requirements
    if (bundle.business_requirements?.length > 0) {
      text += '## Business Requirements\n\n';
      bundle.business_requirements.forEach((req, idx) => {
        text += `### ${idx + 1}. ${req.title} [${req.priority}]\n\n`;
        text += `${req.description}\n\n`;
        if (req.rationale) {
          text += `**Rationale:** ${req.rationale}\n\n`;
        }
        if (req.acceptance_criteria?.length > 0) {
          text += '**Acceptance Criteria:**\n';
          req.acceptance_criteria.forEach(ac => {
            text += `- ${ac}\n`;
          });
          text += '\n';
        }
      });
    }
    
    // Functional Requirements
    if (bundle.functional_requirements?.length > 0) {
      text += '## Functional Requirements\n\n';
      bundle.functional_requirements.forEach((req, idx) => {
        text += `### ${idx + 1}. ${req.title} [${req.priority}]\n\n`;
        text += `${req.description}\n\n`;
        if (req.rationale) {
          text += `**Rationale:** ${req.rationale}\n\n`;
        }
        if (req.acceptance_criteria?.length > 0) {
          text += '**Acceptance Criteria:**\n';
          req.acceptance_criteria.forEach(ac => {
            text += `- ${ac}\n`;
          });
          text += '\n';
        }
      });
    }
    
    // Assumptions
    if (bundle.assumptions?.length > 0) {
      text += '## Assumptions\n\n';
      bundle.assumptions.forEach(assumption => {
        text += `- ${assumption}\n`;
      });
      text += '\n';
    }
    
    // Risks
    if (bundle.risks?.length > 0) {
      text += '## Risks\n\n';
      bundle.risks.forEach(risk => {
        text += `- ${risk}\n`;
      });
      text += '\n';
    }
    
    text += '\n---\n\Generate epics and tasks based on these requirements.';
    
    return text;
  }
  
  /**
   * Enhances a single requirement with AI-generated expansions.
   * @param {Object} req - Requirement object
   * @param {string} reqType - Requirement type: "business" or "functional"
   * @param {number} index - Requirement index in bundle
   * @returns {Promise<Object>} Enhanced requirement
   */
  async enhanceRequirement(req, reqType, index) {
    if (!this.apiClient) {
      throw new Error('API client not initialized');
    }
    
    if (!this.state.projectId) {
      throw new Error('No project selected');
    }
    
    try {
      // Prepare enhancement request
      const enhanceRequest = {
        project_id: this.state.projectId,
        requirement_id: req.id,
        requirement_type: reqType,
        current_content: {
          id: req.id,
          title: req.title || '',
          description: req.description || '',
          acceptance_criteria: req.acceptance_criteria || [],
          priority: req.priority || 'Must',
          rationale: req.rationale || ''
        }
      };
      
      // Call enhancement endpoint
      const response = await this.apiClient.request(
        '/workflow/enhance',
        {
          method: 'POST',
          body: JSON.stringify(enhanceRequest)
        }
      );
      
      // Return enhanced requirement
      return {
        id: response.id,
        title: response.title,
        description: response.description,
        acceptance_criteria: response.acceptance_criteria,
        rationale: response.rationale,
        priority: response.priority || req.priority
      };
      
    } catch (error) {
      console.error('Enhancement API call failed:', error);
      throw new Error(`Enhancement failed: ${ApiClient.formatError(error)}`);
    }
  }
  
  /**
   * Handles enhancement progress updates from SSE.
   * @param {Object} data - Progress message data
   */
  handleEnhancementProgress(data) {
    // Check if this is an enhancement progress message
    if (!data.enhancement_mode || !data.item_id) {
      return;
    }
    
    // Find the requirement index
    const bundle = this.state.currentBundle;
    if (!bundle || !this.enhancedEditor) {
      return;
    }
    
    let reqIndex = -1;
    let reqType = null;
    
    // Search in business requirements
    reqIndex = bundle.business_requirements?.findIndex(r => r.id === data.item_id);
    if (reqIndex !== -1) {
      reqType = 'business';
    } else {
      // Search in functional requirements
      reqIndex = bundle.functional_requirements?.findIndex(r => r.id === data.item_id);
      if (reqIndex !== -1) {
        reqType = 'functional';
      }
    }
    
    if (reqIndex === -1 || !reqType) {
      return; // Not found, ignore
    }
    
    // Find the card element based on current editor mode
    let card = null;
    if (this.enhancedEditor.currentMode === 'inline') {
      // Inline mode: query main content area
      card = this.requirementsContent?.querySelector(`.req-card:nth-child(${reqIndex + 1})`);
    } else {
      // Focus/Fullscreen mode: query within modal
      const modal = document.getElementById('focusModeModal');
      if (modal) {
        card = modal.querySelector(`.req-card:nth-child(${reqIndex + 1})`);
      }
    }
    
    if (!card) {
      return;
    }
    
    // Update progress on the card based on status
    switch (data.status) {
      case 'analyzing_item':
        this.enhancedEditor.showCardProgress(card, 'Analyzing requirement...');
        break;
      case 'retrieving_context':
        this.enhancedEditor.showCardProgress(card, 'Retrieving context...');
        break;
      case 'enhancing_item':
        this.enhancedEditor.showCardProgress(card, 'Enhancing with AI...');
        break;
      case 'completed':
        this.enhancedEditor.hideCardProgress(card);
        break;
      case 'error':
        this.enhancedEditor.showCardError(card, data.thought_summary || 'Enhancement failed');
        break;
    }
  }
}

// Initialize controller
const controller = new RequirementsController();
controller.initialize().catch(console.error);
