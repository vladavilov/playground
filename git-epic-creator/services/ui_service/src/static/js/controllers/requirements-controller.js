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
import { fetchConfig } from '../utils/connections.js';

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
      ...super.createInitialState()
    };
  }
  
  /**
   * Initializes API client after configuration is loaded.
   * @override
   */
  async initialize() {
    // Load config first
    this.state.config = await fetchConfig();
    
    // Create API client before super.initialize() so it's available in loadProject()
    if (this.state.config) {
      this.apiClient = new ApiClient(this.state.config, () => this.handle401Error());
    }
    
    // Now call super.initialize() which will call loadProject()
    await super.initialize();
  }
  
  
  /**
   * Sends requirements generation request to API.
   * @override
   */
  async sendRequest(text) {
    const base = this.state.config.aiWorkflowApiBase.replace(/\/$/, '');
    
    try {
      const bundle = await this.apiClient.post(`${base}/requirements`, {
        project_id: this.state.projectId,
        prompt: text
      });
      
      this.state.currentBundle = bundle;
      
      // Render requirements
      this.renderer.render(bundle);
      
      // Enable enhanced inline editing
      this.enableEnhancedEditing();
      
      // Show success message
      const businessCount = bundle.business_requirements?.length || 0;
      const functionalCount = bundle.functional_requirements?.length || 0;
      this.chatUI.appendAssistantMessage(
        `<div class="text-sm text-indigo-700">✓ Generated ${businessCount + functionalCount} requirement(s) (${businessCount} business, ${functionalCount} functional)</div>`,
        'Assistant'
      );
      
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
      
      if (this.boxManager.pendingBox) {
        this.boxManager.pendingBox.finish('error');
        this.boxManager.clearPending();
      }
    }
  }
  
  /**
   * Gets SSE event handlers for requirements screen.
   * @override
   */
  getSSEEventHandlers() {
    return {
      'ai_requirements_progress': (evt) => {
        try {
          const msg = JSON.parse(evt.data);
          if (!this.state.projectId || String(this.state.projectId) !== String(msg.project_id)) {
            return;
          }
          
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
      },
      'retrieval_progress': (evt) => {
        try {
          const msg = JSON.parse(evt.data);
          if (!this.state.projectId || String(this.state.projectId) !== String(msg.project_id)) {
            return;
          }
          
          this.updateProjectStatus(msg.phase || 'retrieving');
          
          const md = msg.details_md || msg.thought_summary || 'Retrieving context...';
          const rid = msg.prompt_id || null;
          const box = this.getOrCreateBoxForPromptId(rid);
          
          if (box) {
            box.appendMarkdown(md);
            
            if (msg.phase === 'error') {
              box.finish('error');
            }
          }
        } catch (err) {
          console.error('Failed to process retrieval_progress event:', err);
        }
      }
    };
  }
  
  /**
   * Sets up screen-specific event handlers.
   * @override
   */
  setupScreenSpecificHandlers() {
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
    
    // Enable inline editing mode
    this.enhancedEditor.enableInlineEditing();
  }
  
  /**
   * Handles save from enhanced editor.
   */
  handleEnhancedEditorSave() {
    // Re-render requirements with updated data
    this.renderer.render(this.state.currentBundle);
    
    // Re-enable inline editing
    this.enableEnhancedEditing();
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
}

// Initialize controller
const controller = new RequirementsController();
controller.initialize().catch(console.error);
