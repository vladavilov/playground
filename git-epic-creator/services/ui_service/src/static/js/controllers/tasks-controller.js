/**
 * Tasks Screen Controller
 * 
 * Extends ChatBaseController to provide task/epic generation interface.
 * Handles backlog-specific rendering, API calls, SSE events, and GitLab submission.
 * 
 * @module tasks
 */

'use strict';

import { ChatBaseController } from '../core/base-controller.js';
import { BacklogRenderer } from '../renderers/backlog-renderer.js';
import { TasksEditor } from '../editors/tasks-editor.js';
import { ApiClient } from '../services/api-client.js';
import { escapeHtml as esc } from '../utils/dom-helpers.js';
import { fetchConfig } from '../utils/connections.js';

/**
 * Tasks screen controller.
 * Extends ChatBaseController for common chat functionality.
 */
class TasksController extends ChatBaseController {
  constructor() {
    super({
      chatOutputId: 'chatOutput',
      chatFormId: 'chatForm',
      chatInputId: 'chatInput',
      projectStatusId: 'taskProjectStatus'
    });
    
    // Resolve screen-specific DOM elements
    this.backlogContent = document.getElementById('backlogContent');
    this.backlogSummary = document.getElementById('backlogSummary');
    this.editBacklogBtn = document.getElementById('editBacklogBtn');
    this.submitBacklogBtn = document.getElementById('submitBacklogBtn');
    
    // Initialize renderer
    this.renderer = new BacklogRenderer(
      this.backlogContent,
      this.backlogSummary,
      this.editBacklogBtn,
      this.submitBacklogBtn
    );
    
    // Initialize enhanced editor (will be created when needed)
    this.enhancedEditor = null;
    
    // Parse URL parameters
    const params = new URLSearchParams(window.location.search);
    this.fromRequirements = params.get('from_requirements') === 'true';
    
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
      gitlabProjectId: null,
      promptId: null,
      backlogBundle: null
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
   * Loads project from URL parameters.
   * Extends base implementation to also extract GitLab project ID.
   * @override
   */
  async loadProject() {
    // Call base implementation to load project
    await super.loadProject();
    
    // Tasks-specific: Extract GitLab project ID
    if (this.state.project) {
      this.state.gitlabProjectId = this.state.project.gitlab_project_id;
      
      if (!this.state.gitlabProjectId) {
        this.chatUI.appendSystemMessage('Warning: Project does not have a GitLab project ID configured. GitLab features will be unavailable.');
      }
    } else if (this.state.projectId) {
      // Project ID was provided but loading failed
      this.chatUI.appendSystemMessage('Warning: Could not load project details. GitLab features may not work.');
    } else {
      // No project ID provided
      this.chatUI.appendSystemMessage('Error: No project ID provided');
    }
  }
  
  /**
   * Sends task generation request to API.
   * @override
   */
  async sendRequest(text) {
    const base = this.state.config.aiTasksApiBase.replace(/\/$/, '');
    const payload = {
      project_id: this.state.projectId,
      message: text.trim()
    };
    
    // Include prompt_id if we have an ongoing conversation
    if (this.state.promptId) {
      payload.prompt_id = this.state.promptId;
    }
    
    try {
      const bundle = await this.apiClient.post(`${base}/generate`, payload);
      
      this.state.backlogBundle = bundle;
      this.state.promptId = bundle.prompt_id;
      
      // Link pending box to prompt_id if it exists
      const pid = bundle.prompt_id;
      if (this.boxManager.pendingBox && !this.boxManager.pendingBox.promptId && pid) {
        this.boxManager.pendingBox.setPromptId(pid);
        this.boxManager.clearPending();
      }
      
      // Render backlog
      await this.renderer.render(bundle);
      
      // Enable enhanced inline editing
      this.enableEnhancedEditing();
      
      // Show success message
      const epicCount = bundle.epics?.length || 0;
      const taskCount = bundle.epics?.reduce((s, e) => s + (e.tasks?.length || 0), 0) || 0;
      this.chatUI.appendAssistantMessage(
        `<div class="text-sm text-indigo-700">✓ Generated ${epicCount} epic(s) with ${taskCount} task(s)</div>`
      );
      
      // Note: Don't finish the box here - let SSE events handle it
    } catch (error) {
      const errorMsg = ApiClient.formatError(error);
      this.chatUI.appendAssistantMessage(
        `<div class="text-sm text-rose-700">Error: ${esc(errorMsg)}</div>`,
        'System Error'
      );
      
      if (this.boxManager.pendingBox) {
        this.boxManager.pendingBox.finish('error');
        this.boxManager.clearPending();
      }
    }
  }
  
  /**
   * Gets SSE event handlers for tasks screen.
   * @override
   */
  getSSEEventHandlers() {
    return {
      'ai_tasks_progress': (evt) => {
        try {
          const msg = JSON.parse(evt.data);
          
          // Filter by project
          if (msg.project_id && this.state.projectId && 
              String(msg.project_id) !== String(this.state.projectId)) {
            return;
          }
          
          // Update status badge
          this.updateProjectStatus(msg.status);
          
          // Route to thinking box
          const promptId = msg.prompt_id || null;
          const box = this.getOrCreateBoxForPromptId(promptId);
          
          if (box) {
            const thoughtText = msg.details_md || msg.thought_summary;
            if (thoughtText) {
              box.appendMarkdown(thoughtText);
            }
            
            // Finish the box when status is completed or failed
            const status = (msg.status || '').toLowerCase();
            if (status === 'completed') {
              box.finish('ok');
            } else if (status.includes('error') || status.includes('failed')) {
              box.finish('error');
            }
          }
        } catch (err) {
          console.error('Failed to process ai_tasks_progress event:', err);
        }
      }
    };
  }
  
  /**
   * Sets up screen-specific event handlers.
   * @override
   */
  setupScreenSpecificHandlers() {
    // Edit backlog button - scrolls to backlog and shows hint
    if (this.editBacklogBtn) {
      this.editBacklogBtn.addEventListener('click', () => {
        this.backlogContent.scrollIntoView({ behavior: 'smooth', block: 'start' });
        this.chatUI.appendSystemMessage('Click any field to edit, or use the expand button for focus mode');
      });
    }
    
    // Submit to GitLab button
    if (this.submitBacklogBtn) {
      this.submitBacklogBtn.addEventListener('click', () => this.submitToGitLab());
    }
  }
  
  /**
   * Called after initialization completes.
   * Sends initial message with requirements if coming from requirements page.
   * @override
   */
  async onInitialized() {
    // Check if we have requirements from the requirements page
    if (this.fromRequirements) {
      try {
        const requirementsText = sessionStorage.getItem('pendingRequirements');
        
        if (requirementsText) {
          // Clear sessionStorage immediately
          sessionStorage.removeItem('pendingRequirements');
          
          // Show user message with requirements
          this.chatUI.appendUserMessage('Generate tasks from confirmed requirements');
          
          // Create thinking box
          const pending = this.boxManager.createBox(null);
          this.boxManager.pendingBox = pending;
          
          this.chatUI.appendSystemMessage('Generating initial backlog from confirmed requirements...');
          
          // Send requirements to AI
          await this.sendRequest(requirementsText);
        } else {
          // No requirements found in storage
          this.chatUI.appendSystemMessage('⚠️ Requirements not found. Please start from requirements page.');
        }
      } catch (error) {
        console.error('Failed to load requirements:', error);
        this.chatUI.appendSystemMessage('Error: Failed to load requirements. You can describe them manually.');
      }
    } else {
      this.chatUI.appendSystemMessage('Ready to generate tasks. Describe what you need or start from requirements page.');
    }
  }
  
  /**
   * Enables enhanced inline editing for backlog.
   */
  enableEnhancedEditing() {
    if (!this.state.backlogBundle) return;
    
    // Create enhanced editor instance
    this.enhancedEditor = new TasksEditor(
      this.state.backlogBundle,
      () => this.handleEnhancedEditorSave()
    );
    
    // Enable inline editing mode
    this.enhancedEditor.enableInlineEditing();
  }
  
  /**
   * Handles save from enhanced editor.
   */
  async handleEnhancedEditorSave() {
    // Re-render backlog with updated data
    await this.renderer.render(this.state.backlogBundle);
    
    // Re-enable inline editing
    this.enableEnhancedEditing();
  }
  
  /**
   * Submits backlog to GitLab.
   * @private
   */
  async submitToGitLab() {
    if (!this.state.backlogBundle || !this.state.projectId) {
      this.chatUI.appendAssistantMessage(
        '<div class="text-sm text-rose-700">Error: No backlog or project ID available</div>',
        'System Error'
      );
      return;
    }
    
    if (!this.state.gitlabProjectId) {
      this.chatUI.appendAssistantMessage(
        '<div class="text-sm text-rose-700">Error: Project does not have a GitLab project ID. Please set the GitLab project path first.</div>',
        'System Error'
      );
      return;
    }
    
    try {
      // Transform backlog bundle to GitLab API format
      const payload = {
        project_id: this.state.gitlabProjectId,
        internal_project_id: this.state.projectId,
        prompt_id: this.state.promptId || this.state.backlogBundle.prompt_id || '',
        epics: [],
        issues: []
      };
      
      // Process epics and flatten tasks to issues
      this.state.backlogBundle.epics.forEach((epic) => {
        payload.epics.push({
          id: epic.similar && epic.similar.length > 0 ? epic.similar[0].id : null,
          title: epic.title,
          description: epic.description || '',
          labels: []
        });
        
        if (epic.tasks && epic.tasks.length > 0) {
          epic.tasks.forEach((task) => {
            payload.issues.push({
              id: task.similar && task.similar.length > 0 ? task.similar[0].id : null,
              title: task.title,
              description: task.description || '',
              labels: [],
              epic_id: epic.similar && epic.similar.length > 0 ? epic.similar[0].id : null
            });
          });
        }
      });
      
      const base = this.state.config.gitlabApiBase.replace(/\/$/, '');
      const url = `${base}/projects/${this.state.gitlabProjectId}/apply-backlog`;
      
      const response = await this.apiClient.post(url, payload);
      
      // Display results
      const epicsCreated = response.results.epics.filter(e => e.action === 'created').length;
      const epicsUpdated = response.results.epics.filter(e => e.action === 'updated').length;
      const issuesCreated = response.results.issues.filter(i => i.action === 'created').length;
      const issuesUpdated = response.results.issues.filter(i => i.action === 'updated').length;
      const errorCount = response.errors.length;
      
      let html = '<div class="text-sm">';
      html += '<div class="font-semibold text-emerald-700 mb-2">✓ Backlog submitted to GitLab successfully!</div>';
      html += '<div class="space-y-1 text-slate-700">';
      html += `<div>• Epics: ${epicsCreated} created, ${epicsUpdated} updated</div>`;
      html += `<div>• Issues: ${issuesCreated} created, ${issuesUpdated} updated</div>`;
      if (errorCount > 0) {
        html += `<div class="text-rose-600 mt-2">⚠ ${errorCount} error(s) occurred</div>`;
      }
      html += '</div>';
      
      // Add links to created items
      if (response.results.epics.length > 0 || response.results.issues.length > 0) {
        html += '<div class="mt-3 text-xs">';
        if (response.results.epics.slice(0, 3).length > 0) {
          html += '<div class="font-semibold mb-1">Sample Epics:</div>';
          response.results.epics.slice(0, 3).forEach(epic => {
            html += `<div>• <a href="${esc(epic.web_url)}" target="_blank" class="text-indigo-600 hover:underline">${epic.action} #${esc(epic.id)}</a></div>`;
          });
        }
        if (response.results.issues.slice(0, 3).length > 0) {
          html += '<div class="font-semibold mb-1 mt-2">Sample Issues:</div>';
          response.results.issues.slice(0, 3).forEach(issue => {
            html += `<div>• <a href="${esc(issue.web_url)}" target="_blank" class="text-indigo-600 hover:underline">${issue.action} #${esc(issue.id)}</a></div>`;
          });
        }
        html += '</div>';
      }
      
      html += '</div>';
      this.chatUI.appendAssistantMessage(html, 'GitLab Submission');
      
    } catch (error) {
      console.error('Failed to submit to GitLab:', error);
      const errorMsg = ApiClient.formatError(error);
      this.chatUI.appendAssistantMessage(
        `<div class="text-sm text-rose-700">Failed to submit to GitLab: ${esc(errorMsg)}</div>`,
        'System Error'
      );
    }
  }
}

// Initialize controller
const controller = new TasksController();
controller.initialize().catch(err => console.error('Initialization failed:', err));
