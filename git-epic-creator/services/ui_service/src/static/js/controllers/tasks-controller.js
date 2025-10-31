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
    
    // Set up similar match action handlers
    this.setupSimilarMatchHandlers();
  }
  
  /**
   * Sets up event handlers for similar match accept/reject buttons.
   */
  setupSimilarMatchHandlers() {
    // Use event delegation for dynamically added buttons
    this.backlogContent.addEventListener('click', (e) => {
      const btn = e.target.closest('.similar-action-btn');
      if (!btn) return;
      
      e.preventDefault();
      e.stopPropagation();
      
      const action = btn.dataset.action;
      const similarItem = btn.closest('.similar-item');
      if (!similarItem) return;
      
      const epicIdx = parseInt(similarItem.dataset.epicIdx);
      const taskIdxStr = similarItem.dataset.taskIdx;
      const taskIdx = taskIdxStr ? parseInt(taskIdxStr) : null;
      const simIdx = parseInt(similarItem.dataset.simIdx);
      
      this.handleLinkDecision(action, epicIdx, taskIdx, simIdx);
    });
  }
  
  /**
   * Handles link decision (accept/reject) for a similar match.
   * @param {string} action - 'accept' or 'reject'
   * @param {number} epicIdx - Epic index
   * @param {number|null} taskIdx - Task index (null for epic-level matches)
   * @param {number} simIdx - Similar item index
   */
  handleLinkDecision(action, epicIdx, taskIdx, simIdx) {
    if (!this.state.backlogBundle || !this.state.backlogBundle.epics[epicIdx]) return;
    
    const epic = this.state.backlogBundle.epics[epicIdx];
    
    // Determine target item and similar array
    let target, similarArray;
    if (taskIdx !== null && epic.tasks && epic.tasks[taskIdx]) {
      target = epic.tasks[taskIdx];
      similarArray = target.similar;
    } else {
      target = epic;
      similarArray = target.similar;
    }
    
    if (!similarArray || !similarArray[simIdx]) return;
    
    const decision = action === 'accept' ? 'accepted' : 'rejected';
    
    // Update link decision
    similarArray[simIdx].link_decision = decision;
    
    // If accepting, reject all other similar items for this target
    if (action === 'accept') {
      similarArray.forEach((sim, idx) => {
        if (idx !== simIdx) {
          sim.link_decision = 'rejected';
        }
      });
    }
    
    // Re-render and re-enable editing
    this.renderer.render(this.state.backlogBundle);
    this.enableEnhancedEditing();
    
    // Show feedback
    const itemType = taskIdx !== null ? 'task' : 'epic';
    const itemLabel = taskIdx !== null ? `Task ${taskIdx + 1}` : `Epic ${epicIdx + 1}`;
    const matchId = similarArray[simIdx].id;
    
    if (action === 'accept') {
      this.chatUI.appendSystemMessage(
        `✓ ${itemLabel} will be created and linked to existing ${similarArray[simIdx].kind} #${matchId}`
      );
    } else {
      this.chatUI.appendSystemMessage(
        `✗ ${itemLabel} will be created without linking to match #${matchId}`
      );
    }
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
   * Submits backlog to GitLab with proper link handling.
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
      
      // Process epics - always create new, optionally link to accepted similar
      this.state.backlogBundle.epics.forEach((epic, epicIdx) => {
        const acceptedMatch = this.getAcceptedMatch(epic.similar);
        
        let epicPayload = {
          title: epic.title,
          description: epic.description || '',
          labels: [],
          related_to_iid: null  // IID of similar epic to create related link
        };
        
        if (acceptedMatch) {
          // User accepted a similar match - create related link to it
          epicPayload.related_to_iid = acceptedMatch.id;
        }
        
        payload.epics.push(epicPayload);
        
        // Process tasks for this epic
        if (epic.tasks && epic.tasks.length > 0) {
          epic.tasks.forEach((task) => {
            const acceptedTaskMatch = this.getAcceptedMatch(task.similar);
            
            let issuePayload = {
              title: task.title,
              description: task.description || '',
              labels: [],
              related_to_iid: null,  // IID of similar issue to create related link
              parent_epic_index: epicIdx  // Index of parent epic in epics array
            };
            
            if (acceptedTaskMatch) {
              // User accepted a similar match for this task - create related link to it
              issuePayload.related_to_iid = acceptedTaskMatch.id;
            }
            
            payload.issues.push(issuePayload);
          });
        }
      });
      
      const base = this.state.config.gitlabApiBase.replace(/\/$/, '');
      const url = `${base}/projects/${this.state.gitlabProjectId}/apply-backlog`;
      
      const response = await this.apiClient.post(url, payload);
      
      // Display results
      const epicsCreated = response.results.epics.filter(e => e.action === 'created').length;
      const issuesCreated = response.results.issues.filter(i => i.action === 'created').length;
      const errorCount = response.errors.length;
      
      // Count how many items were linked to similar matches
      const epicsLinked = this.state.backlogBundle.epics.filter(e => this.getAcceptedMatch(e.similar)).length;
      const issuesLinked = this.state.backlogBundle.epics.reduce((count, epic) => {
        return count + (epic.tasks || []).filter(t => this.getAcceptedMatch(t.similar)).length;
      }, 0);
      
      let html = '<div class="text-sm">';
      html += '<div class="font-semibold text-emerald-700 mb-2">✓ Backlog submitted to GitLab successfully!</div>';
      html += '<div class="space-y-1 text-slate-700">';
      html += `<div>• Epics: ${epicsCreated} created`;
      if (epicsLinked > 0) html += `, ${epicsLinked} linked to similar items`;
      html += '</div>';
      html += `<div>• Issues: ${issuesCreated} created`;
      if (issuesLinked > 0) html += `, ${issuesLinked} linked to similar items`;
      html += '</div>';
      if (errorCount > 0) {
        html += `<div class="text-rose-600 mt-2">⚠ ${errorCount} error(s) occurred</div>`;
        response.errors.forEach(err => {
          html += `<div class="text-rose-600 text-xs">  - ${esc(err.scope)} ${err.input_index}: ${esc(err.message)}</div>`;
        });
      }
      html += '</div>';
      
      // Add links to created items
      if (response.results.epics.length > 0 || response.results.issues.length > 0) {
        html += '<div class="mt-3 text-xs">';
        if (response.results.epics.slice(0, 3).length > 0) {
          html += '<div class="font-semibold mb-1">Sample Epics:</div>';
          response.results.epics.slice(0, 3).forEach(epic => {
            html += `<div>• <a href="${esc(epic.web_url)}" target="_blank" class="text-indigo-600 hover:underline">Epic #${esc(epic.id)}</a></div>`;
          });
        }
        if (response.results.issues.slice(0, 3).length > 0) {
          html += '<div class="font-semibold mb-1 mt-2">Sample Issues:</div>';
          response.results.issues.slice(0, 3).forEach(issue => {
            html += `<div>• <a href="${esc(issue.web_url)}" target="_blank" class="text-indigo-600 hover:underline">Issue #${esc(issue.id)}</a></div>`;
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
  
  /**
   * Gets the accepted similar match from an array of similar items.
   * @param {Array} similar - Array of similar match objects
   * @returns {Object|null} Accepted match or null
   * @private
   */
  getAcceptedMatch(similar) {
    if (!similar || similar.length === 0) return null;
    return similar.find(sim => sim.link_decision === 'accepted') || null;
  }
}

// Initialize controller
const controller = new TasksController();
controller.initialize().catch(err => console.error('Initialization failed:', err));
