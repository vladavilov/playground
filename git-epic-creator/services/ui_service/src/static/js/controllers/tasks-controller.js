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
      gitlabProjectIds: [],  // Array of all GitLab project IDs
      promptId: null,
      backlogBundle: null
    };
  }
  
  /**
   * Loads project from URL parameters.
   * Extends base implementation to also extract GitLab project IDs.
   * @override
   */
  async loadProject() {
    // Call base implementation to load project
    await super.loadProject();
    
    // Tasks-specific: Extract all GitLab project IDs from backlog projects array
    if (this.state.project) {
      // Project model supports multiple GitLab backlog projects (array)
      // All projects will be used for comprehensive backlog analysis
      const backlogProjectIds = this.state.project.gitlab_backlog_project_ids;
      
      if (backlogProjectIds && Array.isArray(backlogProjectIds) && backlogProjectIds.length > 0) {
        this.state.gitlabProjectIds = backlogProjectIds;
        
        this.chatUI.appendSystemMessage(
          `Project linked to ${backlogProjectIds.length} GitLab project(s) for comprehensive backlog analysis and duplicate detection.`
        );
      } else {
        this.state.gitlabProjectIds = [];
        this.chatUI.appendSystemMessage('Warning: Project does not have GitLab backlog projects configured. Duplicate detection will be unavailable.');
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
      
      // Initialize target_project_id for all items (default to first project)
      this.initializeTargetProjects(bundle);
      
      // Render backlog with gitlab project IDs
      await this.renderer.render(bundle, this.state.gitlabProjectIds);
      
      // Enable enhanced inline editing
      this.enableEnhancedEditing();
      
      // Set up project selection handlers
      this.setupProjectSelectionHandlers();
      
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
      
      // Close all thinking boxes with error state
      this.finishThinkingBoxesWithError();
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
          if (!this.isCurrentProjectEvent(msg)) {
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
   * Sets up page-specific event handlers.
   * @override
   */
  setupPageSpecificHandlers() {
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
    this.renderer.render(this.state.backlogBundle, this.state.gitlabProjectIds);
    this.enableEnhancedEditing();
    this.setupProjectSelectionHandlers();
    
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
   * Initializes target_project_id for all backlog items.
   * @param {Object} bundle - Backlog bundle
   * @private
   */
  initializeTargetProjects(bundle) {
    const defaultProject = this.state.gitlabProjectIds[0] || null;
    
    if (bundle && bundle.epics) {
      bundle.epics.forEach(epic => {
        if (!epic.target_project_id) {
          epic.target_project_id = defaultProject;
        }
        if (epic.tasks) {
          epic.tasks.forEach(task => {
            if (!task.target_project_id) {
              task.target_project_id = defaultProject;
            }
          });
        }
      });
    }
  }
  
  /**
   * Sets up event handlers for project selection dropdowns.
   * @private
   */
  setupProjectSelectionHandlers() {
    // Use event delegation for dynamically added dropdowns
    this.backlogContent.addEventListener('change', (e) => {
      const select = e.target.closest('.project-select');
      if (!select) return;
      
      const epicIdx = parseInt(select.dataset.epicIdx);
      const taskIdxStr = select.dataset.taskIdx;
      const taskIdx = taskIdxStr ? parseInt(taskIdxStr) : null;
      const selectedProject = select.value;
      
      this.handleProjectSelection(epicIdx, taskIdx, selectedProject);
    });
  }
  
  /**
   * Handles project selection change for an item.
   * @param {number} epicIdx - Epic index
   * @param {number|null} taskIdx - Task index (null for epics)
   * @param {string} projectId - Selected project ID
   * @private
   */
  handleProjectSelection(epicIdx, taskIdx, projectId) {
    if (!this.state.backlogBundle || !this.state.backlogBundle.epics[epicIdx]) return;
    
    const epic = this.state.backlogBundle.epics[epicIdx];
    
    if (taskIdx !== null && epic.tasks && epic.tasks[taskIdx]) {
      // Update task
      epic.tasks[taskIdx].target_project_id = projectId;
    } else {
      // Update epic
      epic.target_project_id = projectId;
    }
  }
  
  /**
   * Handles save from enhanced editor.
   */
  async handleEnhancedEditorSave() {
    // Re-render backlog with updated data
    await this.renderer.render(this.state.backlogBundle, this.state.gitlabProjectIds);
    
    // Re-enable inline editing
    this.enableEnhancedEditing();
    
    // Re-setup project selection handlers
    this.setupProjectSelectionHandlers();
  }
  
  /**
   * Submits backlog to GitLab with proper link handling.
   * Uses the correct project_id from each accepted match.
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
    
    if (!this.state.gitlabProjectIds || this.state.gitlabProjectIds.length === 0) {
      this.chatUI.appendAssistantMessage(
        '<div class="text-sm text-rose-700">Error: Project does not have GitLab projects configured. Please set the GitLab project paths first.</div>',
        'System Error'
      );
      return;
    }
    
    try {
      // Use first project as default fallback
      const defaultProjectId = this.state.gitlabProjectIds[0];
      
      // Group items by user-selected target project
      const projectPayloads = new Map();
      
      const getProjectPayload = (projectId) => {
        if (!projectPayloads.has(projectId)) {
          projectPayloads.set(projectId, {
            project_id: projectId,
            epics: [],
            issues: []
          });
        }
        return projectPayloads.get(projectId);
      };
      
      // Process epics with user-selected target projects
      this.state.backlogBundle.epics.forEach((epic, epicIdx) => {
        const targetProjectId = epic.target_project_id || defaultProjectId;
        const acceptedMatch = this.getAcceptedMatch(epic.similar);
        
        let epicPayload = {
          title: epic.title,
          description: epic.description || '',
          labels: [],
          target_project_id: targetProjectId,
          related_to_iid: acceptedMatch ? acceptedMatch.id : null
        };
        
        getProjectPayload(targetProjectId).epics.push(epicPayload);
        
        // Process tasks for this epic
        if (epic.tasks && epic.tasks.length > 0) {
          epic.tasks.forEach((task) => {
            const taskProjectId = task.target_project_id || targetProjectId;
            const acceptedTaskMatch = this.getAcceptedMatch(task.similar);
            
            let issuePayload = {
              title: task.title,
              description: task.description || '',
              labels: [],
              target_project_id: taskProjectId,
              related_to_iid: acceptedTaskMatch ? acceptedTaskMatch.id : null,
              parent_epic_index: epicIdx
            };
            
            getProjectPayload(taskProjectId).issues.push(issuePayload);
          });
        }
      });
      
      // Submit using batch endpoint (supports 1+ projects)
      const url = `${this.state.config.gitlabApiBase.replace(/\/$/, '')}/projects/apply-backlog`;
      const response = await this.apiClient.post(url, {
        prompt_id: this.state.promptId || this.state.backlogBundle.prompt_id || '',
        internal_project_id: this.state.projectId,
        projects: Array.from(projectPayloads.values())
      });
      
      // Display results
      const epicsCreated = response.total_epics_created || 0;
      const issuesCreated = response.total_issues_created || 0;
      const errorCount = response.total_errors || 0;
      
      // Collect all created items for display
      const allEpics = [];
      const allIssues = [];
      
      for (const projectResult of response.project_results || []) {
        if (projectResult.results) {
          allEpics.push(...projectResult.results.epics);
          allIssues.push(...projectResult.results.issues);
        }
      }
      
      // Count how many items were linked to similar matches
      const epicsLinked = this.state.backlogBundle.epics.filter(e => this.getAcceptedMatch(e.similar)).length;
      const issuesLinked = this.state.backlogBundle.epics.reduce((count, epic) => {
        return count + (epic.tasks || []).filter(t => this.getAcceptedMatch(t.similar)).length;
      }, 0);
      
      let html = '<div class="text-sm">';
      html += '<div class="font-semibold text-emerald-700 mb-2">✓ Backlog submitted to GitLab successfully!</div>';
      html += '<div class="space-y-1 text-slate-700">';
      
      // Show project breakdown if multiple projects
      if (response.project_results && response.project_results.length > 1) {
        html += `<div class="font-medium mb-1">Submitted to ${response.projects_succeeded} project(s):</div>`;
      }
      
      html += `<div>• Epics: ${epicsCreated} created`;
      if (epicsLinked > 0) html += `, ${epicsLinked} linked to similar items`;
      html += '</div>';
      html += `<div>• Issues: ${issuesCreated} created`;
      if (issuesLinked > 0) html += `, ${issuesLinked} linked to similar items`;
      html += '</div>';
      
      if (errorCount > 0) {
        html += `<div class="text-rose-600 mt-2">⚠ ${errorCount} error(s) occurred</div>`;
        // Show errors from all projects
        for (const projectResult of response.project_results || []) {
          if (projectResult.error_message) {
            html += `<div class="text-rose-600 text-xs">  - Project ${esc(projectResult.project_id)}: ${esc(projectResult.error_message)}</div>`;
          }
          for (const err of projectResult.errors || []) {
            html += `<div class="text-rose-600 text-xs">  - ${esc(err.scope)} ${err.input_index}: ${esc(err.message)}</div>`;
          }
        }
      }
      html += '</div>';
      
      // Add links to created items
      if (allEpics.length > 0 || allIssues.length > 0) {
        html += '<div class="mt-3 text-xs">';
        if (allEpics.slice(0, 3).length > 0) {
          html += '<div class="font-semibold mb-1">Sample Epics:</div>';
          allEpics.slice(0, 3).forEach(epic => {
            html += `<div>• <a href="${esc(epic.web_url)}" target="_blank" class="text-indigo-600 hover:underline">Epic #${esc(epic.id)}</a></div>`;
          });
        }
        if (allIssues.slice(0, 3).length > 0) {
          html += '<div class="font-semibold mb-1 mt-2">Sample Issues:</div>';
          allIssues.slice(0, 3).forEach(issue => {
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
   * @returns {Object|null} Accepted match or null (includes project_id)
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
