/**
 * Projects Page Controller
 * 
 * Main controller for the projects management page.
 * Extends BasePageController and coordinates between components:
 * - ProjectList: Sidebar project list
 * - ProjectDetails: Project details panel
 * - UploadPanel: File upload interface
 * - ProjectModals: Create/edit and delete modals
 * 
 * @module controllers/projects-page-controller
 */

'use strict';

import { BasePageController } from '../core/base-page-controller.js';
import { ApiClient } from '../services/api-client.js';
import { ProjectList } from '../components/project-list.js';
import { ProjectDetails } from '../components/project-details.js';
import { UploadPanel } from '../components/upload-panel.js';
import { ProjectModals } from '../components/project-modals.js';
import { GraphVisualizer } from '../components/graph-visualizer.js';

/**
 * Projects page controller.
 * Manages project CRUD, file uploads, and progress tracking.
 */
class ProjectsPageController extends BasePageController {
  constructor() {
    super({
      // No chat-specific elements needed for projects page
    });
    
    // Page-specific state
    this.selectedProject = null;
    this.filterText = '';
    this.cachingEmbeddings = false;
    
    // Initialize API client with base URL and loading manager
    // This will be fully configured after initialize() runs
    this.projectApi = null;
    this.gitlabApi = null;
    this.neo4jApi = null;
    this.graphVisualizer = null;
  }
  
  /**
   * Creates initial state.
   * @override
   */
  createInitialState() {
    return {
      ...super.createInitialState(),
      projects: []
    };
  }
  
  /**
   * Resolves DOM elements.
   * @override
   */
  resolveDOMElements(config) {
    const elements = super.resolveDOMElements(config);
    
    // Projects panel elements
    elements.projectsPanel = document.getElementById('projectsPanel');
    elements.projectsList = document.getElementById('projectsList');
    elements.projectName = document.getElementById('projectName');
    elements.createProject = document.getElementById('createProject');
    elements.togglePanel = document.getElementById('togglePanel');
    elements.openPanel = document.getElementById('openPanel');
    
    // Main content elements
    elements.projectDetails = document.getElementById('projectDetails');
    elements.uploadSection = document.getElementById('uploadSection');
    
    // Graph modal elements
    elements.graphModal = document.getElementById('graphModal');
    elements.graphContainer = document.getElementById('graphContainer');
    elements.graphModalSubtitle = document.getElementById('graphModalSubtitle');
    elements.graphLoadingOverlay = document.getElementById('graphLoadingOverlay');
    elements.closeGraphModal = document.getElementById('closeGraphModal');
    
    return elements;
  }
  
  /**
   * Called after initialization to set up page-specific components.
   * @override
   */
  async onInitialized() {
    // Initialize API clients with base URLs and loading manager
    const pmBase = this.state.config.projectManagementApiBase.replace(/\/$/, '');
    const gitlabBase = this.state.config.gitlabApiBase.replace(/\/$/, '');
    
    this.projectApi = new ApiClient(
      this.state.config, 
      () => this.handle401Error(),
      { baseUrl: pmBase, loadingManager: this.loadingManager }
    );
    
    this.gitlabApi = new ApiClient(
      this.state.config,
      () => this.handle401Error(),
      { baseUrl: gitlabBase, loadingManager: this.loadingManager }
    );
    
    const neo4jBase = this.state.config.neo4jApiBase?.replace(/\/$/, '') || '/neo4j';
    this.neo4jApi = new ApiClient(
      this.state.config,
      () => this.handle401Error(),
      { baseUrl: neo4jBase, loadingManager: this.loadingManager }
    );
    
    // Initialize components
    this._initializeComponents();
    
    // Load projects
    await this._fetchProjects();
    
    console.log('Projects page initialized with', this.state.projects.length, 'projects');
  }
  
  /**
   * Initializes UI components.
   * @private
   */
  _initializeComponents() {
    // Project list component
    this.projectList = new ProjectList(
      this.domElements.projectsList,
      (project) => this._selectProject(project)
    );
    
    // Project details component
    this.projectDetails = new ProjectDetails(
      this.domElements.projectDetails,
      {
        onEdit: (p) => this.projectModals.openProjectModal(p),
        onDelete: (p) => this.projectModals.openDeleteModal(p),
        onOpenRequirements: (p) => this._openRequirements(p),
        onOpenTasks: (p) => this._openTasks(p),
        onCacheEmbeddings: (p) => this._handleCacheEmbeddings(p),
        onViewGraph: (p) => this._handleViewGraph(p)
      }
    );
    
    // Graph visualizer component
    this.graphVisualizer = new GraphVisualizer({
      containerId: 'graphContainer',
      onError: (err) => {
        console.error('Graph visualization error:', err);
        alert('Failed to visualize graph: ' + (err.message || 'Unknown error'));
      }
    });
    
    // Upload panel component
    this.uploadPanel = new UploadPanel(
      this.domElements.uploadSection,
      (files) => this._handleUpload(files)
    );
    
    // Project modals component
    this.projectModals = new ProjectModals({
      onSave: (payload, isEdit, projectId) => this._saveProject(payload, isEdit, projectId),
      onDelete: (project) => this._deleteProject(project)
    });
  }
  
  /**
   * Sets up page-specific event handlers.
   * @override
   */
  setupPageSpecificHandlers() {
    // Create project button
    if (this.domElements.createProject) {
      this.domElements.createProject.addEventListener('click', () => {
        this.projectModals.openProjectModal(null);
      });
    }
    
    // Search/filter input
    if (this.domElements.projectName) {
      this.domElements.projectName.addEventListener('input', (e) => {
        this.filterText = e.target.value || '';
        this._renderProjectList();
      });
    }
    
    // Panel toggle for mobile
    this._setupPanelToggle();
    
    // Graph modal close handler
    if (this.domElements.closeGraphModal) {
      this.domElements.closeGraphModal.addEventListener('click', () => {
        this._closeGraphModal();
      });
    }
  }
  
  /**
   * Gets SSE event handlers for project progress.
   * @override
   */
  getSSEEventHandlers() {
    return {
      'project_progress': (evt) => {
        try {
          const msg = JSON.parse(evt.data);
          this._handleProjectProgress(msg);
        } catch (err) {
          console.error('Failed to process project_progress event:', err);
        }
      }
    };
  }
  
  /**
   * Fetches projects from API.
   * @private
   */
  async _fetchProjects() {
    try {
      const projects = await this.projectApi.get('/projects');
      this.state.projects = projects || [];
      this._renderProjectList();
    } catch (error) {
      console.error('Failed to fetch projects:', error);
      alert('Failed to load projects: ' + ApiClient.formatError(error));
    }
  }
  
  /**
   * Renders the project list.
   * @private
   */
  _renderProjectList() {
    this.projectList.render(this.state.projects, this.selectedProject, this.filterText);
  }
  
  /**
   * Selects a project.
   * @private
   */
  _selectProject(project) {
    this.selectedProject = project;
    this._renderProjectList();
    this.projectDetails.render(project, this.cachingEmbeddings);
    this.uploadPanel.render(project);
    
    // Hide mobile panel after selection
    if (window.innerWidth < 1024) {
      this._hideMobilePanel();
    }
  }
  
  /**
   * Saves a project (create or update).
   * @private
   */
  async _saveProject(payload, isEdit, projectId) {
    try {
      if (isEdit) {
        await this.projectApi.put(`/projects/${projectId}`, payload);
      } else {
        await this.projectApi.post('/projects', payload);
      }
      
      // Reload projects
      await this._fetchProjects();
      
      // Update selected project if it was edited
      if (isEdit && this.selectedProject && String(this.selectedProject.id) === String(projectId)) {
        this.selectedProject = this.state.projects.find(p => String(p.id) === String(projectId)) || null;
        this.projectDetails.render(this.selectedProject, this.cachingEmbeddings);
      }
    } catch (error) {
      throw error; // Re-throw for modal to handle
    }
  }
  
  /**
   * Deletes a project.
   * @private
   */
  async _deleteProject(project) {
    try {
      await this.projectApi.delete(`/projects/${project.id}`);
      
      // Clear selection if deleted project was selected
      if (this.selectedProject && this.selectedProject.id === project.id) {
        this.selectedProject = null;
        this.projectDetails.render(null);
        this.uploadPanel.render(null);
      }
      
      // Reload projects
      await this._fetchProjects();
    } catch (error) {
      throw error; // Re-throw for modal to handle
    }
  }
  
  /**
   * Handles file upload.
   * @private
   */
  async _handleUpload(files) {
    if (!this.selectedProject) return;
    
    const formData = new FormData();
    for (const f of files) {
      formData.append('files', f);
    }
    
    try {
      await this.projectApi.postForm(`/projects/${this.selectedProject.id}/documents/upload`, formData);
    } catch (error) {
      alert('Upload failed: ' + ApiClient.formatError(error));
    }
  }
  
  /**
   * Handles cache embeddings request.
   * @private
   */
  async _handleCacheEmbeddings(project) {
    if (this.cachingEmbeddings) return;
    
    const gitlabProjectIds = project.gitlab_backlog_project_ids;
    if (!gitlabProjectIds || gitlabProjectIds.length === 0) {
      alert('No GitLab backlog projects linked');
      return;
    }
    
    try {
      this.cachingEmbeddings = true;
      this.projectDetails.render(project, this.cachingEmbeddings);
      
      const url = `/projects/multi/cache-embeddings?project_id=${project.id}&gitlab_project_ids=${gitlabProjectIds.join(',')}`;
      await this.gitlabApi.post(url, {});
      
      this.uploadPanel.appendLog(`Caching embeddings for ${gitlabProjectIds.length} project(s)`, 'text-emerald-600');
    } catch (error) {
      console.error('Failed to cache embeddings:', error);
      alert('Failed to cache embeddings: ' + ApiClient.formatError(error));
    } finally {
      this.cachingEmbeddings = false;
      this.projectDetails.render(project, this.cachingEmbeddings);
    }
  }
  
  /**
   * Opens requirements page for a project.
   * @private
   */
  _openRequirements(project) {
    window.location.href = `/pages/requirements.html?project_id=${project.id}`;
  }
  
  /**
   * Opens tasks page for a project.
   * @private
   */
  _openTasks(project) {
    window.location.href = `/pages/tasks.html?project_id=${project.id}`;
  }
  
  /**
   * Handles project progress SSE events.
   * @private
   */
  _handleProjectProgress(msg) {
    try {
      const pid = msg?.project_id;
      if (pid == null) return;
      
      const idStr = String(pid);
      const nextStatus = (msg?.status || '').toLowerCase();
      const pctVal = Number.isFinite(msg?.processed_pct) ? msg.processed_pct : null;
      
      // Update project in list (match against project.id - Project Management Service UUID)
      for (const project of this.state.projects) {
        // Compare project_id from message against project.id from Project Management Service
        if (String(project.id) !== idStr) continue;
        
        let changed = false;
        if (nextStatus && nextStatus !== (project.status || '').toLowerCase()) {
          project.status = nextStatus;
          changed = true;
        }
        if (pctVal != null && pctVal !== project.processed_pct) {
          project.processed_pct = pctVal;
          changed = true;
        }
        if (msg.timestamp) {
          const ts = new Date(msg.timestamp);
          if (!isNaN(ts.getTime())) {
            project.updated_at = ts.toISOString();
            changed = true;
          }
        }
        
        if (changed) {
          this._renderProjectList();
          
          // Update details if this project is selected
          if (this.selectedProject && this.selectedProject.id === project.id) {
            Object.assign(this.selectedProject, project);
            this.projectDetails.render(this.selectedProject, this.cachingEmbeddings);
          }
        }
        break;
      }
      
      // Update upload panel if selected project matches
      if (this.selectedProject && String(this.selectedProject.id) === idStr) {
        const stepLabel = (msg.process_step && String(msg.process_step).trim() !== '') 
          ? String(msg.process_step) 
          : (nextStatus || 'unknown');
        
        const safePct = pctVal != null ? Math.max(0, Math.min(100, Math.round(pctVal))) : null;
        
        this.uploadPanel.updateProgress(stepLabel, safePct, nextStatus);
        this.uploadPanel.appendLog(stepLabel);
      }
    } catch (err) {
      console.error('Failed to handle project progress:', err);
    }
  }
  
  /**
   * Sets up mobile panel toggle.
   * @private
   */
  _setupPanelToggle() {
    const panel = this.domElements.projectsPanel;
    const openBtn = this.domElements.openPanel;
    const hideBtn = this.domElements.togglePanel;
    
    if (openBtn) {
      openBtn.addEventListener('click', () => this._showMobilePanel());
    }
    
    if (hideBtn) {
      hideBtn.addEventListener('click', () => this._hideMobilePanel());
    }
    
    // Close on project selection (handled in _selectProject)
  }
  
  /**
   * Shows mobile panel as overlay.
   * @private
   */
  _showMobilePanel() {
    const panel = this.domElements.projectsPanel;
    if (!panel) return;
    
    panel.classList.remove('hidden');
    
    if (window.innerWidth < 1024) {
      panel.style.position = 'fixed';
      panel.style.top = '0';
      panel.style.left = '0';
      panel.style.bottom = '0';
      panel.style.zIndex = '40';
      panel.classList.add('shadow-2xl');
      
      const hideBtn = this.domElements.togglePanel;
      if (hideBtn) hideBtn.classList.remove('lg:hidden');
    }
  }
  
  /**
   * Hides mobile panel.
   * @private
   */
  _hideMobilePanel() {
    const panel = this.domElements.projectsPanel;
    if (!panel) return;
    
    if (window.innerWidth < 1024) {
      panel.classList.add('hidden');
      panel.style.position = '';
      panel.style.top = '';
      panel.style.left = '';
      panel.style.bottom = '';
      panel.style.zIndex = '';
      panel.classList.remove('shadow-2xl');
      
      const hideBtn = this.domElements.togglePanel;
      if (hideBtn) hideBtn.classList.add('lg:hidden');
    }
  }
  
  /**
   * Handles viewing the knowledge graph for a project.
   * @private
   */
  async _handleViewGraph(project) {
    if (!project || !project.id) {
      console.error('Cannot view graph: No project selected');
      return;
    }
    
    try {
      // Show modal with loading state
      this._openGraphModal(project);
      
      // Fetch graph data from backend
      console.log('Fetching graph data for project:', project.id);
      const graphData = await this.neo4jApi.get(`/project/${project.id}/graph`);
      
      // Update subtitle with stats
      if (this.domElements.graphModalSubtitle) {
        const stats = graphData.stats || {};
        const nodeCount = stats.node_count || 0;
        const relCount = stats.relationship_count || 0;
        this.domElements.graphModalSubtitle.textContent = 
          `${project.name} • ${nodeCount} nodes • ${relCount} relationships`;
      }
      
      // Hide loading overlay
      if (this.domElements.graphLoadingOverlay) {
        this.domElements.graphLoadingOverlay.classList.add('hidden');
      }
      
      // Render graph
      await this.graphVisualizer.renderFromData(graphData);
      
      console.log('Graph visualization rendered successfully');
      
    } catch (error) {
      console.error('Failed to load graph:', error);
      
      // Hide loading overlay
      if (this.domElements.graphLoadingOverlay) {
        this.domElements.graphLoadingOverlay.classList.add('hidden');
      }
      
      // Show error in graph container
      if (this.domElements.graphContainer) {
        this.domElements.graphContainer.innerHTML = `
          <div style="display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; color: #dc2626; text-align: center; padding: 2rem;">
            <svg style="width: 64px; height: 64px; margin-bottom: 1rem;" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5" d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"></path>
            </svg>
            <h3 style="font-size: 1.125rem; font-weight: 600; margin-bottom: 0.5rem;">Failed to Load Graph</h3>
            <p style="color: #64748b; max-width: 400px;">${ApiClient.formatError(error)}</p>
            <button onclick="document.getElementById('closeGraphModal').click()" 
                    style="margin-top: 1rem; padding: 0.5rem 1rem; background: #4f46e5; color: white; border-radius: 0.375rem; border: none; cursor: pointer;">
              Close
            </button>
          </div>
        `;
      }
    }
  }
  
  /**
   * Opens the graph visualization modal.
   * @private
   */
  _openGraphModal(project) {
    if (!this.domElements.graphModal) return;
    
    // Show modal
    this.domElements.graphModal.classList.remove('hidden');
    this.domElements.graphModal.classList.add('flex');
    
    // Show loading overlay
    if (this.domElements.graphLoadingOverlay) {
      this.domElements.graphLoadingOverlay.classList.remove('hidden');
    }
    
    // Clear previous graph
    if (this.domElements.graphContainer) {
      this.domElements.graphContainer.innerHTML = '';
    }
    
    // Set subtitle
    if (this.domElements.graphModalSubtitle) {
      this.domElements.graphModalSubtitle.textContent = 'Loading graph data...';
    }
  }
  
  /**
   * Closes the graph visualization modal.
   * @private
   */
  _closeGraphModal() {
    if (!this.domElements.graphModal) return;
    
    // Hide modal
    this.domElements.graphModal.classList.add('hidden');
    this.domElements.graphModal.classList.remove('flex');
    
    // Clean up graph visualizer
    if (this.graphVisualizer) {
      this.graphVisualizer.destroy();
    }
    
    // Clear container
    if (this.domElements.graphContainer) {
      this.domElements.graphContainer.innerHTML = '';
    }
  }
}

// Initialize controller
const controller = new ProjectsPageController();
controller.initialize().catch(err => console.error('Projects page initialization failed:', err));

