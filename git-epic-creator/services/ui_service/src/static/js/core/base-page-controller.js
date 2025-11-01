/**
 * Base Page Controller
 * 
 * Foundation class for all page controllers in the application.
 * 
 * Responsibilities:
 * - Configuration loading
 * - Authentication management
 * - GitLab connection management
 * - SSE connection and event routing
 * - Loading state management
 * - Connections panel UI updates
 * - Optional project loading
 * 
 * Subclasses can override:
 * - createInitialState() - Add screen-specific state
 * - resolveDOMElements(config) - Add screen-specific DOM elements
 * - setupPageSpecificHandlers() - Setup page-specific event handlers
 * - getSSEEventHandlers() - Provide SSE event handlers
 * - loadProject() - Load project data (optional)
 * - onInitialized() - Post-initialization hook
 * 
 * @module core/base-page-controller
 */

'use strict';

import {
  fetchConfig,
  checkAuthStatus,
  fetchGitLabStatus,
  startGitLabSSO,
  startPeriodicGitLabStatusCheck,
  connectSSE,
  handleLogout
} from '../utils/connections.js';
import { createLoadingManager } from '../utils/dom-helpers.js';
import { ApiClient } from '../services/api-client.js';

/**
 * Abstract base class for all page controllers.
 * Handles common initialization, configuration, authentication, and connections.
 */
export class BasePageController {
  /**
   * Creates a new base page controller.
   * @param {Object} [containerConfig={}] - Configuration for DOM element IDs
   */
  constructor(containerConfig = {}) {
    this.containerConfig = containerConfig;
    this.state = this.createInitialState();
    this.domElements = this.resolveDOMElements(containerConfig);
    
    // Initialize loading manager
    this.loadingManager = createLoadingManager();
    
    // API client will be initialized after config is loaded
    this.apiClient = null;
    
    // Bind methods to preserve 'this' context
    this.handleLogout = this.handleLogout.bind(this);
    this.startGitLabSSO = this.startGitLabSSO.bind(this);
  }
  
  /**
   * Creates initial application state.
   * Can be overridden by subclasses to add screen-specific state.
   * @returns {Object} Initial state object
   */
  createInitialState() {
    return {
      config: null,
      projectId: null,
      project: null,
      authenticated: false,
      gitlab: { status: 'connecting', configured: false },
      rtEvents: { status: 'disconnected' },
      evtSource: null
    };
  }
  
  /**
   * Resolves DOM elements based on container config.
   * Can be extended by subclasses for additional elements.
   * @param {Object} config - Container configuration
   * @returns {Object} DOM elements object
   */
  resolveDOMElements(config) {
    const elements = {};
    
    // Common optional elements
    if (config.projectTitleId) {
      elements.projectTitle = document.getElementById(config.projectTitleId);
    }
    if (config.projectStatusId) {
      elements.projectStatus = document.getElementById(config.projectStatusId);
    }
    
    return elements;
  }
  
  /**
   * Main initialization method.
   * Sets up configuration, authentication, GitLab, event handlers, and SSE.
   * @returns {Promise<void>}
   */
  async initialize() {
    try {
      // Load configuration
      this.state.config = await fetchConfig();
      if (!this.state.config) {
        throw new Error('Failed to load configuration');
      }
      
      // Initialize API client with config and 401 handler
      this.apiClient = new ApiClient(this.state.config, () => this.handle401Error());
      
      // Check authentication
      const authenticated = await checkAuthStatus(this.state);
      if (!authenticated) {
        return; // Will redirect to login
      }
      
      // Setup logout button handler
      this.setupLogoutHandler();
      
      // Load project (if applicable)
      await this.loadProject();
      
      // Initialize GitLab connection
      await this.initializeGitLab();
      
      // Setup event handlers
      this.setupEventHandlers();
      
      // Connect to SSE
      this.connectSSE();
      
      // Screen-specific initialization
      await this.onInitialized();
      
      console.log(`${this.constructor.name} initialized successfully`);
    } catch (error) {
      console.error(`${this.constructor.name} initialization failed:`, error);
      throw error;
    }
  }
  
  /**
   * Loads project from URL query parameters.
   * Reads project_id from URLSearchParams and fetches project details.
   * Can be overridden or extended in subclasses for additional functionality.
   * @returns {Promise<void>}
   */
  async loadProject() {
    // Parse project_id from URL query parameters
    const params = new URLSearchParams(window.location.search);
    this.state.projectId = params.get('project_id');
    
    if (!this.state.projectId) {
      return; // No project ID provided
    }
    
    if (!this.apiClient || !this.state.config) {
      return;
    }
    
    try {
      const base = this.state.config.projectManagementApiBase.replace(/\/$/, '');
      this.state.project = await this.apiClient.get(`${base}/projects/${this.state.projectId}`);
      
      if (this.domElements.projectTitle) {
        this.domElements.projectTitle.textContent = this.state.project.name || '(untitled)';
      }
      
      if (this.domElements.projectStatus && this.state.project.status) {
        this.updateProjectStatus(this.state.project.status);
      }
    } catch (error) {
      console.error('Failed to load project:', error);
      // Non-fatal, continue without project details
    }
  }
  
  /**
   * Initializes GitLab connection and starts periodic status checks.
   * @returns {Promise<void>}
   */
  async initializeGitLab() {
    try {
      await fetchGitLabStatus(this.state, this.state.config);
      startPeriodicGitLabStatusCheck(this.state, this.state.config);
    } catch (err) {
      console.error('GitLab initialization failed:', err);
      // Non-fatal, continue without GitLab
    }
  }
  
  /**
   * Sets up all event handlers.
   * Combines common handlers with page-specific handlers.
   */
  setupEventHandlers() {
    this.setupGitLabConnection();
    this.setupPageSpecificHandlers();
  }
  
  /**
   * Sets up logout button handler.
   * @private
   */
  setupLogoutHandler() {
    const logoutBtn = document.getElementById('logoutBtn');
    if (logoutBtn) {
      logoutBtn.addEventListener('click', this.handleLogout);
    }
  }
  
  /**
   * Sets up GitLab connection button handler.
   * @private
   */
  setupGitLabConnection() {
    const gitlabConnectBtn = document.getElementById('gitlabConnectBtn');
    if (gitlabConnectBtn) {
      gitlabConnectBtn.addEventListener('click', () => {
        if (this.state.gitlab?.configured) {
          this.startGitLabSSO();
        }
      });
    }
  }
  
  /**
   * Handles user logout.
   * Delegates to shared logout handler with loading manager.
   */
  async handleLogout() {
    await handleLogout(this.loadingManager);
  }
  
  /**
   * Starts GitLab SSO flow.
   * Can be overridden by subclasses if needed.
   */
  startGitLabSSO() {
    startGitLabSSO(this.state.config);
  }
  
  /**
   * Sets up page-specific event handlers.
   * Must be implemented by subclasses.
   */
  setupPageSpecificHandlers() {
    // Override in subclasses
  }
  
  /**
   * Connects to SSE and registers event handlers.
   */
  connectSSE() {
    const eventHandlers = this.getSSEEventHandlers();
    connectSSE(this.state, eventHandlers);
  }
  
  /**
   * Gets SSE event handlers for this page.
   * Must be implemented by subclasses.
   * @returns {Object} Map of event name to handler function
   */
  getSSEEventHandlers() {
    // Override in subclasses
    return {};
  }
  
  /**
   * Handles 401 Unauthorized errors by refreshing GitLab status.
   * Called automatically when API requests return 401.
   */
  handle401Error() {
    console.warn('Received 401 Unauthorized, refreshing GitLab status');
    fetchGitLabStatus(this.state, this.state.config).catch(err => {
      console.error('Failed to refresh GitLab status after 401:', err);
    });
  }
  
  /**
   * Updates project status badge in the header.
   * Subclasses can override for custom status display.
   * @param {string} status - Status string
   */
  updateProjectStatus(status) {
    // Default implementation - subclasses can override
    if (this.domElements.projectStatus) {
      // Import and use status badge utility if needed
      import('../utils/status-badges.js').then(module => {
        this.domElements.projectStatus.innerHTML = '';
        this.domElements.projectStatus.appendChild(module.getStatusBadge(status));
      });
    }
  }
  
  /**
   * Called after initialization is complete.
   * Override in subclasses for page-specific initialization logic.
   * @returns {Promise<void>}
   */
  async onInitialized() {
    // Override in subclasses
  }
  
  /**
   * Shows loading overlay.
   * Convenience method for subclasses.
   */
  showLoading() {
    this.loadingManager.show();
  }
  
  /**
   * Hides loading overlay.
   * Convenience method for subclasses.
   */
  hideLoading() {
    this.loadingManager.hide();
  }
}

