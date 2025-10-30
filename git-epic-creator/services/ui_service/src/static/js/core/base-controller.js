/**
 * Base Controller for Chat-Based Screens
 * 
 * Provides common functionality for Requirements and Tasks screens following
 * SOLID principles and DRY practices. Screens extend this class and implement
 * abstract methods for screen-specific behavior.
 * 
 * @module core/base-controller
 */

'use strict';

import { ThinkingBoxManager } from '../components/thinking-box-manager.js';
import { ChatUI } from '../components/chat-ui.js';
import {
  fetchConfig,
  checkAuthStatus,
  fetchGitLabStatus,
  startGitLabSSO,
  startPeriodicGitLabStatusCheck,
  connectSSE
} from '../utils/connections.js';
import { getStatusBadge } from '../utils/status-badges.js';

/**
 * Abstract base class for chat-based screens.
 * Handles common initialization, event handling, SSE connection, and GitLab integration.
 * 
 * Subclasses must implement:
 * - sendRequest(text) - Handle API requests
 * - getSSEEventHandlers() - Provide SSE event handlers
 * - loadProject() - Load project data (optional override)
 * - setupScreenSpecificHandlers() - Setup screen-specific event handlers
 * - onInitialized() - Screen-specific initialization (optional)
 */
export class ChatBaseController {
  /**
   * Creates a new chat base controller.
   * @param {Object} containerConfig - Configuration for DOM element IDs
   * @param {string} containerConfig.chatOutputId - Chat output container ID
   * @param {string} containerConfig.chatFormId - Chat form ID
   * @param {string} containerConfig.chatInputId - Chat input textarea ID
   * @param {string} [containerConfig.projectTitleId] - Project title element ID
   * @param {string} [containerConfig.projectStatusId] - Project status element ID
   */
  constructor(containerConfig) {
    if (!containerConfig || !containerConfig.chatOutputId) {
      throw new Error('ChatBaseController requires containerConfig with chatOutputId');
    }
    
    this.containerConfig = containerConfig;
    this.state = this.createInitialState();
    this.domElements = this.resolveDOMElements(containerConfig);
    
    // Initialize utilities
    if (this.domElements.chatOutput) {
      this.boxManager = new ThinkingBoxManager(this.domElements.chatOutput);
      this.chatUI = new ChatUI(this.domElements.chatOutput);
    } else {
      console.warn('ChatBaseController: chatOutput element not found');
    }
    
    // Bind methods to preserve 'this' context
    this.handleChatSubmit = this.handleChatSubmit.bind(this);
    this.handleChatKeydown = this.handleChatKeydown.bind(this);
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
    
    // Required elements
    elements.chatOutput = document.getElementById(config.chatOutputId);
    elements.chatForm = document.getElementById(config.chatFormId || 'chatForm');
    elements.chatInput = document.getElementById(config.chatInputId || 'chatInput');
    
    // Optional elements
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
   * Sets up authentication, project loading, GitLab, event handlers, and SSE.
   * @returns {Promise<void>}
   */
  async initialize() {
    try {
      // Load configuration
      this.state.config = await fetchConfig();
      if (!this.state.config) {
        throw new Error('Failed to load configuration');
      }
      
      // Check authentication
      const authenticated = await checkAuthStatus(this.state);
      if (!authenticated) {
        return; // Will redirect to login
      }
      
      // Load project
      await this.loadProject();
      
      // Initialize GitLab connection
      await this.initializeGitLab();
      
      // Setup event handlers
      this.setupEventHandlers();
      
      // Connect to SSE
      this.connectSSE();
      
      // Screen-specific initialization
      await this.onInitialized();
      
      console.log('ChatBaseController initialized successfully');
    } catch (error) {
      console.error('ChatBaseController initialization failed:', error);
      if (this.chatUI) {
        this.chatUI.appendSystemMessage(`Initialization error: ${error.message}`);
      }
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
    
    // Subclasses must initialize apiClient before loadProject is called
    // This is a no-op if apiClient is not available
    if (!this.apiClient) {
      return;
    }
    
    try {
      const base = this.state.config.projectManagementApiBase.replace(/\/$/, '');
      this.state.project = await this.apiClient.get(`${base}/projects/${this.state.projectId}`);
      
      if (this.domElements.projectTitle) {
        this.domElements.projectTitle.textContent = this.state.project.name || '(untitled)';
      }
      
      this.updateProjectStatus(this.state.project.status);
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
   * Combines common handlers with screen-specific handlers.
   */
  setupEventHandlers() {
    this.setupChatForm();
    this.setupGitLabConnection();
    this.setupScreenSpecificHandlers();
  }
  
  /**
   * Sets up chat form submission and keyboard handling.
   * @private
   */
  setupChatForm() {
    // Keyboard handling (Shift+Enter for newline, Enter to send)
    if (this.domElements.chatInput) {
      this.domElements.chatInput.addEventListener('keydown', this.handleChatKeydown);
    }
    
    // Form submission
    if (this.domElements.chatForm) {
      this.domElements.chatForm.addEventListener('submit', this.handleChatSubmit);
    }
  }
  
  /**
   * Handles chat input keyboard events.
   * @param {KeyboardEvent} e - Keyboard event
   * @private
   */
  handleChatKeydown(e) {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      if (this.domElements.chatForm) {
        this.domElements.chatForm.requestSubmit();
      }
    }
  }
  
  /**
   * Handles chat form submission.
   * @param {Event} e - Form submit event
   * @private
   */
  async handleChatSubmit(e) {
    e.preventDefault();
    const text = this.domElements.chatInput?.value.trim();
    if (!text) return;
    
    // Clear input
    if (this.domElements.chatInput) {
      this.domElements.chatInput.value = '';
    }
    
    // Show user message
    if (this.chatUI) {
      this.chatUI.appendUserMessage(text);
    }
    
    // Create pending thinking box
    if (this.boxManager) {
      const pending = this.boxManager.createBox(null);
      this.boxManager.pendingBox = pending;
    }
    
    // Send request (implemented by subclass)
    try {
      await this.sendRequest(text);
    } catch (error) {
      console.error('Failed to send request:', error);
      if (this.chatUI) {
        this.chatUI.appendSystemMessage(`Error: ${error.message}`);
      }
      // Finish thinking box with error status
      if (this.boxManager?.pendingBox) {
        this.boxManager.pendingBox.finish('error');
        this.boxManager.clearPending();
      }
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
   * Starts GitLab SSO flow.
   * Can be overridden by subclasses if needed.
   */
  startGitLabSSO() {
    startGitLabSSO(this.state.config);
  }
  
  /**
   * Sets up screen-specific event handlers.
   * Must be implemented by subclasses.
   */
  setupScreenSpecificHandlers() {
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
   * Gets SSE event handlers for this screen.
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
   * @param {string} status - Status string
   */
  updateProjectStatus(status) {
    if (this.domElements.projectStatus) {
      this.domElements.projectStatus.innerHTML = '';
      this.domElements.projectStatus.appendChild(getStatusBadge(status));
    }
  }
  
  /**
   * Gets or creates a thinking box for a prompt ID.
   * Convenience method that delegates to boxManager.
   * @param {string|null} promptId - Prompt ID
   * @returns {Object|null} Box wrapper or null if boxManager not available
   */
  getOrCreateBoxForPromptId(promptId) {
    if (!this.boxManager) return null;
    return this.boxManager.getOrCreateForPromptId(promptId);
  }
  
  /**
   * Called after initialization is complete.
   * Override in subclasses for screen-specific initialization logic.
   * @returns {Promise<void>}
   */
  async onInitialized() {
    // Override in subclasses
  }
  
  /**
   * Sends a request to the API.
   * Must be implemented by subclasses.
   * @param {string} text - Request text/prompt
   * @returns {Promise<void>}
   * @throws {Error} If not implemented by subclass
   */
  async sendRequest(text) {
    throw new Error('sendRequest must be implemented by subclass');
  }
}

