/**
 * Base Controller for Chat-Based Screens
 * 
 * Extends BasePageController to provide chat-specific functionality.
 * Used by Requirements and Tasks screens.
 * 
 * Responsibilities:
 * - Chat UI management
 * - Thinking box management  
 * - Chat form handling
 * - Abstract sendRequest() for subclasses
 * 
 * Subclasses must implement:
 * - sendRequest(text) - Handle API requests
 * - getSSEEventHandlers() - Provide SSE event handlers
 * 
 * @module core/base-controller
 */

'use strict';

import { BasePageController } from './base-page-controller.js';
import { ThinkingBoxManager } from '../components/thinking-box-manager.js';
import { ChatUI } from '../components/chat-ui.js';
import { getStatusBadge } from '../utils/status-badges.js';

/**
 * Abstract base class for chat-based screens.
 * Extends BasePageController and adds chat-specific functionality.
 * 
 * Subclasses must implement:
 * - sendRequest(text) - Handle API requests
 * - getSSEEventHandlers() - Provide SSE event handlers
 */
export class ChatBaseController extends BasePageController {
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
    
    // Call parent constructor
    super(containerConfig);
    
    // Initialize chat-specific utilities
    if (this.domElements.chatOutput) {
      this.boxManager = new ThinkingBoxManager(this.domElements.chatOutput);
      this.chatUI = new ChatUI(this.domElements.chatOutput);
    } else {
      console.warn('ChatBaseController: chatOutput element not found');
    }
    
    // Bind chat-specific methods to preserve 'this' context
    this.handleChatSubmit = this.handleChatSubmit.bind(this);
    this.handleChatKeydown = this.handleChatKeydown.bind(this);
  }
  
  /**
   * Creates initial application state.
   * Extends parent state with chat-specific properties.
   * @override
   * @returns {Object} Initial state object
   */
  createInitialState() {
    return {
      ...super.createInitialState(),
      currentBundle: null  // Store current data bundle (requirements/tasks)
    };
  }
  
  /**
   * Resolves DOM elements based on container config.
   * Extends parent implementation with chat-specific elements.
   * @override
   * @param {Object} config - Container configuration
   * @returns {Object} DOM elements object
   */
  resolveDOMElements(config) {
    const elements = super.resolveDOMElements(config);
    
    // Add chat-specific elements
    elements.chatOutput = document.getElementById(config.chatOutputId);
    elements.chatForm = document.getElementById(config.chatFormId || 'chatForm');
    elements.chatInput = document.getElementById(config.chatInputId || 'chatInput');
    
    return elements;
  }
  
  
  /**
   * Sets up all event handlers.
   * Extends parent to add chat form handling.
   * @override
   */
  setupEventHandlers() {
    super.setupEventHandlers();
    this.setupChatForm();
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
   * Updates project status badge in the header.
   * Overrides parent to use imported badge utility.
   * @override
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
   * Finishes all active thinking boxes with error state.
   * Convenience method for error handling in sendRequest.
   * @protected
   */
  finishThinkingBoxesWithError() {
    if (!this.boxManager) return;
    
    // Close pending box if exists
    if (this.boxManager.pendingBox) {
      this.boxManager.pendingBox.finish('error');
      this.boxManager.clearPending();
    }
    
    // Close active box or box linked to current prompt ID
    const box = this.state.promptId 
      ? this.getOrCreateBoxForPromptId(this.state.promptId) 
      : this.boxManager.activeBox;
    if (box && box !== this.boxManager.pendingBox) {
      box.finish('error');
    }
  }
  
  /**
   * Checks if an SSE event belongs to the current project.
   * Helper method for filtering SSE events by project ID.
   * @param {Object} eventData - Parsed SSE event data
   * @returns {boolean} True if event belongs to current project
   * @protected
   */
  isCurrentProjectEvent(eventData) {
    if (!this.state.projectId || !eventData.project_id) {
      return false;
    }
    return String(this.state.projectId) === String(eventData.project_id);
  }
  
  /**
   * Gets SSE event handlers for this page.
   * Provides common 'retrieval_progress' handler that all chat-based pages need.
   * Subclasses should override and merge with their specific handlers.
   * @override
   * @returns {Object} Map of event name to handler function
   */
  getSSEEventHandlers() {
    return {
      'retrieval_progress': (evt) => {
        try {
          const msg = JSON.parse(evt.data);
          if (!this.isCurrentProjectEvent(msg)) {
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

