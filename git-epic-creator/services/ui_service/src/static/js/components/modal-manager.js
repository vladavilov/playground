/**
 * Modal Manager Component
 * 
 * Manages modal dialog lifecycle for editor modals.
 * Provides consistent show/hide behavior and callback handling.
 * 
 * @module components/modal-manager
 */

'use strict';

/**
 * Manages modal dialog lifecycle for editor modals.
 */
export class ModalManager {
  /**
   * @param {HTMLElement} modalElement - The modal container element
   * @param {HTMLElement} contentElement - The modal content container
   * @param {Array<HTMLElement>} closeButtons - Buttons that close the modal
   * @param {HTMLElement|Function|null} saveButtonOrCallback - Save button element or callback function
   * @param {Function|null} saveCallback - Optional callback when save is clicked (if first param is button)
   */
  constructor(modalElement, contentElement, closeButtons = [], saveButtonOrCallback = null, saveCallback = null) {
    if (!modalElement || !contentElement) {
      throw new Error('ModalManager requires modal and content elements');
    }
    this.modal = modalElement;
    this.content = contentElement;
    this.isVisible = false;
    this.boundEscapeHandler = null;
    this.boundBackdropHandler = null;
    this.boundCloseHandlers = [];
    this.boundSaveHandler = null;
    
    // Handle legacy signature: (modal, content, closeButtons, saveCallback)
    // vs new signature: (modal, content, closeButtons, saveButton, saveCallback)
    if (typeof saveButtonOrCallback === 'function') {
      this.saveCallback = saveButtonOrCallback;
      this.saveButton = null;
    } else {
      this.saveButton = saveButtonOrCallback;
      this.saveCallback = saveCallback;
    }
    
    this.setupHandlers(closeButtons);
  }
  
  /**
   * Sets up event handlers for close and save buttons.
   * @private
   */
  setupHandlers(closeButtons) {
    // Setup close button handlers
    closeButtons.forEach(btn => {
      if (btn) {
        const handler = () => this.hide();
        btn.addEventListener('click', handler);
        this.boundCloseHandlers.push({ element: btn, handler });
      }
    });
    
    // Setup save button if provided and callback exists
    if (this.saveButton && this.saveCallback) {
      this.boundSaveHandler = () => {
        this.saveCallback();
        this.hide();
      };
      this.saveButton.addEventListener('click', this.boundSaveHandler);
    } else if (this.saveCallback) {
      // Fallback: try to find save button with warning
      console.warn('ModalManager: No save button element provided, attempting to find it (not recommended)');
      const saveBtn = this.modal.querySelector('#editorSave');
      if (saveBtn) {
        this.saveButton = saveBtn;
        this.boundSaveHandler = () => {
          this.saveCallback();
          this.hide();
        };
        this.saveButton.addEventListener('click', this.boundSaveHandler);
      }
    }
    
    // Close on backdrop click
    this.boundBackdropHandler = (e) => {
      if (e.target === this.modal) {
        this.hide();
      }
    };
    this.modal.addEventListener('click', this.boundBackdropHandler);
    
    // Close on Escape key
    this.boundEscapeHandler = (e) => {
      if (e.key === 'Escape' && this.isVisible) {
        this.hide();
      }
    };
    document.addEventListener('keydown', this.boundEscapeHandler);
  }
  
  /**
   * Shows the modal with provided content.
   * @param {string} html - HTML content to display in modal
   */
  show(html = '') {
    if (html) {
      this.content.innerHTML = html;
    }
    this.modal.classList.remove('hidden');
    this.modal.classList.add('flex');
    this.isVisible = true;
    
    // Focus first input if available
    const firstInput = this.modal.querySelector('input, textarea, select');
    if (firstInput) {
      setTimeout(() => firstInput.focus(), 100);
    }
  }
  
  /**
   * Hides the modal.
   */
  hide() {
    this.modal.classList.add('hidden');
    this.modal.classList.remove('flex');
    this.isVisible = false;
  }
  
  /**
   * Updates the save callback.
   * @param {Function} callback - New save callback
   */
  onSave(callback) {
    this.saveCallback = callback;
  }
  
  /**
   * Gets the modal content element for direct manipulation.
   * @returns {HTMLElement} Content element
   */
  getContent() {
    return this.content;
  }
  
  /**
   * Cleans up all event listeners to prevent memory leaks.
   * Call this before disposing of the ModalManager instance.
   */
  destroy() {
    // Remove close button handlers
    this.boundCloseHandlers.forEach(({ element, handler }) => {
      element.removeEventListener('click', handler);
    });
    this.boundCloseHandlers = [];
    
    // Remove save button handler
    if (this.saveButton && this.boundSaveHandler) {
      this.saveButton.removeEventListener('click', this.boundSaveHandler);
      this.boundSaveHandler = null;
    }
    
    // Remove backdrop handler
    if (this.boundBackdropHandler) {
      this.modal.removeEventListener('click', this.boundBackdropHandler);
      this.boundBackdropHandler = null;
    }
    
    // Remove escape key handler
    if (this.boundEscapeHandler) {
      document.removeEventListener('keydown', this.boundEscapeHandler);
      this.boundEscapeHandler = null;
    }
    
    // Clear references
    this.modal = null;
    this.content = null;
    this.saveButton = null;
    this.saveCallback = null;
  }
}

