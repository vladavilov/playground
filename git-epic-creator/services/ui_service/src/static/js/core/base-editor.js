/**
 * Base Editor
 * 
 * Provides common editing functionality for all editors following DRY and SOLID principles.
 * Handles multi-mode editing (inline, focus, fullscreen) with auto-save, keyboard shortcuts,
 * and live preview capabilities.
 * 
 * Subclasses must implement:
 * - getItemLabel(item, index) - Returns display label for item
 * - getItemFields(item) - Returns array of field configurations
 * - renderPreviewContent(fields) - Renders preview HTML
 * - setupInlineEditing() - Sets up item-specific inline editing
 * 
 * @module core/base-editor
 */

'use strict';

import { escapeHtml as esc } from '../utils/dom-helpers.js';
import { initializeMermaid } from '../utils/markdown-renderer.js';

/**
 * Abstract base class for editors.
 * Provides common editing patterns for focus mode, fullscreen mode, and auto-save.
 */
export class BaseEditor {
  constructor(bundle, onSave) {
    if (!bundle || !onSave) {
      throw new Error('BaseEditor requires bundle and onSave callback');
    }
    
    this.bundle = bundle;
    this.onSave = onSave;
    this.autoSaveTimeout = null;
    this.currentMode = 'inline'; // 'inline', 'focus', 'fullscreen'
    this.editingItem = null;
    this.unsavedChanges = false;
    this.onDelete = null; // Callback for item deletion
    this.onAIEnhance = null; // Callback for AI enhancement
    
    // Bind methods
    this.handleAutoSave = this.handleAutoSave.bind(this);
    this.handleKeyboardShortcuts = this.handleKeyboardShortcuts.bind(this);
  }
  
  /**
   * Sets the callback for item deletion.
   * @param {Function} callback - Function to call when an item is deleted
   */
  setDeleteCallback(callback) {
    this.onDelete = callback;
  }
  
  /**
   * Sets the callback for AI enhancement.
   * @param {Function} callback - Function to call when AI enhancement is requested
   */
  setAIEnhanceCallback(callback) {
    this.onAIEnhance = callback;
  }
  
  /**
   * Enables inline editing for all items.
   * Must be implemented by subclasses.
   */
  enableInlineEditing() {
    throw new Error('enableInlineEditing must be implemented by subclass');
  }
  
  /**
   * Gets display label for an item (e.g., "Epic 1", "BR1").
   * Must be implemented by subclasses.
   * @param {Object} item - Item object
   * @param {number} index - Item index
   * @returns {string} Display label
   */
  getItemLabel(item, index) {
    throw new Error('getItemLabel must be implemented by subclass');
  }
  
  /**
   * Gets field configurations for an item.
   * Must be implemented by subclasses.
   * @param {Object} item - Item object
   * @returns {Array<Object>} Array of field configs with {id, label, value, type, placeholder, rows, helpText}
   */
  getItemFields(item) {
    throw new Error('getItemFields must be implemented by subclass');
  }
  
  /**
   * Renders preview content HTML.
   * Must be implemented by subclasses.
   * @param {Object} fields - Map of field IDs to values
   * @param {Object} item - Original item object
   * @returns {string} HTML string for preview
   */
  renderPreviewContent(fields, item) {
    throw new Error('renderPreviewContent must be implemented by subclass');
  }
  
  /**
   * Gets item type label (e.g., "Epic", "Task", "Requirement").
   * Can be overridden by subclasses.
   * @returns {string} Type label
   */
  getItemTypeLabel() {
    return 'Item';
  }
  
  /**
   * Sets up inline editing for a specific field.
   * Common implementation used by subclasses.
   * @protected
   */
  setupInlineField(card, selector, item, field, updateCallback) {
    const element = card.querySelector(selector);
    if (!element) return;
    
    // Add edit indicator on hover
    element.classList.add('cursor-pointer', 'hover:bg-indigo-50', 'transition-colors', 'rounded', 'px-2', 'py-1', '-mx-2', '-my-1');
    element.title = 'Click to edit';
    
    element.addEventListener('click', (e) => {
      if (element.querySelector('textarea, input')) return; // Already editing
      
      const currentValue = item[field] || '';
      const isMultiline = field === 'description' || field === 'acceptance_criteria';
      const isArray = field === 'acceptance_criteria';
      
      // Create input element
      const input = isMultiline 
        ? this.createAutoExpandingTextarea(isArray && Array.isArray(item[field]) ? item[field].join('\n') : currentValue)
        : this.createInput(currentValue);
      
      // Style input to take full width and match design
      const baseClasses = 'w-full block border border-indigo-300 rounded-lg px-3 py-2 text-sm focus:ring-2 focus:ring-indigo-500 focus:border-transparent transition-all';
      input.className = baseClasses;
      
      // Replace content with input
      const originalHTML = element.innerHTML;
      element.innerHTML = '';
      element.classList.remove('px-2', 'py-1', '-mx-2', '-my-1');
      element.appendChild(input);
      input.focus();
      
      // Save on blur
      const save = () => {
        const newValue = input.value.trim();
        if (newValue !== currentValue) {
          // Handle array fields (like acceptance_criteria)
          if (isArray) {
            item[field] = newValue ? newValue.split('\n').filter(l => l.trim()) : [];
          } else {
            item[field] = newValue;
          }
          this.markUnsaved();
          this.scheduleAutoSave();
          if (updateCallback) updateCallback();
        }
        element.innerHTML = originalHTML;
        element.textContent = newValue || currentValue;
        this.setupInlineField(card, selector, item, field, updateCallback);
      };
      
      // Cancel on Escape
      const cancel = () => {
        element.innerHTML = originalHTML;
        element.classList.add('px-2', 'py-1', '-mx-2', '-my-1');
        this.setupInlineField(card, selector, item, field, updateCallback);
      };
      
      input.addEventListener('blur', save);
      input.addEventListener('keydown', (e) => {
        if (e.key === 'Escape') {
          e.preventDefault();
          cancel();
        } else if (e.key === 'Enter' && !isMultiline) {
          e.preventDefault();
          save();
        }
      });
    });
  }
  
  /**
   * Adds focus mode button to item card.
   * Common implementation used by subclasses.
   * @protected
   */
  addFocusModeButton(card, item, callback) {
    const container = card.querySelector('.card-actions-container');
    if (!container) return;
    
    const btn = document.createElement('button');
    btn.className = 'p-1.5 text-slate-400 hover:text-indigo-600 hover:bg-indigo-50 rounded transition-colors';
    btn.title = 'Open in focus mode (F)';
    btn.innerHTML = `
      <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 8V4m0 0h4M4 4l5 5m11-1V4m0 0h-4m4 0l-5 5M4 16v4m0 0h4m-4 0l5-5m11 5l-5-5m5 5v-4m0 4h-4"></path>
      </svg>
    `;
    
    btn.addEventListener('click', (e) => {
      e.stopPropagation();
      callback();
    });
    
    container.appendChild(btn);
  }
  
  /**
   * Adds delete button to item card.
   * Common implementation used by subclasses.
   * @protected
   */
  addDeleteButton(card, item, callback) {
    const container = card.querySelector('.card-actions-container');
    if (!container) return;
    
    const btn = document.createElement('button');
    btn.className = 'p-1.5 text-slate-400 hover:text-rose-600 hover:bg-rose-50 rounded transition-colors';
    btn.title = 'Delete this item';
    btn.innerHTML = `
      <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"></path>
      </svg>
    `;
    
    btn.addEventListener('click', (e) => {
      e.stopPropagation();
      if (confirm('Are you sure you want to delete this item?')) {
        callback();
      }
    });
    
    container.appendChild(btn);
  }
  
  /**
   * Adds AI enhancement button to item card.
   * Common implementation used by subclasses.
   * @protected
   */
  addAIEnhanceButton(card, item, callback) {
    const container = card.querySelector('.card-actions-container');
    if (!container) return;
    
    const btn = document.createElement('button');
    btn.className = 'p-1.5 text-slate-400 hover:text-purple-600 hover:bg-purple-50 rounded transition-colors';
    btn.title = 'Enhance with AI';
    btn.innerHTML = `
      <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
        <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z"></path>
      </svg>
    `;
    
    btn.addEventListener('click', (e) => {
      e.stopPropagation();
      callback();
    });
    
    container.appendChild(btn);
  }
  
  /**
   * Opens focus mode for single item editing.
   */
  openFocusMode(item, metadata = {}) {
    this.currentMode = 'focus';
    this.editingItem = { item, ...metadata };
    
    const modal = this.createFocusModal(item, metadata);
    document.body.appendChild(modal);
    
    // Setup keyboard shortcuts
    document.addEventListener('keydown', this.handleKeyboardShortcuts);
    
    // Focus first input
    setTimeout(() => {
      const firstInput = modal.querySelector('input, textarea');
      if (firstInput) firstInput.focus();
    }, 100);
  }
  
  /**
   * Creates focus mode modal for detailed editing.
   * @protected
   */
  createFocusModal(item, metadata) {
    const modal = document.createElement('div');
    modal.id = 'focusEditorModal';
    modal.className = 'fixed inset-0 bg-slate-900/70 backdrop-blur-sm flex items-center justify-center z-50 p-4 animate-fade-in';
    
    const itemLabel = this.getItemLabel(item, metadata.index || 0);
    const typeLabel = this.getItemTypeLabel();
    const parentLabel = metadata.parentLabel ? `<span class="text-sm text-slate-500">${esc(metadata.parentLabel)}</span>` : '';
    
    modal.innerHTML = `
      <div class="bg-white rounded-xl shadow-2xl w-full max-w-5xl max-h-[90vh] flex flex-col animate-slide-up">
        <!-- Header -->
        <div class="px-6 py-4 border-b border-slate-200 flex items-center justify-between bg-gradient-to-r from-indigo-50 to-purple-50">
          <div>
            <div class="flex items-center gap-3">
              <span class="px-2 py-1 bg-indigo-600 text-white rounded text-xs font-bold">${esc(itemLabel)}</span>
              <h3 class="text-lg font-semibold text-slate-800">${esc(typeLabel)}</h3>
              ${parentLabel}
            </div>
            <p class="text-sm text-slate-500 mt-1">Focus Mode • Auto-save enabled</p>
          </div>
          <div class="flex items-center gap-2">
            <button id="focusFullscreenBtn" class="p-2 text-slate-500 hover:bg-white rounded-lg transition-colors" title="Full screen (Ctrl+F)">
              <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 8V4m0 0h4M4 4l5 5m11-1V4m0 0h-4m4 0l-5 5M4 16v4m0 0h4m-4 0l5-5m11 5l-5-5m5 5v-4m0 4h-4"></path>
              </svg>
            </button>
            <button id="focusCloseBtn" class="p-2 text-slate-500 hover:bg-slate-100 rounded-lg transition-colors" title="Close (Esc)">
              <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path>
              </svg>
            </button>
          </div>
        </div>
        
        <!-- Content: Split View -->
        <div class="flex-1 overflow-hidden flex">
          <!-- Editor Side -->
          <div class="w-1/2 border-r border-slate-200 overflow-y-auto p-6 space-y-6">
            <div class="flex items-center gap-2 mb-4">
              <svg class="w-5 h-5 text-indigo-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z"></path>
              </svg>
              <h4 class="text-sm font-semibold text-slate-700 uppercase tracking-wide">Edit</h4>
            </div>
            
            ${this.renderFieldInputs(item, 'focus')}
          </div>
          
          <!-- Preview Side -->
          <div class="w-1/2 bg-slate-50 overflow-y-auto p-6">
            <div class="flex items-center justify-between mb-4">
              <div class="flex items-center gap-2">
                <svg class="w-5 h-5 text-indigo-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"></path>
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z"></path>
                </svg>
                <h4 class="text-sm font-semibold text-slate-700 uppercase tracking-wide">Live Preview</h4>
              </div>
              <div class="flex items-center gap-1">
                <button id="focusAIEnhanceBtn" class="p-2 text-slate-400 hover:text-purple-600 hover:bg-purple-50 rounded transition-colors" title="Enhance with AI">
                  <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z"></path>
                  </svg>
                </button>
                <button id="focusDeleteBtn" class="p-2 text-slate-400 hover:text-rose-600 hover:bg-rose-50 rounded transition-colors" title="Delete this item">
                  <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"></path>
                  </svg>
                </button>
              </div>
            </div>
            
            <div id="focus-preview" class="bg-white border border-slate-200 rounded-lg p-6 shadow-sm">
              <!-- Preview content will be injected here -->
            </div>
          </div>
        </div>
        
        <!-- Footer -->
        <div class="px-6 py-4 border-t border-slate-200 bg-slate-50 flex items-center justify-between">
          <div class="flex items-center gap-2 text-xs text-slate-500">
            <div id="autoSaveIndicator" class="flex items-center gap-1.5">
              <div class="w-2 h-2 rounded-full bg-emerald-500"></div>
              <span>All changes saved</span>
            </div>
          </div>
          <div class="flex items-center gap-2">
            <button id="focusCancelBtn" class="px-4 py-2 border border-slate-300 rounded-lg text-sm font-medium text-slate-700 hover:bg-white transition-colors">
              Cancel
            </button>
            <button id="focusSaveBtn" class="px-4 py-2 bg-indigo-600 text-white rounded-lg text-sm font-medium hover:bg-indigo-700 transition-colors">
              Done
            </button>
          </div>
        </div>
      </div>
    `;
    
    // Setup auto-expanding textareas
    const textareas = modal.querySelectorAll('textarea');
    textareas.forEach(ta => this.makeAutoExpanding(ta));
    
    // Setup live preview
    this.setupLivePreview(modal, item, 'focus');
    
    // Setup event handlers
    this.setupFocusModalHandlers(modal, item, metadata);
    
    return modal;
  }
  
  /**
   * Renders field inputs for a modal.
   * @protected
   */
  renderFieldInputs(item, prefix) {
    const fields = this.getItemFields(item);
    
    return fields.map(field => {
      const inputId = `${prefix}-${field.id}`;
      const value = field.value !== undefined ? field.value : '';
      const helpText = field.helpText ? `<p class="text-xs text-slate-500 mt-1">${esc(field.helpText)}</p>` : '';
      
      if (field.type === 'textarea') {
        // Increase default rows significantly for better UX
        const rows = field.rows || 10;
        return `
          <div>
            <label class="block text-sm font-semibold text-slate-700 mb-2">${esc(field.label)}</label>
            <textarea 
              id="${inputId}" 
              class="w-full border border-slate-300 rounded-lg px-4 py-3 text-base focus:ring-2 focus:ring-indigo-500 focus:border-transparent transition-all ${field.monospace ? 'font-mono text-sm' : ''} resize-y min-h-[200px]" 
              rows="${rows}"
              placeholder="${esc(field.placeholder || '')}"
            >${esc(value)}</textarea>
            ${helpText}
          </div>
        `;
      } else if (field.type === 'select') {
        const options = field.options.map(opt => 
          `<option value="${esc(opt.value)}" ${opt.value === value ? 'selected' : ''}>${esc(opt.label)}</option>`
        ).join('');
        return `
          <div>
            <label class="block text-sm font-semibold text-slate-700 mb-2">${esc(field.label)}</label>
            <select 
              id="${inputId}" 
              class="w-full border border-slate-300 rounded-lg px-4 py-3 text-base focus:ring-2 focus:ring-indigo-500 focus:border-transparent transition-all"
            >
              ${options}
            </select>
          </div>
        `;
      } else {
        return `
          <div>
            <label class="block text-sm font-semibold text-slate-700 mb-2">${esc(field.label)}</label>
            <input 
              type="text" 
              id="${inputId}" 
              class="w-full border border-slate-300 rounded-lg px-4 py-3 text-base focus:ring-2 focus:ring-indigo-500 focus:border-transparent transition-all" 
              value="${esc(value)}"
              placeholder="${esc(field.placeholder || '')}"
            />
          </div>
        `;
      }
    }).join('');
  }
  
  /**
   * Sets up event handlers for focus modal.
   * @protected
   */
  setupFocusModalHandlers(modal, item, metadata) {
    const closeBtn = modal.querySelector('#focusCloseBtn');
    const cancelBtn = modal.querySelector('#focusCancelBtn');
    const saveBtn = modal.querySelector('#focusSaveBtn');
    const fullscreenBtn = modal.querySelector('#focusFullscreenBtn');
    const aiEnhanceBtn = modal.querySelector('#focusAIEnhanceBtn');
    const deleteBtn = modal.querySelector('#focusDeleteBtn');
    
    const close = () => {
      document.removeEventListener('keydown', this.handleKeyboardShortcuts);
      modal.remove();
      this.currentMode = 'inline';
      this.editingItem = null;
    };
    
    const save = () => {
      this.saveFieldValues(modal, item, 'focus');
      this.onSave();
      close();
    };
    
    closeBtn.addEventListener('click', close);
    cancelBtn.addEventListener('click', close);
    saveBtn.addEventListener('click', save);
    fullscreenBtn.addEventListener('click', () => this.openFullscreenMode(item, metadata));
    
    // AI Enhance button handler
    if (aiEnhanceBtn) {
      aiEnhanceBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        // Save current values first
        this.saveFieldValues(modal, item, 'focus');
        this.onAIEnhance(metadata);
      });
    }
    
    // Delete button handler
    if (deleteBtn) {
      deleteBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        if (confirm('Are you sure you want to delete this item?')) {
          this.onDelete(metadata);
          close();
        }
      });
    }
    
    // Close on backdrop click
    modal.addEventListener('click', (e) => {
      if (e.target === modal) close();
    });
  }
  
  /**
   * Saves field values from modal inputs back to item.
   * @protected
   */
  saveFieldValues(modal, item, prefix) {
    const fields = this.getItemFields(item);
    
    fields.forEach(field => {
      const input = modal.querySelector(`#${prefix}-${field.id}`);
      if (!input) return;
      
      const value = input.value.trim();
      
      // Handle array fields (split by newlines)
      if (field.isArray) {
        item[field.id] = value ? value.split('\n').filter(l => l.trim()) : [];
      } else {
        item[field.id] = value;
      }
    });
  }
  
  /**
   * Sets up live preview updates.
   * @protected
   */
  setupLivePreview(modal, item, prefix) {
    const preview = modal.querySelector(`#${prefix}-preview`);
    const fields = this.getItemFields(item);
    
    const updatePreview = async () => {
      const fieldValues = {};
      fields.forEach(field => {
        const input = modal.querySelector(`#${prefix}-${field.id}`);
        if (input) {
          fieldValues[field.id] = input.value;
        }
      });
      
      preview.innerHTML = this.renderPreviewContent(fieldValues, item);
      await initializeMermaid(preview);
      
      this.markUnsaved();
      this.scheduleAutoSave();
    };
    
    fields.forEach(field => {
      const input = modal.querySelector(`#${prefix}-${field.id}`);
      if (input) {
        const eventType = input.tagName === 'SELECT' ? 'change' : 'input';
        input.addEventListener(eventType, updatePreview);
      }
    });
    
    updatePreview();
  }
  
  /**
   * Opens fullscreen mode for distraction-free editing.
   */
  openFullscreenMode(item, metadata = {}) {
    // Close focus modal first
    const focusModal = document.getElementById('focusEditorModal');
    if (focusModal) focusModal.remove();
    
    this.currentMode = 'fullscreen';
    
    const modal = document.createElement('div');
    modal.id = 'fullscreenEditorModal';
    modal.className = 'fixed inset-0 bg-white z-50 flex flex-col';
    
    const itemLabel = this.getItemLabel(item, metadata.index || 0);
    const typeLabel = this.getItemTypeLabel();
    const parentLabel = metadata.parentLabel ? `<span class="text-sm text-slate-500">${esc(metadata.parentLabel)}</span>` : '';
    
    modal.innerHTML = `
      <!-- Header -->
      <div class="px-8 py-4 border-b border-slate-200 flex items-center justify-between bg-gradient-to-r from-indigo-50 to-purple-50">
        <div>
          <div class="flex items-center gap-3">
            <span class="px-2 py-1 bg-indigo-600 text-white rounded text-xs font-bold">${esc(itemLabel)}</span>
            <h3 class="text-xl font-semibold text-slate-800">${esc(typeLabel)}</h3>
            ${parentLabel}
          </div>
          <p class="text-sm text-slate-500 mt-1">Fullscreen Mode • Auto-save enabled • Press Esc to exit</p>
        </div>
        <div class="flex items-center gap-2">
          <button id="fullscreenMinimizeBtn" class="p-2 text-slate-500 hover:bg-white rounded-lg transition-colors" title="Back to focus mode (Esc)">
            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12"></path>
            </svg>
          </button>
        </div>
      </div>
      
      <!-- Content: Full Width Split -->
      <div class="flex-1 overflow-hidden flex">
        <!-- Editor Side -->
        <div class="w-1/2 border-r border-slate-200 overflow-y-auto px-12 py-8 space-y-8">
          <div class="max-w-3xl mx-auto space-y-6">
            ${this.renderFieldInputs(item, 'fullscreen')}
          </div>
        </div>
        
        <!-- Preview Side -->
        <div class="w-1/2 bg-slate-50 overflow-y-auto px-12 py-8">
          <div class="max-w-3xl mx-auto">
            <div class="flex items-center justify-between mb-6">
              <div class="flex items-center gap-2">
                <svg class="w-6 h-6 text-indigo-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"></path>
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M2.458 12C3.732 7.943 7.523 5 12 5c4.478 0 8.268 2.943 9.542 7-1.274 4.057-5.064 7-9.542 7-4.477 0-8.268-2.943-9.542-7z"></path>
                </svg>
                <h4 class="text-base font-semibold text-slate-700 uppercase tracking-wide">Live Preview</h4>
              </div>
              <div class="flex items-center gap-1">
                <button id="fullscreenAIEnhanceBtn" class="p-2 text-slate-400 hover:text-purple-600 hover:bg-purple-50 rounded transition-colors" title="Enhance with AI">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z"></path>
                  </svg>
                </button>
                <button id="fullscreenDeleteBtn" class="p-2 text-slate-400 hover:text-rose-600 hover:bg-rose-50 rounded transition-colors" title="Delete this item">
                  <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"></path>
                  </svg>
                </button>
              </div>
            </div>
            
            <div id="fullscreen-preview" class="bg-white border border-slate-200 rounded-xl p-8 shadow-lg">
              <!-- Preview content will be injected here -->
            </div>
          </div>
        </div>
      </div>
      
      <!-- Footer -->
      <div class="px-8 py-4 border-t border-slate-200 bg-slate-50 flex items-center justify-between">
        <div class="flex items-center gap-4 text-sm text-slate-500">
          <div id="fullscreenAutoSaveIndicator" class="flex items-center gap-2">
            <div class="w-2 h-2 rounded-full bg-emerald-500"></div>
            <span>All changes saved</span>
          </div>
          <div class="text-xs text-slate-400">
            Tip: Press <kbd class="px-2 py-1 bg-slate-200 rounded text-xs">Esc</kbd> to exit fullscreen
          </div>
        </div>
        <div class="flex items-center gap-2">
          <button id="fullscreenDoneBtn" class="px-6 py-2.5 bg-indigo-600 text-white rounded-lg text-base font-medium hover:bg-indigo-700 transition-colors">
            Done
          </button>
        </div>
      </div>
    `;
    
    document.body.appendChild(modal);
    
    // Setup auto-expanding textareas
    const textareas = modal.querySelectorAll('textarea');
    textareas.forEach(ta => this.makeAutoExpanding(ta));
    
    // Setup live preview
    this.setupLivePreview(modal, item, 'fullscreen');
    
    // Setup handlers
    this.setupFullscreenModalHandlers(modal, item, metadata);
    
    // Focus first input
    setTimeout(() => {
      const firstInput = modal.querySelector('input, textarea');
      if (firstInput) firstInput.focus();
    }, 100);
  }
  
  /**
   * Sets up event handlers for fullscreen modal.
   * @protected
   */
  setupFullscreenModalHandlers(modal, item, metadata) {
    const minimizeBtn = modal.querySelector('#fullscreenMinimizeBtn');
    const doneBtn = modal.querySelector('#fullscreenDoneBtn');
    const aiEnhanceBtn = modal.querySelector('#fullscreenAIEnhanceBtn');
    const deleteBtn = modal.querySelector('#fullscreenDeleteBtn');
    
    const close = () => {
      document.removeEventListener('keydown', this.handleKeyboardShortcuts);
      modal.remove();
      // Return to focus mode
      this.openFocusMode(item, metadata);
    };
    
    const save = () => {
      this.saveFieldValues(modal, item, 'fullscreen');
      this.onSave();
      document.removeEventListener('keydown', this.handleKeyboardShortcuts);
      modal.remove();
      this.currentMode = 'inline';
      this.editingItem = null;
    };
    
    minimizeBtn.addEventListener('click', close);
    doneBtn.addEventListener('click', save);
    
    // AI Enhance button handler
    if (aiEnhanceBtn) {
      aiEnhanceBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        // Save current values first
        this.saveFieldValues(modal, item, 'fullscreen');
        this.onAIEnhance(metadata);
      });
    }
    
    // Delete button handler
    if (deleteBtn) {
      deleteBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        if (confirm('Are you sure you want to delete this item?')) {
          this.onDelete(metadata);
          document.removeEventListener('keydown', this.handleKeyboardShortcuts);
          modal.remove();
          this.currentMode = 'inline';
          this.editingItem = null;
        }
      });
    }
  }
  
  /**
   * Handles keyboard shortcuts.
   * @protected
   */
  handleKeyboardShortcuts(e) {
    // Escape - close current modal
    if (e.key === 'Escape') {
      if (this.currentMode === 'fullscreen') {
        const modal = document.getElementById('fullscreenEditorModal');
        if (modal) modal.querySelector('#fullscreenMinimizeBtn').click();
      } else if (this.currentMode === 'focus') {
        const modal = document.getElementById('focusEditorModal');
        if (modal) modal.querySelector('#focusCloseBtn').click();
      }
    }
    
    // Ctrl/Cmd + S - save (prevent default browser save)
    if ((e.ctrlKey || e.metaKey) && e.key === 's') {
      e.preventDefault();
      if (this.currentMode === 'fullscreen') {
        const modal = document.getElementById('fullscreenEditorModal');
        if (modal) modal.querySelector('#fullscreenDoneBtn').click();
      } else if (this.currentMode === 'focus') {
        const modal = document.getElementById('focusEditorModal');
        if (modal) modal.querySelector('#focusSaveBtn').click();
      }
    }
    
    // Ctrl/Cmd + F - toggle fullscreen
    if ((e.ctrlKey || e.metaKey) && e.key === 'f' && this.currentMode === 'focus') {
      e.preventDefault();
      const modal = document.getElementById('focusEditorModal');
      if (modal) modal.querySelector('#focusFullscreenBtn').click();
    }
  }
  
  /**
   * Creates auto-expanding textarea.
   * @protected
   */
  createAutoExpandingTextarea(value = '') {
    const textarea = document.createElement('textarea');
    textarea.value = value;
    textarea.rows = 3;
    this.makeAutoExpanding(textarea);
    return textarea;
  }
  
  /**
   * Makes textarea auto-expand based on content.
   * @protected
   */
  makeAutoExpanding(textarea) {
    if (!textarea) return;
    
    const adjust = () => {
      textarea.style.height = 'auto';
      textarea.style.height = Math.max(textarea.scrollHeight, 80) + 'px';
    };
    
    textarea.addEventListener('input', adjust);
    adjust();
  }
  
  /**
   * Creates simple input.
   * @protected
   */
  createInput(value = '') {
    const input = document.createElement('input');
    input.type = 'text';
    input.value = value;
    return input;
  }
  
  /**
   * Marks bundle as having unsaved changes.
   */
  markUnsaved() {
    this.unsavedChanges = true;
    
    // Update indicators
    const indicators = document.querySelectorAll('#autoSaveIndicator, #fullscreenAutoSaveIndicator');
    indicators.forEach(indicator => {
      if (indicator) {
        indicator.innerHTML = `
          <div class="w-2 h-2 rounded-full bg-amber-500 animate-pulse"></div>
          <span>Saving...</span>
        `;
      }
    });
  }
  
  /**
   * Marks bundle as saved.
   */
  markSaved() {
    this.unsavedChanges = false;
    
    const indicators = document.querySelectorAll('#autoSaveIndicator, #fullscreenAutoSaveIndicator');
    indicators.forEach(indicator => {
      if (indicator) {
        indicator.innerHTML = `
          <div class="w-2 h-2 rounded-full bg-emerald-500"></div>
          <span>All changes saved</span>
        `;
      }
    });
  }
  
  /**
   * Schedules auto-save after typing stops.
   */
  scheduleAutoSave() {
    clearTimeout(this.autoSaveTimeout);
    this.autoSaveTimeout = setTimeout(() => {
      this.handleAutoSave();
    }, 1500); // Save after 1.5 seconds of inactivity
  }
  
  /**
   * Handles auto-save.
   */
  handleAutoSave() {
    if (this.unsavedChanges) {
      this.onSave();
      this.markSaved();
    }
  }
}

