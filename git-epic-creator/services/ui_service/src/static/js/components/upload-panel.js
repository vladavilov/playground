/**
 * Upload Panel Component
 * 
 * Renders file upload interface with drag-and-drop,
 * progress tracking, and processing log.
 * 
 * @module components/upload-panel
 */

'use strict';

import { el } from '../utils/dom-helpers.js';
import { getStatusBadge } from '../utils/status-badges.js';

/**
 * Component for managing file upload UI.
 */
export class UploadPanel {
  /**
   * @param {HTMLElement} container - Upload panel container
   * @param {Function} onUpload - Upload callback(files)
   */
  constructor(container, onUpload) {
    this.container = container;
    this.onUpload = onUpload;
    this.project = null;
    
    // DOM elements (will be set after render)
    this.dropzone = null;
    this.fileInput = null;
    this.uploadBtn = null;
    this.progressBar = null;
    this.progressPct = null;
    this.progressText = null;
    this.statusBadge = null;
    this.progressLog = null;
    this.fileCount = null;
    this.selectedFiles = null;
  }
  
  /**
   * Renders the upload panel.
   * @param {Object|null} project - Current project or null to hide
   */
  render(project) {
    this.project = project;
    
    if (!project) {
      this.container.classList.add('hidden');
      return;
    }
    
    this.container.classList.remove('hidden');
    this.container.innerHTML = '';
    
    this.container.appendChild(this._createUploadUI());
    
    // Cache DOM elements
    this.dropzone = document.getElementById('dropzone');
    this.fileInput = document.getElementById('fileInput');
    this.uploadBtn = document.getElementById('uploadBtn');
    this.progressBar = document.getElementById('progressBar');
    this.progressPct = document.getElementById('progressPct');
    this.progressText = document.getElementById('progressText');
    this.statusBadge = document.getElementById('statusBadge');
    this.progressLog = document.getElementById('progressLog');
    this.fileCount = document.getElementById('fileCount');
    this.selectedFiles = document.getElementById('selectedFiles');
    
    // Setup event handlers
    this._setupEventHandlers();
  }
  
  /**
   * Creates the upload UI structure.
   * @private
   */
  _createUploadUI() {
    return el('div', { class: 'h-full flex flex-col min-h-0' }, [
      // Header
      el('div', { class: 'flex items-center justify-between' }, [
        el('h3', { class: 'font-semibold' }, 'Upload documents'),
        el('span', { id: 'fileCount', class: 'text-xs text-slate-500' }, 'No files selected')
      ]),
      
      // Dropzone
      el('div', { 
        id: 'dropzone',
        class: 'mt-3 border-2 border-dashed border-slate-300 rounded-lg p-6 text-center cursor-pointer transition-all hover:border-indigo-400 hover:bg-indigo-50/30' 
      }, [
        el('div', { class: 'flex flex-col items-center gap-2' }, [
          el('div', { class: 'h-10 w-10 rounded-full bg-slate-100 flex items-center justify-center text-slate-600' }, [
            el('svg', { 
              class: 'w-6 h-6', 
              fill: 'none', 
              stroke: 'currentColor', 
              viewBox: '0 0 24 24',
              innerHTML: '<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M15 13l-3-3m0 0l-3 3m3-3v12"></path>'
            })
          ]),
          el('p', { class: 'text-sm text-slate-700' }, [
            el('span', { class: 'font-medium' }, 'Drag & drop'),
            ' files here or click to browse'
          ]),
          el('p', { class: 'text-xs text-slate-500' }, 'PDF, DOCX, TXT and more')
        ]),
        el('input', { 
          id: 'fileInput', 
          type: 'file', 
          multiple: true, 
          class: 'sr-only', 
          'aria-label': 'Select files to upload' 
        })
      ]),
      
      // Selected files display
      el('div', { id: 'selectedFiles', class: 'mt-3 text-sm text-slate-700 hidden' }),
      
      // Upload button
      el('div', { class: 'mt-4 flex items-center gap-2' }, [
        el('button', { 
          id: 'uploadBtn',
          class: 'inline-flex items-center justify-center gap-2 w-32 px-3 py-2 bg-emerald-600 text-white rounded hover:bg-emerald-700 transition-colors disabled:opacity-50 disabled:cursor-not-allowed',
          disabled: true 
        }, [
          el('svg', { 
            class: 'w-4 h-4', 
            fill: 'none', 
            stroke: 'currentColor', 
            viewBox: '0 0 24 24',
            innerHTML: '<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-8l-4-4m0 0L8 8m4-4v12"></path>'
          }),
          'Upload'
        ])
      ]),
      
      // Progress bar
      el('div', { class: 'mt-4' }, [
        el('div', { class: 'relative h-3 bg-slate-200 rounded overflow-hidden' }, [
          el('div', { id: 'progressBar', class: 'absolute left-0 top-0 h-full bg-emerald-500 w-0 transition-all' }),
          el('div', { id: 'progressPct', class: 'absolute inset-0 flex items-center justify-center text-[10px] font-medium text-slate-700' }, '0%')
        ]),
        el('div', { class: 'flex items-center gap-2 mt-2' }, [
          el('span', { id: 'statusBadge' }),
          el('div', { id: 'progressText', class: 'text-sm text-slate-600' }, 'No processing yet')
        ])
      ]),
      
      // Processing log
      el('div', { class: 'mt-4 flex flex-col min-h-0 flex-1' }, [
        el('div', { class: 'flex items-center justify-between' }, [
          el('h4', { class: 'text-sm font-medium text-slate-700' }, 'Processing log')
        ]),
        el('div', { 
          id: 'progressLog', 
          class: 'mt-2 flex-1 overflow-y-auto bg-slate-50 border rounded p-2 text-xs font-mono text-slate-700',
          'aria-live': 'polite',
          'aria-atomic': 'false'
        })
      ])
    ]);
  }
  
  /**
   * Sets up event handlers for upload interactions.
   * @private
   */
  _setupEventHandlers() {
    // File input change
    if (this.fileInput) {
      this.fileInput.addEventListener('change', () => this._updateSelectedFiles());
    }
    
    // Dropzone interactions
    if (this.dropzone && this.fileInput) {
      this.dropzone.addEventListener('click', () => this.fileInput.click());
      
      this.dropzone.addEventListener('dragover', (e) => { 
        e.preventDefault(); 
        this.dropzone.classList.add('border-indigo-500', 'bg-indigo-50'); 
      });
      
      this.dropzone.addEventListener('dragleave', () => { 
        this.dropzone.classList.remove('border-indigo-500', 'bg-indigo-50'); 
      });
      
      this.dropzone.addEventListener('drop', (e) => {
        e.preventDefault();
        this.dropzone.classList.remove('border-indigo-500', 'bg-indigo-50');
        if (e.dataTransfer && e.dataTransfer.files && e.dataTransfer.files.length) {
          this.fileInput.files = e.dataTransfer.files;
          this._updateSelectedFiles();
        }
      });
    }
    
    // Upload button
    if (this.uploadBtn) {
      this.uploadBtn.addEventListener('click', () => this._handleUpload());
    }
  }
  
  /**
   * Updates the selected files display.
   * @private
   */
  _updateSelectedFiles() {
    if (!this.fileInput || !this.fileCount || !this.selectedFiles) return;
    
    const files = Array.from(this.fileInput.files || []);
    
    if (files.length === 0) {
      this.fileCount.textContent = 'No files selected';
      this.selectedFiles.classList.add('hidden');
      this.selectedFiles.textContent = '';
      if (this.uploadBtn) this.uploadBtn.disabled = true;
      return;
    }
    
    this.fileCount.textContent = `${files.length} file${files.length > 1 ? 's' : ''} selected`;
    this.selectedFiles.classList.remove('hidden');
    this.selectedFiles.textContent = files.map(f => `• ${f.name}`).join(', ');
    if (this.uploadBtn) this.uploadBtn.disabled = false;
  }
  
  /**
   * Handles upload button click.
   * @private
   */
  async _handleUpload() {
    if (!this.fileInput || !this.fileInput.files || this.fileInput.files.length === 0) return;
    
    const files = this.fileInput.files;
    
    // Reset progress UI
    this.resetProgress();
    this.updateProgress('Uploading files…', 0, null);
    
    // Call upload callback
    await this.onUpload(files);
    
    // Reset file input after successful upload
    if (this.fileInput) {
      this.fileInput.value = '';
      this._updateSelectedFiles();
    }
  }
  
  /**
   * Resets progress indicators.
   */
  resetProgress() {
    if (this.progressText) this.progressText.textContent = 'No processing yet';
    if (this.progressBar) this.progressBar.style.width = '0%';
    if (this.progressPct) this.progressPct.textContent = '0%';
    if (this.statusBadge) this.statusBadge.innerHTML = '';
    if (this.progressLog) this.progressLog.innerHTML = '';
  }
  
  /**
   * Updates progress display.
   * @param {string} text - Progress text
   * @param {number|null} percent - Progress percentage (0-100)
   * @param {string|null} status - Status for badge
   */
  updateProgress(text, percent, status) {
    if (this.progressText && text) {
      this.progressText.textContent = text;
    }
    
    if (this.progressBar && percent != null) {
      const safePct = Math.max(0, Math.min(100, Math.round(percent)));
      this.progressBar.style.width = `${safePct}%`;
      if (this.progressPct) {
        this.progressPct.textContent = `${safePct}%`;
      }
    }
    
    if (this.statusBadge && status) {
      this.statusBadge.innerHTML = '';
      this.statusBadge.appendChild(getStatusBadge(status));
    }
  }
  
  /**
   * Appends a line to the processing log.
   * @param {string} text - Log text
   * @param {string} [className=''] - Optional CSS class for styling
   */
  appendLog(text, className = '') {
    if (!this.progressLog) return;
    
    const ts = new Date().toLocaleTimeString();
    const line = document.createElement('div');
    line.textContent = `${ts} | ${text}`;
    if (className) line.className = className;
    
    this.progressLog.appendChild(line);
    
    // Limit log size
    while (this.progressLog.children.length > 200) {
      this.progressLog.removeChild(this.progressLog.firstChild);
    }
    
    this.progressLog.scrollTop = this.progressLog.scrollHeight;
  }
}

