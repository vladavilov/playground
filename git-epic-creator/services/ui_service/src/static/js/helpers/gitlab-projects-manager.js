/**
 * GitLab Backlog Projects Manager
 * 
 * Manages multiple GitLab project URLs for backlog (issues/epics) in the project form modal.
 * Handles tag rendering, addition, removal, and validation.
 */

'use strict';

export class GitLabBacklogProjectsManager {
  constructor() {
    this.urls = [];  // Array of GitLab backlog project URLs
    this.tagsContainer = null;
    this.inputElement = null;
    this.addButton = null;
    this.feedbackElement = null;
  }
  
  /**
   * Initialize the manager with DOM elements for backlog projects.
   */
  init() {
    this.tagsContainer = document.getElementById('gitlabBacklogProjectTags');
    this.inputElement = document.getElementById('newGitlabBacklogProjectUrl');
    this.addButton = document.getElementById('addGitlabBacklogProject');
    this.feedbackElement = document.getElementById('gitlabBacklogUrlFeedback');
    
    if (!this.tagsContainer || !this.inputElement || !this.addButton) {
      console.error('GitLab backlog projects manager: Required DOM elements not found');
      return;
    }
    
    // Set up event listeners
    this.addButton.addEventListener('click', () => this.addUrl());
    this.inputElement.addEventListener('keypress', (e) => {
      if (e.key === 'Enter') {
        e.preventDefault();
        this.addUrl();
      }
    });
    
    // Real-time URL validation
    this.inputElement.addEventListener('input', () => this.validateInput());
  }
  
  /**
   * Set URLs (for editing existing project).
   */
  setUrls(urls) {
    this.urls = Array.isArray(urls) ? [...urls] : [];
    this.render();
  }
  
  /**
   * Get current URLs.
   */
  getUrls() {
    return [...this.urls];
  }
  
  /**
   * Add a new URL.
   */
  addUrl() {
    const url = this.inputElement.value.trim();
    
    if (!url) {
      this.showFeedback('Please enter a GitLab project URL', 'error');
      return;
    }
    
    // Basic validation
    if (!this.isValidGitLabUrl(url)) {
      this.showFeedback('Invalid GitLab URL format', 'error');
      return;
    }
    
    // Check for duplicates
    if (this.urls.includes(url)) {
      this.showFeedback('This project is already added', 'warning');
      return;
    }
    
    // Add URL
    this.urls.push(url);
    this.inputElement.value = '';
    this.showFeedback('');
    this.render();
  }
  
  /**
   * Remove a URL by index.
   */
  removeUrl(index) {
    if (index >= 0 && index < this.urls.length) {
      this.urls.splice(index, 1);
      this.render();
    }
  }
  
  /**
   * Validate input field in real-time.
   */
  validateInput() {
    const url = this.inputElement.value.trim();
    
    if (!url) {
      this.showFeedback('');
      return;
    }
    
    if (!this.isValidGitLabUrl(url)) {
      this.showFeedback('⚠️ Invalid URL. Expected: https://host.name/team/project', 'warning');
    } else if (this.urls.includes(url)) {
      this.showFeedback('ℹ️ This project is already added', 'info');
    } else {
      this.showFeedback('✓ Valid URL format', 'success');
    }
  }
  
  /**
   * Validate GitLab URL format.
   * Supports custom GitLab instances with nested group structures.
   * Example: https://host.name/team_1/sub_team_1/sub_team_n/project_name
   */
  isValidGitLabUrl(url) {
    try {
      const parsed = new URL(url);
      
      // Must be HTTPS or HTTP
      if (parsed.protocol !== 'https:' && parsed.protocol !== 'http:') {
        return false;
      }
      
      // Must have hostname
      if (!parsed.hostname) {
        return false;
      }
      
      // Must have at least 2 path segments (e.g., /group/project or /team/subteam/project)
      const pathSegments = parsed.pathname.split('/').filter(p => p);
      return pathSegments.length >= 2;
    } catch {
      return false;
    }
  }
  
  /**
   * Extract display path from URL.
   */
  extractPath(url) {
    try {
      const parsed = new URL(url);
      // Remove leading slash and .git suffix
      let path = parsed.pathname.replace(/^\//, '').replace(/\.git$/, '');
      return path;
    } catch {
      // Assume it's already a path
      return url.replace(/\.git$/, '');
    }
  }
  
  /**
   * Show feedback message.
   */
  showFeedback(message, type = 'info') {
    if (!this.feedbackElement) return;
    
    if (!message) {
      this.feedbackElement.classList.add('hidden');
      return;
    }
    
    this.feedbackElement.classList.remove('hidden');
    this.feedbackElement.textContent = message;
    
    // Update styling based on type
    this.feedbackElement.className = 'text-xs mt-1';
    if (type === 'error') {
      this.feedbackElement.classList.add('text-rose-600');
    } else if (type === 'warning') {
      this.feedbackElement.classList.add('text-amber-600');
    } else if (type === 'success') {
      this.feedbackElement.classList.add('text-emerald-600');
    } else {
      this.feedbackElement.classList.add('text-slate-500');
    }
  }
  
  /**
   * Render tags.
   */
  render() {
    if (!this.tagsContainer) return;
    
    this.tagsContainer.innerHTML = '';
    
    if (this.urls.length === 0) {
      const placeholder = document.createElement('div');
      placeholder.className = 'text-sm text-slate-400 italic';
      placeholder.textContent = 'No GitLab projects added yet';
      this.tagsContainer.appendChild(placeholder);
      return;
    }
    
    this.urls.forEach((url, index) => {
      const tag = this.createTag(url, index);
      this.tagsContainer.appendChild(tag);
    });
  }
  
  /**
   * Create a tag element.
   */
  createTag(url, index) {
    const tag = document.createElement('div');
    tag.className = 'gitlab-project-tag flex items-center gap-2 px-3 py-1.5 bg-white border border-slate-200 rounded-lg shadow-sm';
    
    // GitLab icon (simplified)
    const icon = document.createElement('div');
    icon.className = 'w-4 h-4 rounded bg-orange-500 flex items-center justify-center text-white text-xs font-bold';
    icon.textContent = 'G';
    
    // Project path
    const pathText = document.createElement('span');
    pathText.className = 'text-sm font-medium text-slate-700';
    pathText.textContent = this.extractPath(url);
    
    // Remove button
    const removeBtn = document.createElement('button');
    removeBtn.type = 'button';
    removeBtn.className = 'ml-1 text-slate-400 hover:text-rose-600 transition-colors';
    removeBtn.innerHTML = '✕';
    removeBtn.title = 'Remove this project';
    removeBtn.addEventListener('click', () => this.removeUrl(index));
    
    tag.appendChild(icon);
    tag.appendChild(pathText);
    tag.appendChild(removeBtn);
    
    return tag;
  }
  
  /**
   * Clear all URLs.
   */
  clear() {
    this.urls = [];
    this.inputElement.value = '';
    this.showFeedback('');
    this.render();
  }
}

