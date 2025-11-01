/**
 * Project Modals Component
 * 
 * Manages project create/edit and delete confirmation modals.
 * Integrates with GitLabBacklogProjectsManager for backlog project management.
 * 
 * @module components/project-modals
 */

'use strict';

import { GitLabBacklogProjectsManager } from '../helpers/gitlab-projects-manager.js';

/**
 * Component for managing project modals.
 */
export class ProjectModals {
  /**
   * @param {Object} callbacks - Action callbacks
   * @param {Function} callbacks.onSave - Save project callback(payload, isEdit)
   * @param {Function} callbacks.onDelete - Delete project callback(project)
   */
  constructor(callbacks) {
    this.callbacks = callbacks;
    this.editTargetId = null;
    this.gitlabBacklogProjectsManager = new GitLabBacklogProjectsManager();
    
    // Cache modal elements
    this.projectModal = document.getElementById('projectModal');
    this.projectModalTitle = document.getElementById('projectModalTitle');
    this.projectForm = document.getElementById('projectForm');
    this.deleteModal = document.getElementById('deleteModal');
    this.deleteProjectName = document.getElementById('deleteProjectName');
    
    this._setupEventHandlers();
  }
  
  /**
   * Sets up event handlers for modals.
   * @private
   */
  _setupEventHandlers() {
    // Project modal close handlers
    const projectClose = () => this.closeProjectModal();
    const btnClose = document.getElementById('projectModalClose');
    const btnCancel = document.getElementById('projectFormCancel');
    if (btnClose) btnClose.addEventListener('click', projectClose);
    if (btnCancel) btnCancel.addEventListener('click', projectClose);
    
    // Project form submit
    if (this.projectForm) {
      this.projectForm.addEventListener('submit', (e) => this._handleProjectSubmit(e));
    }
    
    // Project modal keyboard
    if (this.projectModal) {
      this.projectModal.addEventListener('keydown', (e) => { 
        if (e.key === 'Escape') projectClose(); 
      });
    }
    
    // Delete modal close handlers
    const deleteClose = () => this.closeDeleteModal();
    const dmClose = document.getElementById('deleteModalClose');
    const dmCancel = document.getElementById('deleteCancelBtn');
    const dmConfirm = document.getElementById('deleteConfirmBtn');
    if (dmClose) dmClose.addEventListener('click', deleteClose);
    if (dmCancel) dmCancel.addEventListener('click', deleteClose);
    if (this.deleteModal) {
      this.deleteModal.addEventListener('keydown', (e) => { 
        if (e.key === 'Escape') deleteClose(); 
      });
    }
    if (dmConfirm) {
      dmConfirm.addEventListener('click', async () => {
        await this._handleDeleteConfirm();
        deleteClose();
      });
    }
  }
  
  /**
   * Opens the project modal for create or edit.
   * @param {Object|null} project - Project object for edit, null for create
   */
  openProjectModal(project = null) {
    this.editTargetId = project ? project.id : null;
    
    // Set modal title
    if (this.projectModalTitle) {
      this.projectModalTitle.textContent = project ? 'Edit Project' : 'Create Project';
    }
    
    // Populate form fields
    const nameInput = document.getElementById('f_name');
    const descInput = document.getElementById('f_description');
    const repoUrlInput = document.getElementById('f_gitlab_repository_url');
    
    if (nameInput) nameInput.value = project?.name || '';
    if (descInput) descInput.value = project?.description || '';
    if (repoUrlInput) repoUrlInput.value = project?.gitlab_repository_url || '';
    
    // Initialize GitLab Backlog Projects Manager
    this.gitlabBacklogProjectsManager.init();
    
    // Set backlog project URLs
    if (project && project.gitlab_backlog_project_urls) {
      this.gitlabBacklogProjectsManager.setUrls(project.gitlab_backlog_project_urls);
    } else {
      this.gitlabBacklogProjectsManager.clear();
    }
    
    // Show modal
    if (this.projectModal) {
      this.projectModal.classList.remove('hidden');
      this.projectModal.classList.add('flex');
    }
  }
  
  /**
   * Closes the project modal.
   */
  closeProjectModal() {
    if (this.projectModal) {
      this.projectModal.classList.add('hidden');
      this.projectModal.classList.remove('flex');
    }
    this.editTargetId = null;
  }
  
  /**
   * Opens the delete confirmation modal.
   * @param {Object} project - Project to delete
   */
  openDeleteModal(project) {
    if (this.deleteProjectName) {
      this.deleteProjectName.textContent = project?.name || '(untitled)';
    }
    
    // Store project for deletion
    this.projectToDelete = project;
    
    // Show modal
    if (this.deleteModal) {
      this.deleteModal.classList.remove('hidden');
      this.deleteModal.classList.add('flex');
    }
  }
  
  /**
   * Closes the delete confirmation modal.
   */
  closeDeleteModal() {
    if (this.deleteModal) {
      this.deleteModal.classList.add('hidden');
      this.deleteModal.classList.remove('flex');
    }
    this.projectToDelete = null;
  }
  
  /**
   * Handles project form submission.
   * @private
   */
  async _handleProjectSubmit(e) {
    e.preventDefault();
    
    const nameInput = document.getElementById('f_name');
    const descInput = document.getElementById('f_description');
    const repoUrlInput = document.getElementById('f_gitlab_repository_url');
    
    const name = nameInput?.value.trim();
    if (!name) { 
      alert('Name is required'); 
      return; 
    }
    
    const payload = {
      name,
      description: this._valOrNull(descInput?.value),
      gitlab_repository_url: this._valOrNull(repoUrlInput?.value),
      gitlab_backlog_project_urls: null
    };
    
    // Get backlog project URLs from manager
    const gitlabBacklogUrls = this.gitlabBacklogProjectsManager.getUrls();
    if (gitlabBacklogUrls.length > 0) {
      payload.gitlab_backlog_project_urls = gitlabBacklogUrls;
    }
    
    try {
      const isEdit = !!this.editTargetId;
      await this.callbacks.onSave(payload, isEdit, this.editTargetId);
      this.closeProjectModal();
    } catch (error) {
      console.error('Project save failed:', error);
      alert('Save failed: ' + (error.message || 'Unknown error'));
    }
  }
  
  /**
   * Handles delete confirmation.
   * @private
   */
  async _handleDeleteConfirm() {
    if (!this.projectToDelete) return;
    
    try {
      await this.callbacks.onDelete(this.projectToDelete);
    } catch (error) {
      console.error('Delete failed:', error);
      alert('Delete failed: ' + (error.message || 'Unknown error'));
    }
  }
  
  /**
   * Converts value to null if empty string.
   * @private
   */
  _valOrNull(v) {
    const s = (v || '').trim();
    return s === '' ? null : s;
  }
}

