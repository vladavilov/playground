/**
 * Project List Component
 * 
 * Renders and manages the project list sidebar.
 * Handles filtering, selection, and progress updates.
 * 
 * @module components/project-list
 */

'use strict';

import { el } from '../utils/dom-helpers.js';
import { getStatusBadge } from '../utils/status-badges.js';

/**
 * Component for rendering and managing project list.
 */
export class ProjectList {
  /**
   * @param {HTMLElement} container - List container element
   * @param {Function} onProjectSelect - Callback when project is selected
   */
  constructor(container, onProjectSelect) {
    this.container = container;
    this.onProjectSelect = onProjectSelect;
    this.projects = [];
    this.selectedProject = null;
    this.filterText = '';
  }
  
  /**
   * Sets the projects data and renders the list.
   * @param {Array} projects - Array of project objects
   * @param {Object} [selectedProject=null] - Currently selected project
   * @param {string} [filterText=''] - Filter text
   */
  render(projects, selectedProject = null, filterText = '') {
    this.projects = projects || [];
    this.selectedProject = selectedProject;
    this.filterText = filterText;
    
    this._renderList();
  }
  
  /**
   * Renders the project list.
   * @private
   */
  _renderList() {
    this.container.innerHTML = '';
    
    const q = (this.filterText || '').toLowerCase();
    const filtered = this.projects.filter(p => (p.name || '').toLowerCase().includes(q));
    
    if (filtered.length === 0) {
      this.container.appendChild(this._createEmptyState(q));
      return;
    }
    
    filtered.forEach(p => {
      this.container.appendChild(this._createProjectItem(p));
    });
  }
  
  /**
   * Creates empty state element.
   * @private
   */
  _createEmptyState(filterText) {
    const message = filterText 
      ? 'No projects match your search' 
      : 'No projects yet. Click "Create" to get started.';
    
    return el('div', { 
      class: 'text-center text-sm text-slate-500 py-8' 
    }, message);
  }
  
  /**
   * Creates a project list item.
   * @private
   */
  _createProjectItem(project) {
    const isSelected = this.selectedProject && this.selectedProject.id === project.id;
    
    const item = el('li', { 
      class: 'border rounded p-3 cursor-pointer transition-colors ' + 
             (isSelected 
               ? 'bg-indigo-50 border-indigo-300 ring-1 ring-indigo-300' 
               : 'hover:bg-slate-50 border-slate-200'),
      onclick: () => this.onProjectSelect(project), 
      role: 'button', 
      tabindex: '0',
      onkeypress: (e) => { 
        if (e.key === 'Enter' || e.key === ' ') { 
          e.preventDefault(); 
          this.onProjectSelect(project); 
        } 
      }
    }, [
      el('div', { class: 'flex items-start justify-between gap-2' }, [
        el('div', { class: 'flex-1 min-w-0' }, [
          el('div', { 
            class: 'font-semibold truncate ' + (isSelected ? 'text-indigo-900' : 'text-slate-900') 
          }, project.name || '(untitled)'),
          project.description 
            ? el('div', { 
                class: 'text-sm mt-1 truncate ' + (isSelected ? 'text-indigo-700' : 'text-slate-600') 
              }, project.description) 
            : null,
        ]),
        el('div', { class: 'mt-0.5 flex-shrink-0' }, [getStatusBadge(project.status)])
      ])
    ]);
    
    return item;
  }
  
  /**
   * Updates a single project in the list (for progress updates).
   * @param {string} projectId - Project ID
   * @param {Object} updates - Properties to update
   */
  updateProject(projectId, updates) {
    const project = this.projects.find(p => String(p.id) === String(projectId));
    if (project) {
      Object.assign(project, updates);
      this._renderList();
    }
  }
  
  /**
   * Gets filtered projects.
   * @returns {Array} Filtered project list
   */
  getFilteredProjects() {
    const q = (this.filterText || '').toLowerCase();
    return this.projects.filter(p => (p.name || '').toLowerCase().includes(q));
  }
}

