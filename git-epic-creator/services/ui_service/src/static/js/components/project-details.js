/**
 * Project Details Component
 * 
 * Renders project details panel including GitLab information,
 * action buttons, and empty states.
 * 
 * @module components/project-details
 */

'use strict';

import { el } from '../utils/dom-helpers.js';
import { getStatusBadge } from '../utils/status-badges.js';
import { formatDate } from '../utils/formatting.js';

/**
 * Component for rendering project details panel.
 */
export class ProjectDetails {
  /**
   * @param {HTMLElement} container - Details container element
   * @param {Object} callbacks - Action callbacks
   * @param {Function} callbacks.onEdit - Edit project callback
   * @param {Function} callbacks.onDelete - Delete project callback
   * @param {Function} callbacks.onOpenRequirements - Open requirements callback
   * @param {Function} callbacks.onOpenTasks - Open tasks callback
   * @param {Function} callbacks.onCacheEmbeddings - Cache embeddings callback
   */
  constructor(container, callbacks) {
    this.container = container;
    this.callbacks = callbacks;
    this.project = null;
    this.cachingEmbeddings = false;
  }
  
  /**
   * Renders the project details or empty state.
   * @param {Object|null} project - Project object or null for empty state
   * @param {boolean} [cachingEmbeddings=false] - Whether embeddings are being cached
   */
  render(project, cachingEmbeddings = false) {
    this.project = project;
    this.cachingEmbeddings = cachingEmbeddings;
    
    this.container.innerHTML = '';
    this.container.classList.remove('hidden');
    
    if (!project) {
      this.container.appendChild(this._createEmptyState());
      return;
    }
    
    this.container.appendChild(this._createDetailsView(project));
  }
  
  /**
   * Creates empty state when no project is selected.
   * @private
   */
  _createEmptyState() {
    const hasProjects = true; // This will be determined by caller context
    
    return el('div', { class: 'flex flex-col items-center justify-center h-full text-center py-12 px-6' }, [
      el('div', { class: 'w-16 h-16 rounded-full bg-slate-100 flex items-center justify-center text-slate-400 text-3xl mb-4' }, '📁'),
      el('h3', { class: 'text-lg font-semibold text-slate-900 mb-2' }, 'No project selected'),
      el('p', { class: 'text-sm text-slate-600 max-w-sm' }, 
        hasProjects
          ? 'Select a project from the sidebar to view details, upload documents, and manage requirements.'
          : 'Create your first project to get started with document processing and AI-powered analysis.'
      )
    ]);
  }
  
  /**
   * Creates the details view for a project.
   * @private
   */
  _createDetailsView(p) {
    return el('div', {}, [
      // Header with name and status
      el('div', { class: 'flex items-center gap-2 flex-wrap' }, [
        el('div', { class: 'text-lg font-semibold' }, p.name || '(untitled)'),
        getStatusBadge(p.status)
      ]),
      
      // Description
      p.description ? el('div', { class: 'mt-2 text-slate-700' }, p.description) : null,
      
      // Metadata
      el('div', { class: 'mt-3 grid grid-cols-2 gap-x-4 gap-y-1 text-sm text-slate-600' }, [
        el('div', {}, [
          el('span', { class: 'font-medium' }, 'Created: '),
          formatDate(p.created_at)
        ]),
        el('div', {}, [
          el('span', { class: 'font-medium' }, 'Updated: '),
          formatDate(p.updated_at)
        ])
      ]),
      
      // GitLab Info
      el('div', { class: 'mt-3' }, [
        this._renderGitLabInfo(p)
      ]),
      
      // Action Buttons
      el('div', { class: 'mt-4 flex items-center justify-between flex-wrap gap-2' }, [
        el('div', { class: 'flex items-center gap-2 flex-wrap' }, [
          el('button', { 
            class: 'px-3 py-1.5 border rounded text-sm text-slate-700 hover:bg-slate-50', 
            title: 'Capture requirements', 
            onclick: () => this.callbacks.onOpenRequirements(p) 
          }, 'Requirements'),
          el('button', { 
            class: 'px-3 py-1.5 border rounded text-sm text-slate-700 hover:bg-slate-50', 
            title: 'Open tasks screen', 
            onclick: () => this.callbacks.onOpenTasks(p) 
          }, 'Tasks'),
        ]),
        el('div', { class: 'flex items-center gap-2 flex-wrap' }, [
          el('button', { 
            class: 'px-3 py-1.5 border rounded text-sm text-slate-700 hover:bg-slate-50', 
            onclick: () => this.callbacks.onEdit(p), 
            'aria-label': 'Edit project' 
          }, 'Edit'),
          el('button', { 
            class: 'px-3 py-1.5 bg-rose-600 text-white rounded text-sm hover:bg-rose-700', 
            onclick: () => this.callbacks.onDelete(p), 
            'aria-label': 'Delete project' 
          }, 'Delete')
        ])
      ])
    ]);
  }
  
  /**
   * Renders GitLab information section.
   * @private
   */
  _renderGitLabInfo(project) {
    const repoSection = el('div', { class: 'mb-3' }, [
      el('div', { class: 'text-xs font-semibold text-slate-600 mb-1' }, 'Repository (Source Code)'),
      project.gitlab_repository_url 
        ? el('a', { 
            href: project.gitlab_repository_url, 
            target: '_blank',
            class: 'text-sm text-indigo-600 hover:underline flex items-center gap-1'
          }, [
            '🔗 ',
            project.gitlab_repository_url
          ])
        : el('div', { class: 'text-sm text-slate-400 italic' }, 'No repository URL')
    ]);
    
    const backlogSection = this._renderGitLabBacklogProjects(
      project.gitlab_backlog_project_ids,
      project.gitlab_backlog_project_urls
    );
    
    return el('div', {}, [repoSection, backlogSection]);
  }
  
  /**
   * Renders GitLab backlog projects section.
   * @private
   */
  _renderGitLabBacklogProjects(gitlabProjectIds, gitlabProjectUrls) {
    // Header with cache embeddings button
    const header = el('div', { class: 'flex items-center gap-2 mb-1' }, [
      el('div', { class: 'text-xs font-semibold text-slate-600' }, 'Backlog Projects (Issues/Epics)'),
      gitlabProjectIds && gitlabProjectIds.length > 0 ? el('button', {
        class: 'inline-flex items-center justify-center p-1 rounded transition-all ' + 
               (this.cachingEmbeddings 
                 ? 'text-blue-600 bg-blue-50 cursor-wait' 
                 : 'text-indigo-600 hover:text-indigo-700 hover:bg-indigo-50'),
        disabled: this.cachingEmbeddings,
        onclick: () => this.callbacks.onCacheEmbeddings(this.project),
        title: this.cachingEmbeddings 
          ? 'Caching embeddings in progress...' 
          : 'Cache embeddings for all linked backlog projects to enable AI-powered search and retrieval',
        'aria-label': this.cachingEmbeddings ? 'Caching embeddings' : 'Cache embeddings'
      }, [
        el('svg', { 
          class: 'w-3.5 h-3.5' + (this.cachingEmbeddings ? ' animate-spin' : ''),
          fill: 'none',
          stroke: 'currentColor',
          viewBox: '0 0 24 24',
          innerHTML: '<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z"></path>'
        })
      ]) : null
    ]);
    
    if (!gitlabProjectIds || gitlabProjectIds.length === 0) {
      return el('div', {}, [
        header,
        el('div', { class: 'text-sm text-slate-400 italic' }, 'No backlog projects linked')
      ]);
    }
    
    return el('div', { class: 'space-y-2' }, [
      header,
      el('div', { class: 'grid gap-2' }, 
        gitlabProjectIds.map((id, idx) => {
          const url = gitlabProjectUrls[idx];
          const path = this._extractPathFromUrl(url);
          
          return el('div', { 
            class: 'flex items-center justify-between p-2 bg-slate-50 border border-slate-200 rounded'
          }, [
            el('div', { class: 'flex items-center gap-2 flex-1' }, [
              el('div', { class: 'w-4 h-4 rounded bg-orange-500 flex items-center justify-center text-white text-xs font-bold' }, 'G'),
              el('a', { 
                href: url, 
                target: '_blank',
                class: 'text-sm text-sky-600 hover:underline',
                title: `ID: ${id}`
              }, path)
            ])
          ]);
        })
      )
    ]);
  }
  
  /**
   * Extracts display path from GitLab URL.
   * @private
   */
  _extractPathFromUrl(url) {
    try {
      const parsed = new URL(url);
      return parsed.pathname.replace(/^\//, '').replace(/\.git$/, '');
    } catch {
      return url.replace(/\.git$/, '');
    }
  }
}

