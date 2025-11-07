/**
 * Breadcrumb Navigation Utility
 * 
 * Provides predictable, hierarchical navigation with visual breadcrumbs.
 * Follows industry standards for breadcrumb UI and navigation patterns.
 * 
 * @module breadcrumb-navigation
 */

'use strict';

/**
 * Navigation hierarchy definition
 */
const NAVIGATION_HIERARCHY = {
  projects: {
    label: 'Projects',
    path: '/pages/projects.html',
    level: 0
  },
  requirements: {
    label: 'Requirements',
    path: '/pages/requirements.html',
    level: 1,
    parent: 'projects'
  },
  tasks: {
    label: 'Tasks',
    path: '/pages/tasks.html',
    level: 2,
    parent: 'requirements'  // Primary parent is requirements
  }
};

/**
 * Builds breadcrumb trail based on current page and context
 * @param {string} currentPage - Current page identifier ('projects', 'requirements', 'tasks')
 * @param {URLSearchParams} params - URL search parameters for context
 * @returns {Array<Object>} Array of breadcrumb items { label, url, active }
 */
export function buildBreadcrumbs(currentPage, params) {
  const trail = [];
  const projectId = params.get('project_id');
  
  // Get navigation node
  const current = NAVIGATION_HIERARCHY[currentPage];
  if (!current) return trail;
  
  // Build trail from root to current
  const buildTrail = (pageKey) => {
    const node = NAVIGATION_HIERARCHY[pageKey];
    if (!node) return;
    
    // Recursively add parents first
    if (node.parent) {
      buildTrail(node.parent);
    }
    
    // Add current node
    const url = projectId && pageKey !== 'projects' 
      ? `${node.path}?project_id=${projectId}`
      : node.path;
      
    trail.push({
      label: node.label,
      url: url,
      active: pageKey === currentPage
    });
  };
  
  buildTrail(currentPage);
  return trail;
}

/**
 * Renders breadcrumb HTML
 * @param {Array<Object>} breadcrumbs - Breadcrumb items
 * @returns {string} HTML string for breadcrumbs
 */
export function renderBreadcrumbs(breadcrumbs) {
  if (!breadcrumbs || breadcrumbs.length === 0) {
    return '';
  }
  
  const items = breadcrumbs.map((crumb, idx) => {
    const isLast = idx === breadcrumbs.length - 1;
    
    if (isLast) {
      // Current page - not clickable
      return `
        <span class="text-slate-700 font-medium">${escapeHtml(crumb.label)}</span>
      `;
    } else {
      // Clickable parent pages
      return `
        <a href="${escapeHtml(crumb.url)}" 
           class="text-sky-600 hover:text-sky-700 font-medium transition-colors">
          ${escapeHtml(crumb.label)}
        </a>
        <svg class="w-4 h-4 text-slate-400 flex-shrink-0" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7"></path>
        </svg>
      `;
    }
  }).join('');
  
  return `
    <nav aria-label="Breadcrumb" class="flex items-center gap-2 text-sm">
      ${items}
    </nav>
  `;
}

/**
 * Initializes breadcrumb navigation for current page
 * @param {string} currentPage - Current page identifier
 * @param {HTMLElement} container - Container element for breadcrumbs
 */
export function initBreadcrumbs(currentPage, container) {
  if (!container) {
    console.warn('Breadcrumb container not found');
    return;
  }
  
  const params = new URLSearchParams(window.location.search);
  const breadcrumbs = buildBreadcrumbs(currentPage, params);
  const html = renderBreadcrumbs(breadcrumbs);
  
  container.innerHTML = html;
}

/**
 * Gets the parent page URL for navigation
 * @param {string} currentPage - Current page identifier
 * @param {URLSearchParams} params - URL search parameters
 * @returns {string} Parent page URL
 */
export function getParentUrl(currentPage, params) {
  const current = NAVIGATION_HIERARCHY[currentPage];
  if (!current || !current.parent) {
    return NAVIGATION_HIERARCHY.projects.path;
  }
  
  const parent = NAVIGATION_HIERARCHY[current.parent];
  const projectId = params.get('project_id');
  
  if (projectId && parent.level > 0) {
    return `${parent.path}?project_id=${projectId}`;
  }
  
  return parent.path;
}

/**
 * Creates a URL with preserved project context
 * @param {string} page - Target page identifier
 * @param {string} projectId - Project ID to preserve
 * @param {Object} additionalParams - Additional URL parameters
 * @returns {string} Complete URL with parameters
 */
export function createNavigationUrl(page, projectId, additionalParams = {}) {
  const node = NAVIGATION_HIERARCHY[page];
  if (!node) {
    return NAVIGATION_HIERARCHY.projects.path;
  }
  
  const params = new URLSearchParams();
  
  if (projectId) {
    params.set('project_id', projectId);
  }
  
  // Add any additional parameters
  Object.entries(additionalParams).forEach(([key, value]) => {
    if (value !== null && value !== undefined) {
      params.set(key, value);
    }
  });
  
  const queryString = params.toString();
  return queryString ? `${node.path}?${queryString}` : node.path;
}

/**
 * Escapes HTML to prevent XSS
 * @param {string} text - Text to escape
 * @returns {string} Escaped text
 * @private
 */
function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}

