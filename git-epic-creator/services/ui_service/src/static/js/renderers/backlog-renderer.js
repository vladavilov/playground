/**
 * Backlog Renderer
 * 
 * Handles rendering of backlog bundles (epics and tasks)
 * with proper formatting, similar items detection, and metadata display.
 * 
 * @module renderers/backlog-renderer
 */

'use strict';

import { BaseRenderer } from '../core/base-renderer.js';
import { escapeHtml as esc } from '../utils/dom-helpers.js';
import { renderMarkdown, initializeMermaid } from '../utils/markdown-renderer.js';

/**
 * Renders backlog bundles to the UI.
 * Extends BaseRenderer for common functionality.
 */
export class BacklogRenderer extends BaseRenderer {
  /**
   * @param {HTMLElement} contentElement - Container for backlog content
   * @param {HTMLElement} summaryElement - Element to display summary text
   * @param {HTMLElement} editButton - Edit button element (optional)
   * @param {HTMLElement} submitButton - Submit button element (optional)
   */
  constructor(contentElement, summaryElement, editButton = null, submitButton = null) {
    super(contentElement, summaryElement, [editButton, submitButton]);
    this.editButton = editButton;
    this.submitButton = submitButton;
  }
  
  /**
   * Renders a backlog bundle.
   * @param {Object} bundle - Backlog bundle object
   */
  render(bundle) {
    if (!bundle || !bundle.epics || bundle.epics.length === 0) {
      super.renderEmptyState({
        iconPath: 'M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2',
        message: 'No epics generated yet'
      });
      this.summaryElement.textContent = 'No backlog yet';
      return;
    }
    
    // Update summary
    const epicCount = bundle.epics.length;
    const taskCount = bundle.epics.reduce((sum, epic) => sum + (epic.tasks?.length || 0), 0);
    this.summaryElement.textContent = 
      `${epicCount} epic${epicCount !== 1 ? 's' : ''}, ${taskCount} task${taskCount !== 1 ? 's' : ''}`;
    
    // Show edit button
    this.showActionButtons();
    
    // Build HTML
    let html = '';
    
    // Epics
    bundle.epics.forEach((epic, epicIdx) => {
      html += this.renderEpic(epic, epicIdx);
    });
    
    // Metadata (Assumptions, Risks) - stacked layout
    html += super.renderMetadata(bundle, { useGrid: false });
    
    // Quality Score
    html += super.renderQualityScore(bundle.score);
    
    this.contentElement.innerHTML = html;
    
    // Trigger Mermaid rendering for dynamically added content
    initializeMermaid(this.contentElement);
  }
  
  /**
   * Renders a single epic with its tasks.
   * @param {Object} epic - Epic object
   * @param {number} epicIdx - Epic index
   * @returns {string} HTML string
   * @private
   */
  renderEpic(epic, epicIdx) {
    const descriptionHtml = renderMarkdown(epic.description || '');
    
    let html = `
      <div class="epic-card bg-white border border-slate-200 rounded-lg p-4 mb-4 shadow-sm">
        <div class="flex items-start justify-between gap-3 mb-3">
          <div class="flex-1">
            <h3 class="epic-title font-semibold text-slate-800">${esc(epic.title)}</h3>
            <div class="epic-description text-sm text-slate-600 mt-1 prose prose-sm max-w-none">${descriptionHtml}</div>
          </div>
          <div class="flex items-center gap-1 card-actions-container">
            <span class="px-2 py-1 bg-indigo-100 text-indigo-700 rounded text-xs font-medium whitespace-nowrap">Epic ${epicIdx + 1}</span>
          </div>
        </div>
    `;
    
    // Similar matches for epic
    if (epic.similar && epic.similar.length > 0) {
      html += this.renderSimilarItems(epic.similar, 'epic', epicIdx);
    }
    
    // Tasks
    if (epic.tasks && epic.tasks.length > 0) {
      html += '<div class="space-y-2">';
      epic.tasks.forEach((task, taskIdx) => {
        html += this.renderTask(task, taskIdx, epicIdx);
      });
      html += '</div>';
    }
    
    html += '</div>';
    return html;
  }
  
  /**
   * Renders a single task.
   * @param {Object} task - Task object
   * @param {number} taskIdx - Task index
   * @param {number} epicIdx - Parent epic index
   * @returns {string} HTML string
   * @private
   */
  renderTask(task, taskIdx, epicIdx) {
    const descriptionHtml = renderMarkdown(task.description || '');
    
    let html = `
      <div class="task-item border border-slate-200 rounded-lg p-3 bg-slate-50" data-epic-idx="${epicIdx}" data-task-idx="${taskIdx}">
        <div class="flex items-start justify-between gap-2">
          <div class="flex-1">
            <div class="task-title font-medium text-sm text-slate-800">${esc(task.title)}</div>
            <div class="task-description text-xs text-slate-600 mt-1 prose prose-xs max-w-none">${descriptionHtml}</div>
          </div>
          <div class="flex items-center gap-1 card-actions-container">
            <span class="px-1.5 py-0.5 bg-slate-200 text-slate-700 rounded text-xs whitespace-nowrap">T${taskIdx + 1}</span>
          </div>
        </div>
    `;
    
    // Acceptance criteria
    if (task.acceptance_criteria && task.acceptance_criteria.length > 0) {
      html += `
        <div class="task-acceptance-criteria mt-2 text-xs">
          <div class="font-medium text-slate-700 mb-1">Acceptance Criteria:</div>
          <ul class="space-y-0.5">
            ${task.acceptance_criteria.map(ac => `<li class="ml-4 list-disc text-slate-600">${esc(ac)}</li>`).join('')}
          </ul>
        </div>
      `;
    }
    
    // Dependencies
    if (task.dependencies && task.dependencies.length > 0) {
      html += `<div class="mt-2 text-xs text-slate-600">Dependencies: ${task.dependencies.map(d => esc(d)).join(', ')}</div>`;
    }
    
    // Similar matches for task
    if (task.similar && task.similar.length > 0) {
      html += this.renderSimilarItems(task.similar, 'task', epicIdx, taskIdx);
    }
    
    html += '</div>';
    return html;
  }
  
  /**
   * Renders similar items with action controls.
   * @param {Array<Object>} similarItems - Array of similar item objects
   * @param {string} context - Context ('epic' or 'task')
   * @param {number} epicIdx - Epic index (for task context)
   * @param {number} taskIdx - Task index (optional, for task context)
   * @returns {string} HTML string
   * @private
   */
  renderSimilarItems(similarItems, context = 'epic', epicIdx = 0, taskIdx = null) {
    const title = context === 'epic' ? '🔗 Similar GitLab Items' : '🔗 Similar Issues';
    const itemType = context === 'epic' ? 'Epic' : 'Issue';
    
    let html = `<div class="similar-items-container mt-3 p-3 bg-blue-50 border border-blue-200 rounded">
      <div class="flex items-center justify-between mb-2">
        <div class="font-semibold text-blue-800 text-xs">${title}</div>
        <div class="text-xs text-blue-600">Choose an action for each match:</div>
      </div>`;
    
    similarItems.forEach((sim, simIdx) => {
      const simUrl = sim.url || '#';
      const matchPercent = Math.round((sim.similarity || 0) * 100);
      const decision = sim.link_decision || 'pending';
      
      // Status badge
      let statusBadge = '';
      if (decision === 'accepted') {
        statusBadge = '<span class="px-2 py-0.5 bg-emerald-100 text-emerald-700 text-xs rounded font-medium">✓ Accepted</span>';
      } else if (decision === 'rejected') {
        statusBadge = '<span class="px-2 py-0.5 bg-slate-200 text-slate-600 text-xs rounded font-medium">✗ Rejected</span>';
      } else {
        statusBadge = '<span class="px-2 py-0.5 bg-amber-100 text-amber-700 text-xs rounded font-medium">⏱ Pending</span>';
      }
      
      html += `
        <div class="similar-item flex items-start justify-between gap-2 p-2 bg-white border border-blue-100 rounded mt-2" 
             data-epic-idx="${epicIdx}" 
             data-task-idx="${taskIdx !== null ? taskIdx : ''}" 
             data-sim-idx="${simIdx}"
             data-sim-id="${esc(sim.id)}"
             data-sim-kind="${esc(sim.kind)}">
          <div class="flex-1 min-w-0">
            <div class="flex items-center gap-2 mb-1">
              <span class="text-xs font-medium text-slate-600">${esc(sim.kind).toUpperCase()}</span>
              <a href="${esc(simUrl)}" target="_blank" class="text-blue-600 hover:underline text-xs font-medium">
                #${esc(sim.id)}
              </a>
              <span class="text-xs text-slate-500">(${matchPercent}% match)</span>
            </div>
            <div class="flex items-center gap-2">
              ${statusBadge}
              ${sim.status ? `<span class="text-xs text-slate-500">${esc(sim.status)}</span>` : ''}
            </div>
          </div>
          <div class="flex items-center gap-1 flex-shrink-0">
            <button class="similar-action-btn accept-btn px-2 py-1 text-xs bg-emerald-500 hover:bg-emerald-600 text-white rounded transition-colors ${decision === 'accepted' ? 'opacity-50' : ''}" 
                    title="Use this existing ${itemType.toLowerCase()} (link or update)"
                    data-action="accept"
                    ${decision === 'accepted' ? 'disabled' : ''}>
              ✓ Use
            </button>
            <button class="similar-action-btn reject-btn px-2 py-1 text-xs bg-slate-400 hover:bg-slate-500 text-white rounded transition-colors ${decision === 'rejected' ? 'opacity-50' : ''}" 
                    title="Ignore this match and create new"
                    data-action="reject"
                    ${decision === 'rejected' ? 'disabled' : ''}>
              ✗ Ignore
            </button>
          </div>
        </div>
      `;
    });
    
    html += '</div>';
    return html;
  }
  
}

