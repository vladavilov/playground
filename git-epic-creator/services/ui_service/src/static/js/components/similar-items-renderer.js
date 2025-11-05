/**
 * Similar Items Renderer Utility
 * 
 * Shared rendering logic for similar GitLab items (epics/issues) with proper
 * layout constraints and interactive controls. Ensures consistency between
 * inline editing and focus mode.
 * 
 * @module components/similar-items-renderer
 */

'use strict';

import { escapeHtml as esc } from '../utils/dom-helpers.js';

/**
 * Renders similar items with action controls.
 * Prevents horizontal overflow with proper flex constraints.
 * 
 * @param {Array<Object>} similarItems - Array of similar item objects
 * @param {string} context - Context ('epic' or 'task')
 * @param {number} epicIdx - Epic index
 * @param {number|null} taskIdx - Task index (null for epics)
 * @param {Object} options - Rendering options
 * @param {string} options.mode - 'inline' or 'focus' mode
 * @param {boolean} options.showButtons - Whether to show action buttons (default: true)
 * @returns {string} HTML string
 */
export function renderSimilarItems(similarItems, context, epicIdx, taskIdx = null, options = {}) {
  const {
    mode = 'inline',
    showButtons = true
  } = options;
  
  if (!similarItems || similarItems.length === 0) {
    return '';
  }
  
  const title = context === 'epic' ? '🔗 Similar GitLab Items' : '🔗 Similar Issues';
  const itemType = context === 'epic' ? 'Epic' : 'Issue';
  
  // Mode-specific styling
  const isFocusMode = mode === 'focus';
  const containerPadding = isFocusMode ? 'p-4' : 'p-3';
  const itemPadding = isFocusMode ? 'p-3' : 'p-2';
  const itemSpacing = isFocusMode ? 'mt-3' : 'mt-2';
  const textSize = isFocusMode ? 'text-sm' : 'text-xs';
  const titleSize = isFocusMode ? 'text-sm' : 'text-xs';
  const badgeSize = isFocusMode ? 'px-2.5 py-1 text-xs' : 'px-2 py-0.5 text-xs';
  const buttonSize = isFocusMode ? 'px-3 py-1.5 text-sm' : 'px-2 py-1 text-xs';
  
  let html = `<div class="similar-items-container mt-3 ${containerPadding} bg-blue-50 border border-blue-200 rounded">
    <div class="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-2 mb-2">
      <div class="font-semibold text-blue-800 ${titleSize}">${title}</div>
      <div class="${textSize} text-blue-600">You can link to multiple matches</div>
    </div>
    <div class="space-y-2">`;
  
  similarItems.forEach((sim, simIdx) => {
    const simUrl = sim.url || '#';
    const matchPercent = Math.round((sim.similarity || 0) * 100);
    const decision = sim.link_decision || 'pending';
    const displayId = sim.iid || sim.id; // Prefer IID for display
    
    // Status badge
    let statusBadge = '';
    if (decision === 'accepted') {
      statusBadge = `<span class="${badgeSize} bg-emerald-100 text-emerald-700 rounded font-medium">✓ Accepted</span>`;
    } else if (decision === 'rejected') {
      statusBadge = `<span class="${badgeSize} bg-slate-200 text-slate-600 rounded font-medium">✗ Rejected</span>`;
    } else {
      statusBadge = `<span class="${badgeSize} bg-amber-100 text-amber-700 rounded font-medium">⏱ Pending</span>`;
    }
    
    // Responsive layout: stack on mobile, side-by-side on desktop
    html += `
      <div class="similar-item flex flex-col sm:flex-row sm:items-start gap-2 ${itemPadding} bg-white border border-blue-100 rounded ${itemSpacing} hover:border-blue-200 transition-colors" 
           data-epic-idx="${epicIdx}" 
           data-task-idx="${taskIdx !== null ? taskIdx : ''}" 
           data-sim-idx="${simIdx}"
           data-sim-id="${esc(sim.id)}"
           data-sim-iid="${esc(sim.iid || sim.id)}"
           data-sim-kind="${esc(sim.kind)}"
           data-sim-project-id="${esc(sim.project_id || '')}">
        
        <!-- Content Area: flex-1 with min-w-0 to enable truncation -->
        <div class="flex-1 min-w-0">
          <div class="flex flex-wrap items-center gap-2 mb-1.5">
            <span class="${textSize} font-medium text-slate-600 uppercase">${esc(sim.kind)}</span>
            <a href="${esc(simUrl)}" 
               target="_blank" 
               class="text-blue-600 hover:underline ${textSize} font-medium truncate block max-w-full" 
               title="${esc(sim.title || 'Untitled')}">
              #${esc(displayId)}: ${esc(sim.title || 'Untitled')}
            </a>
            <span class="${textSize} text-slate-500 whitespace-nowrap">(${matchPercent}% match)</span>
          </div>
          
          <div class="flex flex-wrap items-center gap-2">
            ${statusBadge}
            ${sim.status ? `<span class="${textSize} text-slate-500">${esc(sim.status)}</span>` : ''}
            ${sim.project_id ? `<span class="px-1.5 py-0.5 bg-slate-100 text-slate-600 text-xs rounded font-mono" title="GitLab Project ID">proj:${esc(sim.project_id)}</span>` : ''}
          </div>
        </div>
        
        <!-- Action Buttons: flex-shrink-0 to prevent squishing -->
        ${showButtons ? `
        <div class="flex items-center gap-1.5 flex-shrink-0 sm:self-start">
          <button class="similar-action-btn accept-btn ${buttonSize} bg-emerald-500 hover:bg-emerald-600 text-white rounded transition-all duration-200 ${decision === 'accepted' ? 'opacity-50 cursor-not-allowed' : 'hover:shadow-md active:scale-95'} whitespace-nowrap" 
                  title="Link to this existing ${itemType.toLowerCase()}"
                  data-action="accept"
                  ${decision === 'accepted' ? 'disabled' : ''}>
            ✓ Use
          </button>
          <button class="similar-action-btn reject-btn ${buttonSize} bg-slate-400 hover:bg-slate-500 text-white rounded transition-all duration-200 ${decision === 'rejected' ? 'opacity-50 cursor-not-allowed' : 'hover:shadow-md active:scale-95'} whitespace-nowrap" 
                  title="Ignore this match and create new"
                  data-action="reject"
                  ${decision === 'rejected' ? 'disabled' : ''}>
            ✗ Ignore
          </button>
        </div>
        ` : ''}
      </div>
    `;
  });
  
  html += '</div></div>';
  return html;
}

