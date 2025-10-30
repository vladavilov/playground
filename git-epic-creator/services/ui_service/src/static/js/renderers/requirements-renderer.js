/**
 * Requirements Renderer
 * 
 * Handles rendering of requirements bundles (business and functional requirements)
 * with proper formatting, priority indicators, and metadata display.
 * 
 * @module renderers/requirements-renderer
 */

'use strict';

import { BaseRenderer } from '../core/base-renderer.js';
import { escapeHtml as esc } from '../utils/dom-helpers.js';
import { renderMarkdown, initializeMermaid } from '../utils/markdown-renderer.js';

/**
 * Renders requirements bundles to the UI.
 * Extends BaseRenderer for common functionality.
 */
export class RequirementsRenderer extends BaseRenderer {
  /**
   * @param {HTMLElement} contentElement - Container for requirements content
   * @param {HTMLElement} summaryElement - Element to display summary text
   * @param {HTMLElement} editButton - Edit button element (optional)
   * @param {HTMLElement} confirmButton - Confirm button element (optional)
   */
  constructor(contentElement, summaryElement, editButton = null, confirmButton = null) {
    super(contentElement, summaryElement, [editButton, confirmButton]);
    this.editButton = editButton;
    this.confirmButton = confirmButton;
  }
  
  /**
   * Renders a requirements bundle.
   * @param {Object} bundle - Requirements bundle object
   * @returns {Promise<void>}
   */
  async render(bundle) {
    if (!bundle || (!bundle.business_requirements?.length && !bundle.functional_requirements?.length)) {
      super.renderEmptyState({
        iconPath: 'M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z',
        message: 'No requirements generated yet'
      });
      this.summaryElement.textContent = 'No requirements yet';
      return;
    }
    
    // Update summary
    const businessCount = bundle.business_requirements?.length || 0;
    const functionalCount = bundle.functional_requirements?.length || 0;
    const totalCount = businessCount + functionalCount;
    
    this.summaryElement.textContent = 
      `${totalCount} requirement${totalCount !== 1 ? 's' : ''} (${businessCount} business, ${functionalCount} functional)`;
    
    // Show action buttons
    this.showActionButtons();
    
    // Render content
    let html = '';
    
    // Business Requirements
    if (bundle.business_requirements?.length > 0) {
      html += this.renderRequirementSection('Business Requirements', 'BR', bundle.business_requirements);
    }
    
    // Functional Requirements
    if (bundle.functional_requirements?.length > 0) {
      html += this.renderRequirementSection('Functional Requirements', 'FR', bundle.functional_requirements);
    }
    
    // Metadata (Assumptions, Risks) - use grid layout
    html += super.renderMetadata(bundle, { useGrid: true });
    
    // Quality Score
    html += super.renderQualityScore(bundle.score);
    
    this.contentElement.innerHTML = html;
    
    // Initialize Mermaid diagrams in descriptions
    // Note: Using initializeMermaid() here because we compose HTML from multiple renderMarkdown() calls
    await initializeMermaid(this.contentElement);
  }
  
  /**
   * Renders a section of requirements (business or functional).
   * @param {string} sectionTitle - Section title
   * @param {string} typePrefix - Type prefix (BR or FR)
   * @param {Array<Object>} requirements - Array of requirement objects
   * @returns {string} HTML string
   * @private
   */
  renderRequirementSection(sectionTitle, typePrefix, requirements) {
    return `
      <div class="mb-6">
        <h3 class="text-xs uppercase tracking-wide font-semibold text-slate-500 mb-3">${esc(sectionTitle)}</h3>
        <div class="space-y-3">
          ${requirements.map((req, idx) => this.renderRequirementCard(req, idx, typePrefix)).join('')}
        </div>
      </div>
    `;
  }
  
  /**
   * Renders a single requirement card.
   * @param {Object} req - Requirement object
   * @param {number} idx - Requirement index
   * @param {string} typePrefix - Type prefix (BR or FR)
   * @returns {string} HTML string
   * @private
   */
  renderRequirementCard(req, idx, typePrefix) {
    const descriptionHtml = renderMarkdown(req.description || '');
    
    return `
      <div class="req-card bg-white border border-slate-200 rounded-lg p-4 shadow-sm hover:shadow-md transition-all" data-req-idx="${idx}" data-req-type="${typePrefix}">
        <div class="flex items-start justify-between gap-2 mb-2">
          <h3 class="req-title font-semibold text-slate-800 flex-1">${esc(req.title || `Requirement ${idx + 1}`)}</h3>
          <div class="flex items-center gap-1 card-actions-container">
            <span class="px-2 py-0.5 bg-indigo-100 text-indigo-700 rounded text-xs font-medium whitespace-nowrap">${typePrefix}${idx + 1}</span>
          </div>
        </div>
        <div class="req-description text-sm text-slate-600 mb-3 prose prose-sm max-w-none">${descriptionHtml}</div>
        <div class="flex items-center gap-2 mb-2">
          <span class="text-xs font-medium text-slate-700">Priority:</span>
          ${this.renderPriorityPill(req.priority)}
        </div>
        ${req.acceptance_criteria?.length ? `
          <div class="req-acceptance-criteria mt-3 pt-3 border-t border-slate-100">
            <div class="text-xs font-semibold text-slate-700 mb-1.5">Acceptance Criteria</div>
            <ul class="space-y-1">
              ${req.acceptance_criteria.map(ac => `<li class="text-xs text-slate-600 ml-4 list-disc">${esc(ac)}</li>`).join('')}
            </ul>
          </div>
        ` : ''}
      </div>
    `;
  }
  
  /**
   * Renders a priority pill badge.
   * @param {string} priority - Priority value
   * @returns {string} HTML string
   * @private
   */
  renderPriorityPill(priority) {
    const val = String(priority || '').toLowerCase();
    let cls = 'bg-slate-100 text-slate-700 border-slate-200';
    
    if (val === 'must') cls = 'bg-amber-50 text-amber-700 border-amber-200';
    else if (val === 'should') cls = 'bg-sky-50 text-sky-700 border-sky-200';
    else if (val === 'could') cls = 'bg-emerald-50 text-emerald-700 border-emerald-200';
    
    return `<span class="priority-pill inline-flex items-center px-2 py-0.5 rounded border ${cls} text-xs font-medium">${esc(priority)}</span>`;
  }
  
}

