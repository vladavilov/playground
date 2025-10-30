/**
 * Base Renderer
 * 
 * Provides common rendering functionality for all renderers.
 * Implements shared methods for metadata, quality scores, and empty states.
 * 
 * @module core/base-renderer
 */

'use strict';

import { escapeHtml as esc } from '../utils/dom-helpers.js';

/**
 * Base class for all renderers.
 * Provides common rendering patterns following DRY principle.
 */
export class BaseRenderer {
  /**
   * @param {HTMLElement} contentElement - Container for content
   * @param {HTMLElement} summaryElement - Element to display summary
   * @param {HTMLElement[]} actionButtons - Optional action buttons
   */
  constructor(contentElement, summaryElement, actionButtons = []) {
    if (!contentElement || !summaryElement) {
      throw new Error('BaseRenderer requires contentElement and summaryElement');
    }
    this.contentElement = contentElement;
    this.summaryElement = summaryElement;
    this.actionButtons = actionButtons.filter(btn => btn !== null);
    this.onDelete = null; // Callback for item deletion
    this.onAIEnhance = null; // Callback for AI enhancement
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
   * Shows all action buttons.
   * @protected
   */
  showActionButtons() {
    this.actionButtons.forEach(btn => btn.classList.remove('hidden'));
  }
  
  /**
   * Hides all action buttons.
   * @protected
   */
  hideActionButtons() {
    this.actionButtons.forEach(btn => btn.classList.add('hidden'));
  }
  
  /**
   * Renders empty state with customizable content.
   * @param {Object} config - Empty state configuration
   * @param {string} config.iconPath - SVG path for icon
   * @param {string} config.message - Main message text
   * @param {string} config.submessage - Optional secondary message
   * @protected
   */
  renderEmptyState(config) {
    const { iconPath, message, submessage = '' } = config;
    
    this.contentElement.innerHTML = `
      <div class="flex flex-col items-center justify-center h-full text-center">
        <div class="w-16 h-16 rounded-full bg-slate-200 flex items-center justify-center mb-4">
          <svg class="w-8 h-8 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="${iconPath}"></path>
          </svg>
        </div>
        <p class="text-slate-500">${esc(message)}</p>
        ${submessage ? `<p class="text-xs text-slate-500 mt-1">${esc(submessage)}</p>` : ''}
      </div>
    `;
    this.hideActionButtons();
  }
  
  /**
   * Renders metadata section (assumptions and risks).
   * Provides consistent styling across all renderers.
   * 
   * @param {Object} bundle - Bundle object containing metadata
   * @param {string[]} [bundle.assumptions] - Array of assumptions
   * @param {string[]} [bundle.risks] - Array of risks
   * @param {Object} [options] - Rendering options
   * @param {boolean} [options.useGrid=false] - Use grid layout instead of stacked
   * @returns {string} HTML string
   * @protected
   */
  renderMetadata(bundle, options = {}) {
    const { useGrid = false } = options;
    
    if (!bundle.assumptions?.length && !bundle.risks?.length) {
      return '';
    }
    
    const containerClass = useGrid 
      ? 'grid grid-cols-1 sm:grid-cols-2 gap-4 mb-4'
      : 'bg-white border border-slate-200 rounded-lg p-4 shadow-sm mb-4';
    
    let html = `<div class="${containerClass}">`;
    
    if (bundle.assumptions?.length > 0) {
      const assumptionsHtml = this._renderMetadataSection({
        title: 'Assumptions',
        items: bundle.assumptions,
        textClass: 'text-slate-600',
        titleClass: 'text-slate-700',
        useCard: useGrid
      });
      html += assumptionsHtml;
    }
    
    if (bundle.risks?.length > 0) {
      const risksHtml = this._renderMetadataSection({
        title: 'Risks',
        items: bundle.risks,
        textClass: 'text-rose-600',
        titleClass: 'text-rose-700',
        borderClass: useGrid ? 'border-rose-100' : '',
        useCard: useGrid
      });
      html += risksHtml;
    }
    
    html += '</div>';
    return html;
  }
  
  /**
   * Renders a single metadata section.
   * @private
   */
  _renderMetadataSection(config) {
    const {
      title,
      items,
      textClass,
      titleClass,
      borderClass = 'border-slate-200',
      useCard = false
    } = config;
    
    const wrapper = useCard
      ? `bg-white border ${borderClass} rounded-lg p-4 shadow-sm`
      : `${items === config.items ? 'mb-3' : ''}`;
    
    const itemsList = items
      .map(item => `<li class="text-xs ${textClass} ml-4 list-disc">${esc(item)}</li>`)
      .join('');
    
    if (useCard) {
      return `
        <div class="${wrapper}">
          <h4 class="text-sm font-semibold ${titleClass} mb-2">${esc(title)}</h4>
          <ul class="space-y-1">${itemsList}</ul>
        </div>
      `;
    } else {
      return `
        <div class="${wrapper}">
          <div class="text-sm font-semibold ${titleClass} mb-1">${esc(title)}</div>
          <ul class="space-y-1">${itemsList}</ul>
        </div>
      `;
    }
  }
  
  /**
   * Renders quality score indicator.
   * Provides consistent quality visualization across all renderers.
   * 
   * @param {number|null} score - Quality score (0-1 range)
   * @param {Object} [options] - Rendering options
   * @param {boolean} [options.showLabel=true] - Show "Quality Score" label
   * @returns {string} HTML string
   * @protected
   */
  renderQualityScore(score, options = {}) {
    const { showLabel = true } = options;
    
    if (score == null) return '';
    
    const scoreValue = Math.round(score * 100);
    const { color, label } = this._getScoreColorAndLabel(scoreValue);
    
    return `
      <div class="p-3 bg-gradient-to-r from-${color}-50 to-${color}-100 border border-${color}-200 rounded-lg">
        <div class="flex items-center justify-between">
          ${showLabel ? `<span class="text-sm font-medium text-slate-700">Quality Score</span>` : ''}
          <span class="px-3 py-1 bg-${color}-100 text-${color}-700 rounded-full text-sm font-semibold" title="${label}">
            ${scoreValue}%
          </span>
        </div>
      </div>
    `;
  }
  
  /**
   * Determines color and label based on score value.
   * @private
   */
  _getScoreColorAndLabel(scoreValue) {
    if (scoreValue >= 75) {
      return { color: 'emerald', label: 'Excellent' };
    } else if (scoreValue >= 50) {
      return { color: 'amber', label: 'Good' };
    } else {
      return { color: 'rose', label: 'Needs improvement' };
    }
  }
  
  /**
   * Renders a list of items with a common structure.
   * Utility method for consistent list rendering.
   * 
   * @param {Array} items - Items to render
   * @param {Function} itemRenderer - Function to render each item
   * @param {string} [emptyMessage] - Message when no items
   * @returns {string} HTML string
   * @protected
   */
  renderList(items, itemRenderer, emptyMessage = '') {
    if (!items || items.length === 0) {
      return emptyMessage 
        ? `<p class="text-sm text-slate-500 italic">${esc(emptyMessage)}</p>`
        : '';
    }
    
    return items.map((item, idx) => itemRenderer(item, idx)).join('');
  }
  
  /**
   * Creates a card wrapper with consistent styling.
   * @protected
   */
  createCard(content, options = {}) {
    const {
      classes = 'bg-white border border-slate-200 rounded-lg p-4 shadow-sm',
      hover = false
    } = options;
    
    const hoverClass = hover ? ' hover:shadow-md transition-shadow' : '';
    
    return `<div class="${classes}${hoverClass}">${content}</div>`;
  }
}

