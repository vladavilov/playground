/**
 * Enhanced Requirements Editor
 * 
 * Provides multi-mode editing experience for requirements.
 * Extends BaseEditor with requirements-specific field configuration and rendering.
 * 
 * @module requirements-editor
 */

'use strict';

import { BaseEditor } from '../core/base-editor.js';
import { escapeHtml as esc } from '../utils/dom-helpers.js';
import { renderMarkdown } from '../utils/markdown-renderer.js';

/**
 * Enhanced editor for requirements with multiple editing modes.
 * Extends BaseEditor to provide requirements-specific functionality.
 */
export class RequirementsEditor extends BaseEditor {
  
  constructor(bundle, onSave) {
    super(bundle, onSave);
    
    // Reference to controller for event handling (will be set by controller)
    this.controller = null;
    
    // Set up delete callback with requirements-specific logic
    this.setDeleteCallback((metadata) => {
      const { reqType, index } = metadata;
      const list = reqType === 'business' 
        ? this.bundle.business_requirements 
        : this.bundle.functional_requirements;
      list.splice(index, 1);
      this.markUnsaved();
      this.onSave();
    });
    
    // Set up AI enhance callback
    this.setAIEnhanceCallback(async (metadata) => {
      await this.handleRequirementEnhancement(metadata);
    });
  }
  
  /**
   * Sets the controller reference for event handling.
   * @param {Object} controller - Requirements controller instance
   */
  setController(controller) {
    this.controller = controller;
  }
  
  /**
   * Handles AI enhancement of a single requirement.
   * @param {Object} metadata - Requirement metadata with req, reqType, index
   */
  async handleRequirementEnhancement(metadata) {
    if (!this.controller) {
      console.error('Controller not set on RequirementsEditor');
      return;
    }
    
    const { req, reqType, index } = metadata;
    
    // Find the card element
    const card = document.querySelector(`.req-card:nth-child(${index + 1})`);
    if (!card) {
      console.error('Could not find requirement card for enhancement');
      return;
    }
    
    try {
      // Show progress on the card
      this.showCardProgress(card, 'Enhancing with AI...');
      
      // Call controller to perform enhancement
      const enhanced = await this.controller.enhanceRequirement(req, reqType, index);
      
      // Update the requirement in the bundle
      const list = reqType === 'business' 
        ? this.bundle.business_requirements 
        : this.bundle.functional_requirements;
      
      Object.assign(list[index], enhanced);
      
      // Mark as unsaved and trigger save
      this.markUnsaved();
      this.onSave();
      
      // Hide progress indicator
      this.hideCardProgress(card);
      
      // Re-enable inline editing for the updated card
      setTimeout(() => {
        this.enableInlineEditing();
      }, 100);
      
    } catch (error) {
      console.error('Enhancement failed:', error);
      this.showCardError(card, error.message || 'Enhancement failed. Please try again.');
    }
  }
  
  /**
   * Enables inline editing for all requirement cards.
   * @override
   */
  enableInlineEditing() {
    document.querySelectorAll('.req-card').forEach((card, globalIdx) => {
      // Determine requirement type and index
      const isBusinessReq = globalIdx < (this.bundle.business_requirements?.length || 0);
      const reqType = isBusinessReq ? 'business' : 'functional';
      const reqList = isBusinessReq ? this.bundle.business_requirements : this.bundle.functional_requirements;
      const localIdx = isBusinessReq ? globalIdx : globalIdx - (this.bundle.business_requirements?.length || 0);
      const req = reqList[localIdx];
      
      if (!req) return;
      
      // Make fields editable
      this.setupInlineField(card, '.req-title', req, 'title');
      this.setupInlineField(card, '.req-description', req, 'description');
      this.setupInlineField(card, '.req-acceptance-criteria', req, 'acceptance_criteria');
      this.setupInlinePriority(card, req);
      
      // Add control buttons with metadata
      const metadata = { req, reqType, index: localIdx, itemType: 'requirement' };
      
      this.addFocusModeButton(card, req, () => {
        this.openFocusMode(req, metadata);
      });
      
      this.addAIEnhanceButton(card, req, () => {
        this.onAIEnhance(metadata);
      });
      
      this.addDeleteButton(card, req, () => {
        this.onDelete(metadata);
      });
    });
  }
  
  /**
   * Sets up inline editing for priority dropdown.
   * @private
   */
  setupInlinePriority(card, req) {
    const priorityPill = card.querySelector('.priority-pill');
    if (!priorityPill) return;
    
    priorityPill.classList.add('cursor-pointer', 'hover:ring-2', 'hover:ring-indigo-300', 'transition-all');
    priorityPill.title = 'Click to change priority';
    
    priorityPill.addEventListener('click', () => {
      const select = document.createElement('select');
      select.className = 'border border-indigo-300 rounded px-2 py-1 text-xs font-medium focus:ring-2 focus:ring-indigo-500';
      
      ['MUST', 'SHOULD', 'COULD', 'WONT'].forEach(priority => {
        const option = document.createElement('option');
        option.value = priority;
        option.textContent = priority;
        option.selected = req.priority === priority;
        select.appendChild(option);
      });
      
      const parent = priorityPill.parentElement;
      parent.replaceChild(select, priorityPill);
      select.focus();
      
      const save = () => {
        req.priority = select.value;
        this.markUnsaved();
        this.scheduleAutoSave();
        parent.replaceChild(priorityPill, select);
        priorityPill.textContent = select.value;
        this.setupInlinePriority(card, req);
      };
      
      select.addEventListener('blur', save);
      select.addEventListener('change', save);
      select.addEventListener('keydown', (e) => {
        if (e.key === 'Escape') {
          parent.replaceChild(priorityPill, select);
          this.setupInlinePriority(card, req);
        } else if (e.key === 'Enter') {
          save();
        }
      });
    });
  }
  
  /**
   * Gets display label for an item.
   * @override
   */
  getItemLabel(item, index) {
    const reqType = this.editingItem?.reqType || 'business';
    return reqType === 'business' ? `BR${index + 1}` : `FR${index + 1}`;
  }
  
  /**
   * Gets item type label.
   * @override
   */
  getItemTypeLabel() {
    const reqType = this.editingItem?.reqType || 'business';
    return reqType === 'business' ? 'Business Requirement' : 'Functional Requirement';
  }
  
  /**
   * Gets field configurations for an item.
   * @override
   */
  getItemFields(item) {
    return [
      {
        id: 'title',
        label: 'Title',
        value: item.title || '',
        type: 'text',
        placeholder: 'Enter requirement title...'
      },
      {
        id: 'description',
        label: 'Description',
        value: item.description || '',
        type: 'textarea',
        rows: 6,
        placeholder: 'Describe the requirement in detail...',
        helpText: 'Supports markdown formatting'
      },
      {
        id: 'priority',
        label: 'Priority',
        value: item.priority || 'MUST',
        type: 'select',
        options: [
          { value: 'MUST', label: 'MUST - Critical requirement' },
          { value: 'SHOULD', label: 'SHOULD - Important requirement' },
          { value: 'COULD', label: 'COULD - Nice to have' },
          { value: 'WONT', label: 'WONT - Not in scope' }
        ]
      },
      {
        id: 'acceptance_criteria',
        label: 'Acceptance Criteria',
        value: (item.acceptance_criteria || []).join('\n'),
        type: 'textarea',
        rows: 8,
        placeholder: 'Enter acceptance criteria (one per line)\n- User can login with email\n- Password must be validated\n- Success message is displayed',
        helpText: 'Enter one criterion per line. Lines starting with - or • will be formatted as bullets.',
        monospace: true,
        isArray: true
      }
    ];
  }
  
  /**
   * Renders preview content HTML.
   * @override
   */
  renderPreviewContent(fields, item) {
    const title = fields.title || 'Untitled requirement';
    const description = fields.description || 'No description provided';
    const priority = fields.priority || 'MUST';
    
    const acceptanceText = fields.acceptance_criteria?.trim() || '';
    const acceptanceCriteria = acceptanceText 
      ? acceptanceText.split('\n').filter(l => l.trim())
      : [];
    
    const priorityColor = this.getPriorityColor(priority);
    
    // Determine size classes based on mode
    const isFullscreen = this.currentMode === 'fullscreen';
    const titleClass = isFullscreen ? 'text-2xl' : 'text-lg';
    const badgeClass = isFullscreen ? 'px-3 py-1.5 text-sm' : 'px-2 py-1 text-xs';
    const sectionSpacing = isFullscreen ? 'space-y-6' : 'space-y-4';
    const textClass = isFullscreen ? 'text-base' : 'text-sm';
    const headingClass = isFullscreen ? 'text-sm mb-3' : 'text-xs mb-2';
    const listClass = isFullscreen ? 'space-y-2 ml-5' : 'space-y-1.5 ml-4';
    
    // Render description as markdown
    const descriptionHtml = renderMarkdown(description);
    
    let html = `
      <div class="${sectionSpacing}">
        <div>
          <h3 class="${titleClass} font-semibold text-slate-800 mb-2">${esc(title)}</h3>
          <span class="inline-flex items-center ${badgeClass} rounded border ${priorityColor} font-medium">
            ${esc(priority)}
          </span>
        </div>
        
        <div>
          <h4 class="${headingClass} font-semibold text-slate-500 uppercase tracking-wide">Description</h4>
          <div class="${textClass} text-slate-700 leading-relaxed prose prose-sm max-w-none">${descriptionHtml}</div>
        </div>
    `;
    
    if (acceptanceCriteria.length > 0) {
      html += `
        <div>
          <h4 class="${headingClass} font-semibold text-slate-500 uppercase tracking-wide">Acceptance Criteria</h4>
          <ul class="${listClass}">
            ${acceptanceCriteria.map(ac => {
              const cleaned = ac.replace(/^[-•*]\s*/, '');
              return `<li class="${textClass} text-slate-700 list-disc">${esc(cleaned)}</li>`;
            }).join('')}
          </ul>
        </div>
      `;
    }
    
    html += '</div>';
    return html;
  }
  
  /**
   * Gets priority color classes.
   * @private
   */
  getPriorityColor(priority) {
    const val = String(priority || '').toLowerCase();
    if (val === 'must') return 'bg-amber-50 text-amber-700 border-amber-200';
    if (val === 'should') return 'bg-sky-50 text-sky-700 border-sky-200';
    if (val === 'could') return 'bg-emerald-50 text-emerald-700 border-emerald-200';
    return 'bg-slate-100 text-slate-700 border-slate-200';
  }
}
