/**
 * Enhanced Tasks Editor
 * 
 * Provides multi-mode editing experience for backlog (epics and tasks).
 * Extends BaseEditor with tasks-specific field configuration and rendering.
 * 
 * @module tasks-editor
 */

'use strict';

import { BaseEditor } from '../core/base-editor.js';
import { escapeHtml as esc } from '../utils/dom-helpers.js';
import { renderMarkdown } from '../utils/markdown-renderer.js';

/**
 * Enhanced editor for backlog (epics and tasks) with multiple editing modes.
 * Extends BaseEditor to provide tasks-specific functionality.
 */
export class TasksEditor extends BaseEditor {
  
  constructor(bundle, onSave) {
    super(bundle, onSave);
    
    // Set up delete callback with tasks-specific logic
    this.setDeleteCallback((metadata) => {
      const { itemType, epicIdx, taskIdx } = metadata;
      
      if (itemType === 'epic') {
        // Delete entire epic
        this.bundle.epics.splice(epicIdx, 1);
      } else if (itemType === 'task') {
        // Delete task from its epic
        const epic = this.bundle.epics[epicIdx];
        if (epic && epic.tasks) {
          epic.tasks.splice(taskIdx, 1);
        }
      }
      
      this.markUnsaved();
      this.onSave();
    });
    
    // Set up AI enhance callback placeholder
    this.setAIEnhanceCallback((metadata) => {
      // TODO: Implement AI enhancement
      alert('AI enhancement feature will be available soon!');
    });
  }
  
  /**
   * Enables inline editing for all epic and task cards.
   * @override
   */
  enableInlineEditing() {
    // Enable inline editing for epics
    document.querySelectorAll('.epic-card').forEach((epicCard, epicIdx) => {
      const epic = this.bundle.epics[epicIdx];
      if (!epic) return;
      
      // Make epic fields editable
      this.setupInlineField(epicCard, '.epic-title', epic, 'title');
      this.setupInlineField(epicCard, '.epic-description', epic, 'description');
      
      // Add control buttons to epic
      const epicMetadata = { epic, epicIdx, index: epicIdx, itemType: 'epic' };
      
      this.addFocusModeButton(epicCard, epic, () => {
        this.openFocusMode(epic, epicMetadata);
      });
      
      this.addAIEnhanceButton(epicCard, epic, () => {
        this.onAIEnhance(epicMetadata);
      });
      
      this.addDeleteButton(epicCard, epic, () => {
        this.onDelete(epicMetadata);
      });
    });
    
    // Enable inline editing for tasks
    document.querySelectorAll('.task-item').forEach((taskCard) => {
      const epicIdx = parseInt(taskCard.getAttribute('data-epic-idx'));
      const taskIdx = parseInt(taskCard.getAttribute('data-task-idx'));
      
      if (isNaN(epicIdx) || isNaN(taskIdx)) return;
      
      const epic = this.bundle.epics[epicIdx];
      const task = epic?.tasks?.[taskIdx];
      if (!task) return;
      
      // Make task fields editable
      this.setupInlineField(taskCard, '.task-title', task, 'title');
      this.setupInlineField(taskCard, '.task-description', task, 'description');
      this.setupInlineField(taskCard, '.task-acceptance-criteria', task, 'acceptance_criteria');
      
      // Add control buttons to task
      const taskMetadata = { 
        task, 
        epicIdx, 
        taskIdx, 
        index: taskIdx, 
        itemType: 'task',
        parentLabel: `in Epic ${epicIdx + 1}`
      };
      
      this.addFocusModeButton(taskCard, task, () => {
        this.openFocusMode(task, taskMetadata);
      });
      
      this.addAIEnhanceButton(taskCard, task, () => {
        this.onAIEnhance(taskMetadata);
      });
      
      this.addDeleteButton(taskCard, task, () => {
        this.onDelete(taskMetadata);
      });
    });
  }
  
  /**
   * Gets display label for an item.
   * @override
   */
  getItemLabel(item, index) {
    // Determine if this is an epic or task from editingItem metadata
    const itemType = this.editingItem?.itemType || 'epic';
    return itemType === 'epic' ? `E${index + 1}` : `T${index + 1}`;
  }
  
  /**
   * Gets item type label.
   * @override
   */
  getItemTypeLabel() {
    const itemType = this.editingItem?.itemType || 'epic';
    return itemType === 'epic' ? 'Epic' : 'Task';
  }
  
  /**
   * Gets field configurations for an item.
   * @override
   */
  getItemFields(item) {
    const isEpic = this.editingItem?.itemType === 'epic';
    
    const fields = [
      {
        id: 'title',
        label: 'Title',
        value: item.title || '',
        type: 'text',
        placeholder: `Enter ${isEpic ? 'epic' : 'task'} title...`
      },
      {
        id: 'description',
        label: 'Description',
        value: item.description || '',
        type: 'textarea',
        rows: 6,
        placeholder: `Describe the ${isEpic ? 'epic' : 'task'} in detail...`
      }
    ];
    
    // Add task-specific fields
    if (!isEpic) {
      if (item.acceptance_criteria !== undefined) {
        fields.push({
          id: 'acceptance_criteria',
          label: 'Acceptance Criteria',
          value: (item.acceptance_criteria || []).join('\n'),
          type: 'textarea',
          rows: 8,
          placeholder: 'Enter acceptance criteria (one per line)\n- User can complete the action\n- Validation works correctly\n- Success feedback is shown',
          helpText: 'Enter one criterion per line. Lines starting with - or • will be formatted as bullets.',
          monospace: true,
          isArray: true
        });
      }
      
      if (item.dependencies !== undefined) {
        fields.push({
          id: 'dependencies',
          label: 'Dependencies',
          value: (item.dependencies || []).join('\n'),
          type: 'textarea',
          rows: 3,
          placeholder: 'Enter task dependencies (one per line)',
          helpText: 'List tasks that must be completed before this one.',
          monospace: true,
          isArray: true
        });
      }
    }
    
    return fields;
  }
  
  /**
   * Renders preview content HTML.
   * @override
   */
  renderPreviewContent(fields, item) {
    const isEpic = this.editingItem?.itemType === 'epic';
    const title = fields.title || `Untitled ${isEpic ? 'epic' : 'task'}`;
    const description = fields.description || 'No description provided';
    
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
          <span class="inline-flex items-center ${badgeClass} rounded border ${isEpic ? 'bg-indigo-50 text-indigo-700 border-indigo-200' : 'bg-slate-100 text-slate-700 border-slate-200'} font-medium">
            ${esc(isEpic ? 'EPIC' : 'TASK')}
          </span>
        </div>
        
        <div>
          <h4 class="${headingClass} font-semibold text-slate-500 uppercase tracking-wide">Description</h4>
          <div class="${textClass} text-slate-700 leading-relaxed prose prose-sm max-w-none">${descriptionHtml}</div>
        </div>
    `;
    
    // Acceptance criteria
    if (fields.acceptance_criteria) {
      const acceptanceText = fields.acceptance_criteria.trim();
      const acceptanceCriteria = acceptanceText 
        ? acceptanceText.split('\n').filter(l => l.trim())
        : [];
      
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
    }
    
    // Dependencies
    if (fields.dependencies) {
      const depsText = fields.dependencies.trim();
      const dependencies = depsText 
        ? depsText.split('\n').filter(l => l.trim())
        : [];
      
      if (dependencies.length > 0) {
        html += `
          <div>
            <h4 class="${headingClass} font-semibold text-slate-500 uppercase tracking-wide">Dependencies</h4>
            <ul class="${listClass}">
              ${dependencies.map(dep => `<li class="${textClass} text-slate-700 list-disc">${esc(dep)}</li>`).join('')}
            </ul>
          </div>
        `;
      }
    }
    
    // Similar matches section (read-only in preview)
    html += this.renderSimilarMatchesSection(item);
    
    html += '</div>';
    return html;
  }
  
  /**
   * Renders similar matches section for focus mode.
   * @param {Object} item - Epic or task object
   * @returns {string} HTML string
   * @private
   */
  renderSimilarMatchesSection(item) {
    if (!item.similar || item.similar.length === 0) {
      return '';
    }
    
    const isFullscreen = this.currentMode === 'fullscreen';
    const headingClass = isFullscreen ? 'text-sm mb-3' : 'text-xs mb-2';
    const itemType = this.editingItem?.itemType === 'epic' ? 'Epic' : 'Issue';
    
    let html = `
      <div>
        <h4 class="${headingClass} font-semibold text-slate-500 uppercase tracking-wide">🔗 Similar GitLab ${itemType}s</h4>
        <div class="space-y-2">
    `;
    
    item.similar.forEach((sim, simIdx) => {
      const matchPercent = Math.round((sim.similarity || 0) * 100);
      const decision = sim.link_decision || 'pending';
      const simUrl = sim.url || '#';
      
      let statusColor = 'text-amber-600 bg-amber-50';
      let statusText = '⏱ Pending Decision';
      if (decision === 'accepted') {
        statusColor = 'text-emerald-600 bg-emerald-50';
        statusText = '✓ Will Use This';
      } else if (decision === 'rejected') {
        statusColor = 'text-slate-500 bg-slate-50';
        statusText = '✗ Ignored';
      }
      
      html += `
        <div class="p-3 border border-slate-200 rounded bg-slate-50">
          <div class="flex items-center justify-between mb-2">
            <div class="flex items-center gap-2">
              <span class="text-xs font-semibold text-slate-600">${esc(sim.kind).toUpperCase()}</span>
              <a href="${esc(simUrl)}" target="_blank" class="text-blue-600 hover:underline font-medium">
                #${esc(sim.id)}
              </a>
              <span class="text-xs text-slate-500">(${matchPercent}% match)</span>
            </div>
            <span class="px-2 py-0.5 ${statusColor} rounded text-xs font-medium">${statusText}</span>
          </div>
          ${sim.status ? `<div class="text-xs text-slate-500 mb-2">Status: ${esc(sim.status)}</div>` : ''}
          <div class="text-xs text-slate-600">
            You can accept this match to update the existing ${itemType.toLowerCase()} instead of creating a new one.
          </div>
        </div>
      `;
    });
    
    html += `
        </div>
      </div>
    `;
    
    return html;
  }
}
