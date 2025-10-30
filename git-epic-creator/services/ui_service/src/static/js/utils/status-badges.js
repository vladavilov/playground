/**
 * Status Badge Utilities
 * 
 * Functions for creating and styling status badges.
 * 
 * @module utils/status-badges
 */

'use strict';

/**
 * Creates a status badge element with appropriate styling.
 * @param {string} status - Status string
 * @returns {HTMLElement} Status badge element
 */
export function getStatusBadge(status) {
  const s = (status || '').toLowerCase();
  const base = 'inline-flex items-center px-2 py-0.5 rounded text-xs font-medium ';
  let cls = 'bg-slate-100 text-slate-700';
  
  if (s === 'analyzing_requirements' || s === 'analyzing') cls = 'bg-blue-100 text-blue-700';
  else if (s === 'retrieving_context') cls = 'bg-cyan-100 text-cyan-700';
  else if (s === 'fetching_backlog') cls = 'bg-teal-100 text-teal-700';
  else if (s === 'drafting_backlog') cls = 'bg-indigo-100 text-indigo-700';
  else if (s === 'mapping_duplicates') cls = 'bg-purple-100 text-purple-700';
  else if (s === 'evaluating') cls = 'bg-amber-100 text-amber-700';
  else if (s === 'needs_clarification') cls = 'bg-orange-100 text-orange-700';
  else if (s === 'processing') cls = 'bg-blue-100 text-blue-700';
  else if (s === 'active') cls = 'bg-emerald-100 text-emerald-700';
  else if (s === 'rag_processing') cls = 'bg-teal-100 text-teal-700';
  else if (s === 'rag_ready') cls = 'bg-green-100 text-green-700';
  else if (s === 'completed') cls = 'bg-emerald-100 text-emerald-700';
  else if (s.includes('error') || s.includes('failed') || s === 'rag_failed') cls = 'bg-rose-100 text-rose-700';
  
  const span = document.createElement('span');
  span.className = base + cls;
  span.textContent = s || 'unknown';
  return span;
}

