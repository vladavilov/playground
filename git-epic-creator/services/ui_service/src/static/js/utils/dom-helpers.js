/**
 * DOM Helper Utilities
 * 
 * Common DOM manipulation and utility functions.
 * 
 * @module utils/dom-helpers
 */

'use strict';

/**
 * Escapes HTML special characters to prevent XSS.
 * @param {string} s - String to escape
 * @returns {string} Escaped string
 */
export function escapeHtml(s) {
  return String(s ?? '').replace(/[&<>]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));
}

/**
 * Scrolls an element to the bottom.
 * @param {HTMLElement} element - Element to scroll
 */
export function scrollToBottom(element) {
  if (element) element.scrollTop = element.scrollHeight;
}

/**
 * Smart scroll: only scrolls to bottom if user is already near bottom (within threshold).
 * This prevents disrupting user's manual scrolling.
 * @param {HTMLElement} element - Element to scroll
 * @param {number} threshold - Distance from bottom in pixels (default: 50)
 * @returns {boolean} True if scrolled, false if not
 */
export function smartScrollToBottom(element, threshold = 50) {
  if (!element) return false;
  
  const isNearBottom = element.scrollHeight - element.scrollTop - element.clientHeight <= threshold;
  
  if (isNearBottom) {
    element.scrollTop = element.scrollHeight;
    return true;
  }
  return false;
}

/**
 * Creates a loading manager for showing/hiding loading overlays.
 * @returns {Object} Loading manager with show() and hide() methods
 */
export function createLoadingManager() {
  let loadingCount = 0;
  
  return {
    show() {
      loadingCount += 1;
      const overlay = document.getElementById('loadingOverlay');
      if (overlay && loadingCount > 0) {
        overlay.classList.remove('hidden');
        overlay.classList.add('flex');
      }
    },
    hide() {
      loadingCount = Math.max(0, loadingCount - 1);
      const overlay = document.getElementById('loadingOverlay');
      if (overlay && loadingCount === 0) {
        overlay.classList.add('hidden');
        overlay.classList.remove('flex');
      }
    }
  };
}

