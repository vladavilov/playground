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

/**
 * Creates a DOM element with attributes and children.
 * Convenience helper for programmatic DOM construction.
 * Automatically handles SVG namespace for SVG elements.
 * 
 * @param {string} tag - HTML or SVG tag name
 * @param {Object} [attrs={}] - Element attributes and event handlers
 * @param {Array|string|HTMLElement} [children=[]] - Child elements or text
 * @returns {HTMLElement|SVGElement} Created element
 * 
 * @example
 * // Create a button with click handler
 * const btn = el('button', { 
 *   class: 'btn', 
 *   onclick: () => console.log('clicked') 
 * }, 'Click me');
 * 
 * @example
 * // Create a div with nested children
 * const div = el('div', { class: 'container' }, [
 *   el('h1', {}, 'Title'),
 *   el('p', {}, 'Content')
 * ]);
 * 
 * @example
 * // Create SVG elements (automatically uses SVG namespace)
 * const icon = el('svg', { viewBox: '0 0 24 24' }, [
 *   el('path', { d: 'M12 2L2 7v10l10 5 10-5V7z' })
 * ]);
 */
export function el(tag, attrs = {}, children = []) {
  // SVG elements require proper namespace
  const svgTags = ['svg', 'path', 'circle', 'rect', 'line', 'polyline', 'polygon', 'ellipse', 'g', 'text', 'defs', 'use', 'clipPath', 'mask'];
  const isSVG = svgTags.includes(tag.toLowerCase());
  
  const node = isSVG 
    ? document.createElementNS('http://www.w3.org/2000/svg', tag)
    : document.createElement(tag);
  
  // Set attributes
  Object.entries(attrs).forEach(([k, v]) => {
    if (k === 'class') {
      node.className.baseVal !== undefined ? (node.className.baseVal = v) : (node.className = v);
    } else if (k === 'innerHTML') {
      // Handle innerHTML specially for SVG elements
      node.innerHTML = v;
    } else if (k.startsWith('on') && typeof v === 'function') {
      // Event handler (e.g., 'onclick', 'onkeypress')
      node.addEventListener(k.substring(2), v);
    } else if (v === null || v === undefined || v === false) {
      // Skip null, undefined, or false values
      return;
    } else if (v === true) {
      // For true boolean values, set attribute without value (e.g., disabled, checked)
      node.setAttribute(k, '');
    } else {
      node.setAttribute(k, v);
    }
  });
  
  // Append children
  for (const child of [].concat(children)) {
    if (child == null) continue;
    node.append(typeof child === 'string' ? document.createTextNode(child) : child);
  }
  
  return node;
}

