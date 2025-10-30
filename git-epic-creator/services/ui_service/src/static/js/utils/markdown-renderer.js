/**
 * Markdown Renderer Utility
 * 
 * Centralized markdown rendering using markdown-it with full GFM support,
 * tables, task lists, and Mermaid diagram integration.
 * 
 * Usage Patterns:
 * 
 * 1. Simple case (single markdown text to single element):
 *    Use renderMarkdownWithMermaid(text, element) - this is the preferred entry point
 * 
 * 2. Complex composition (multiple markdown snippets combined into larger HTML):
 *    - Use renderMarkdown(text) for individual pieces (returns HTML string)
 *    - Compose the HTML strings as needed
 *    - Insert the final HTML into the DOM
 *    - Call initializeMermaid(container) to process all pending Mermaid diagrams
 * 
 * @module utils/markdown-renderer
 */

'use strict';

import { escapeHtml as esc } from './dom-helpers.js';

/**
 * Singleton markdown-it instance with GFM support
 */
let markdownInstance = null;

/**
 * Initializes markdown-it with GFM, tables, and task list support
 * @private
 */
function initializeMarkdownIt() {
  if (!markdownInstance && typeof markdownit !== 'undefined') {
    // Initialize markdown-it with GFM options
    markdownInstance = window.markdownit({
      html: false,         // Disable HTML tags in source for security
      xhtmlOut: true,      // Use XHTML-compliant output
      breaks: true,        // Convert \n to <br>
      linkify: true,       // Auto-convert URLs to links
      typographer: true,   // Enable smartquotes and other typographic replacements
    });
  }
}

/**
 * Renders markdown text to HTML with full GFM support
 * 
 * Features:
 * - GitHub Flavored Markdown (GFM)
 * - Tables
 * - Task lists
 * - Autolinks
 * - Strikethrough
 * - Mermaid diagrams (post-processed)
 * 
 * @param {string} text - Markdown text to render
 * @param {Object} [options] - Rendering options
 * @param {boolean} [options.inline=false] - Use inline rendering (no block elements)
 * @param {boolean} [options.enableMermaid=true] - Enable Mermaid diagram rendering
 * @returns {string} HTML string
 */
export function renderMarkdown(text, options = {}) {
  const { inline = false, enableMermaid = true } = options;
  
  if (!text) return '';
  
  // Initialize markdown-it on first use
  if (!markdownInstance) {
    initializeMarkdownIt();
  }
  
  // Fallback if markdown-it is not available
  if (!markdownInstance) {
    console.warn('[MarkdownRenderer] markdown-it library not loaded');
    return esc(text).replace(/\n/g, '<br>');
  }
  
  try {
    // Render markdown to HTML
    let html = inline 
      ? markdownInstance.renderInline(text)
      : markdownInstance.render(text);
    
    // Post-process for Mermaid diagrams if enabled
    if (enableMermaid) {
      html = processMermaidBlocks(html);
    }
    
    return html;
  } catch (error) {
    console.error('[MarkdownRenderer] Error rendering markdown:', error);
    // Fallback to escaped text with line breaks
    return esc(text).replace(/\n/g, '<br>');
  }
}

/**
 * Post-processes HTML to extract and prepare Mermaid code blocks
 * Converts <pre><code class="language-mermaid">...</code></pre> to 
 * <div data-mermaid-pending>...</div> for programmatic rendering
 * 
 * @param {string} html - HTML string to process
 * @returns {string} Processed HTML with Mermaid placeholders
 * @private
 */
function processMermaidBlocks(html) {
  if (!html) return html;
  
  // Match mermaid code blocks: <pre><code class="language-mermaid">...</code></pre>
  const mermaidRegex = /<pre><code\s+class="language-mermaid">([\s\S]*?)<\/code><\/pre>/gi;
  
  let diagramIndex = 0;
  return html.replace(mermaidRegex, (match, code) => {
    // Unescape HTML entities in the code
    const unescapedCode = unescapeHtmlEntities(code);
    
    // Generate unique ID for this diagram
    const diagramId = `mermaid-diagram-${Date.now()}-${diagramIndex++}`;
    
    // Return placeholder div with diagram definition stored in data attribute
    return `<div id="${diagramId}" class="mermaid-container" data-mermaid-pending="true" data-mermaid-definition="${esc(unescapedCode)}"></div>`;
  });
}

/**
 * Unescapes HTML entities
 * @param {string} text - Text with HTML entities
 * @returns {string} Unescaped text
 * @private
 */
function unescapeHtmlEntities(text) {
  const textarea = document.createElement('textarea');
  textarea.innerHTML = text;
  return textarea.value;
}

/**
 * Initializes Mermaid for rendering diagrams using the programmatic render API
 * Should be called after markdown content is rendered to the DOM
 * 
 * Note: For simple cases, prefer using renderMarkdownWithMermaid() instead.
 * This function is useful when you need to compose complex HTML from multiple
 * markdown sources before initializing Mermaid.
 * 
 * @param {HTMLElement} [container=document] - Container element to search for pending diagrams
 * @returns {Promise<void>}
 */
export async function initializeMermaid(container = document) {
  if (typeof mermaid === 'undefined') {
    console.warn('[MarkdownRenderer] Mermaid library not loaded');
    return;
  }
  
  try {
    // Initialize Mermaid with configuration (only once)
    mermaid.initialize({
      startOnLoad: false,
      theme: 'default',
      securityLevel: 'loose',
      flowchart: {
        useMaxWidth: true,
        htmlLabels: true,
      },
      sequence: {
        useMaxWidth: true,
      },
    });
    
    // Find all pending Mermaid diagrams in the container
    const pendingDiagrams = container.querySelectorAll('[data-mermaid-pending="true"]');
    
    if (pendingDiagrams.length === 0) {
      return;
    }
    
    // Render each diagram using the programmatic API
    const renderPromises = Array.from(pendingDiagrams).map(async (element) => {
      try {
        const diagramId = element.id;
        const definition = element.getAttribute('data-mermaid-definition');
        
        if (!definition || !diagramId) {
          console.warn('[MarkdownRenderer] Mermaid diagram missing definition or ID');
          return;
        }
        
        // Use mermaid.render() API for programmatic rendering
        const { svg, bindFunctions } = await mermaid.render(diagramId, definition);
        
        // Insert the rendered SVG
        element.innerHTML = svg;
        element.classList.remove('data-mermaid-pending');
        element.removeAttribute('data-mermaid-pending');
        element.removeAttribute('data-mermaid-definition');
        
        // Bind any interactive functions (for clicks, tooltips, etc.)
        if (bindFunctions) {
          bindFunctions(element);
        }
      } catch (error) {
        console.error('[MarkdownRenderer] Error rendering Mermaid diagram:', error);
        // Display error message in the element
        element.innerHTML = `<div class="p-4 bg-rose-50 border border-rose-200 rounded text-rose-700 text-sm">
          <strong>Mermaid Rendering Error:</strong><br/>
          ${esc(error.message || 'Unknown error')}
        </div>`;
        element.removeAttribute('data-mermaid-pending');
      }
    });
    
    await Promise.all(renderPromises);
  } catch (error) {
    console.error('[MarkdownRenderer] Error initializing Mermaid:', error);
  }
}

/**
 * Renders markdown and initializes Mermaid in one call
 * Convenience method for common use case
 * 
 * @param {string} text - Markdown text to render
 * @param {HTMLElement} targetElement - Element to render into
 * @param {Object} [options] - Rendering options
 * @returns {Promise<void>}
 */
export async function renderMarkdownWithMermaid(text, targetElement, options = {}) {
  if (!targetElement) {
    console.error('[MarkdownRenderer] Target element is required');
    return;
  }
  
  // Render markdown
  const html = renderMarkdown(text, options);
  targetElement.innerHTML = html;
  
  // Initialize Mermaid diagrams
  if (options.enableMermaid !== false) {
    await initializeMermaid(targetElement);
  }
}
