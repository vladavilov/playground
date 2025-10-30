/**
 * Markdown Renderer Utility
 * 
 * Centralized markdown rendering using markdown-it with full GFM support
 * and custom Mermaid diagram integration.
 * 
 * @module utils/markdown-renderer
 */

'use strict';

import { escapeHtml as esc } from './dom-helpers.js';

let markdownInstance = null;

/**
 * Minimal Mermaid plugin for markdown-it
 * Converts ```mermaid code blocks to <div class="mermaid">...</div>
 */
function mermaidPlugin(md) {
  const defaultFence = md.renderer.rules.fence || ((tokens, idx, options, env, self) => self.renderToken(tokens, idx, options));
  
  md.renderer.rules.fence = (tokens, idx, options, env, self) => {
    const token = tokens[idx];
    const lang = token.info.trim().split(/\s+/g)[0];
    
    if (lang === 'mermaid') {
      return `<div class="mermaid">${esc(token.content)}</div>\n`;
    }
    
    return defaultFence(tokens, idx, options, env, self);
  };
}

/**
 * Initialize markdown-it with GFM and Mermaid support
 * @private
 */
function initializeMarkdownIt() {
  if (markdownInstance) return;
  
  if (typeof markdownit === 'undefined') {
    console.warn('[MarkdownRenderer] markdown-it not loaded');
    return;
  }
  
  markdownInstance = window.markdownit({
    html: false,         // Disable HTML tags in source for security
    xhtmlOut: false,     // Use standard HTML output (not XHTML)
    breaks: true,        // Convert \n to <br> (GFM behavior)
    linkify: true,       // Auto-convert URLs to links
    typographer: true,   // Enable smartquotes and other typographic replacements
    highlight: function(str, lang) {
      // Basic syntax highlighting for code blocks
      // Note: Mermaid blocks are handled by the plugin, not this function
      if (lang && lang !== 'mermaid') {
        return `<pre><code class="language-${lang}">${esc(str)}</code></pre>`;
      }
      return `<pre><code>${esc(str)}</code></pre>`;
    }
  });
  
  markdownInstance.enable(['table', 'strikethrough', 'linkify']);
  markdownInstance.use(mermaidPlugin);
  
  if (typeof mermaid !== 'undefined') {
    mermaid.initialize({
      startOnLoad: false,
      theme: 'default',
      securityLevel: 'loose',
      flowchart: { useMaxWidth: true, htmlLabels: true },
      sequence: { useMaxWidth: true },
      gantt: { useMaxWidth: true },
    });
  }
}

/**
 * Render markdown text to HTML
 * @param {string} text - Markdown text
 * @param {Object} [options] - Options
 * @param {boolean} [options.inline=false] - Inline rendering
 * @returns {string} HTML string
 */
export function renderMarkdown(text, options = {}) {
  if (!text) return '';
  
  if (!markdownInstance) initializeMarkdownIt();
  if (!markdownInstance) return esc(text).replace(/\n/g, '<br>');
  
  try {
    return options.inline ? markdownInstance.renderInline(text) : markdownInstance.render(text);
  } catch (error) {
    console.error('[MarkdownRenderer] Error:', error);
    return esc(text).replace(/\n/g, '<br>');
  }
}

/**
 * Render Mermaid diagrams in container
 * @param {HTMLElement} [container=document.body] - Container element
 * @returns {Promise<void>}
 */
export async function initializeMermaid(container = document.body) {
  if (typeof mermaid === 'undefined') return;
  
  try {
    const nodes = container.querySelectorAll('.mermaid:not([data-processed="true"])');
    if (nodes.length > 0) {
      await mermaid.run({ nodes: Array.from(nodes) });
    }
  } catch (error) {
    console.error('[MarkdownRenderer] Mermaid error:', error);
  }
}

/**
 * Render markdown with Mermaid diagrams
 * @param {string} text - Markdown text
 * @param {HTMLElement} targetElement - Target element
 * @param {Object} [options] - Options
 * @returns {Promise<void>}
 */
export async function renderMarkdownWithMermaid(text, targetElement, options = {}) {
  if (!targetElement) return;
  
  targetElement.innerHTML = renderMarkdown(text, options);
  await initializeMermaid(targetElement);
}
