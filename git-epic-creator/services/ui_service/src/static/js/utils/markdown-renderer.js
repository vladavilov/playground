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
 * Render Mermaid diagrams in container with proper error handling
 * @param {HTMLElement} [container=document.body] - Container element
 * @returns {Promise<void>}
 */
export async function initializeMermaid(container = document.body) {
  if (typeof mermaid === 'undefined') return;
  
  const nodes = container.querySelectorAll('.mermaid:not([data-processed="true"])');
  
  // Process each diagram individually to handle failures gracefully
  for (const node of nodes) {
    try {
      // Add containment wrapper to prevent overflow
      node.style.maxWidth = '100%';
      node.style.overflowX = 'hidden';
      
      // Render the diagram
      await mermaid.run({ nodes: [node] });
      
      // Mark as processed after successful render
      node.setAttribute('data-processed', 'true');
      
      // Ensure SVG is contained
      const svg = node.querySelector('svg');
      if (svg) {
        svg.style.maxWidth = '100%';
        svg.style.height = 'auto';
      }
      
    } catch (error) {
      console.error('[MarkdownRenderer] Mermaid diagram failed to render:', error);
      
      // Mark as processed to prevent retry loops
      node.setAttribute('data-processed', 'true');
      
      // Store error details for AI fix
      const diagramSource = node.textContent.trim();
      const errorMessage = error.message || error.toString();
      const nodeId = `mermaid-error-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
      
      // Display user-friendly error message with AI fix button
      const errorHTML = `
        <div class="mermaid-error" style="padding: 1rem; background: #fee; border: 1px solid #fcc; border-radius: 0.5rem; color: #c33;" data-error-id="${nodeId}">
          <div style="display: flex; align-items: start; justify-content: space-between; gap: 1rem; margin-bottom: 0.5rem;">
            <div>
              <div style="font-weight: 600; margin-bottom: 0.25rem;">⚠ Diagram Error</div>
              <div style="font-size: 0.875rem;">Failed to render diagram. The syntax may be invalid.</div>
            </div>
            <button 
              class="mermaid-ai-fix-btn"
              data-error-id="${nodeId}"
              style="padding: 0.375rem 0.75rem; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border: none; border-radius: 0.375rem; font-size: 0.75rem; font-weight: 600; cursor: pointer; white-space: nowrap; transition: all 0.2s; box-shadow: 0 2px 4px rgba(102, 126, 234, 0.3);"
              onmouseover="this.style.transform='translateY(-1px)'; this.style.boxShadow='0 4px 8px rgba(102, 126, 234, 0.4)';"
              onmouseout="this.style.transform='translateY(0)'; this.style.boxShadow='0 2px 4px rgba(102, 126, 234, 0.3)';"
              onmousedown="this.style.transform='scale(0.98)';"
              onmouseup="this.style.transform='translateY(-1px)';"
              title="Use AI to automatically fix the diagram syntax">
              🤖 Fix with AI
            </button>
          </div>
          <details style="margin-top: 0.5rem; font-size: 0.75rem;">
            <summary style="cursor: pointer; color: #a22;">View diagram source & error details</summary>
            <div style="margin-top: 0.5rem;">
              <div style="font-weight: 600; margin-bottom: 0.25rem; color: #666;">Error Message:</div>
              <pre style="padding: 0.5rem; background: #fff8f8; border: 1px solid #fcc; border-radius: 0.25rem; overflow-x: auto; font-family: monospace; margin-bottom: 0.75rem; color: #c33;">${esc(errorMessage)}</pre>
              <div style="font-weight: 600; margin-bottom: 0.25rem; color: #666;">Diagram Source:</div>
              <pre style="padding: 0.5rem; background: white; border: 1px solid #ddd; border-radius: 0.25rem; overflow-x: auto; font-family: monospace;">${esc(diagramSource)}</pre>
            </div>
          </details>
        </div>
      `;
      node.innerHTML = errorHTML;
      node.style.overflow = 'hidden'; // Prevent any horizontal scroll
      
      // Store error data for the AI fix handler
      node.dataset.diagramSource = diagramSource;
      node.dataset.errorMessage = errorMessage;
      
      // Attach click handler to the AI fix button
      const fixBtn = node.querySelector('.mermaid-ai-fix-btn');
      if (fixBtn) {
        fixBtn.addEventListener('click', () => {
          handleMermaidAIFix(diagramSource, errorMessage, node);
        });
      }
    }
  }
}

/**
 * Handles AI-powered Mermaid diagram fix request.
 * Currently shows a placeholder message. Will be implemented to call AI service.
 * @param {string} diagramSource - Original Mermaid diagram source code
 * @param {string} errorMessage - Error message from Mermaid rendering
 * @param {HTMLElement} errorNode - The error container node
 * @private
 */
function handleMermaidAIFix(diagramSource, errorMessage, errorNode) {
  // Disable button to prevent multiple clicks
  const btn = errorNode.querySelector('.mermaid-ai-fix-btn');
  if (btn) {
    btn.disabled = true;
    btn.textContent = '⏳ Processing...';
    btn.style.opacity = '0.6';
    btn.style.cursor = 'not-allowed';
  }
  
  // Log the data that would be sent to AI service
  console.log('[MermaidAIFix] Request Data:', {
    diagramSource,
    errorMessage,
    timestamp: new Date().toISOString()
  });
  
  // Show placeholder notification
  setTimeout(() => {
    // Create notification element
    const notification = document.createElement('div');
    notification.style.cssText = `
      position: fixed;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      padding: 1.5rem 2rem;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      border-radius: 0.75rem;
      box-shadow: 0 10px 30px rgba(0, 0, 0, 0.3);
      z-index: 10000;
      max-width: 90%;
      text-align: center;
      animation: slideIn 0.3s ease-out;
    `;
    notification.innerHTML = `
      <div style="font-size: 2rem; margin-bottom: 0.5rem;">🤖</div>
      <div style="font-weight: 600; font-size: 1.125rem; margin-bottom: 0.5rem;">AI Diagram Fix</div>
      <div style="font-size: 0.875rem; opacity: 0.95; margin-bottom: 1rem;">This feature will be implemented soon!</div>
      <div style="font-size: 0.75rem; opacity: 0.8; line-height: 1.5;">
        The AI will analyze the diagram syntax, identify issues,<br>and provide corrected code automatically.
      </div>
    `;
    
    // Add animation keyframes
    const style = document.createElement('style');
    style.textContent = `
      @keyframes slideIn {
        from { transform: translate(-50%, -60%); opacity: 0; }
        to { transform: translate(-50%, -50%); opacity: 1; }
      }
    `;
    document.head.appendChild(style);
    
    document.body.appendChild(notification);
    
    // Remove after 3 seconds
    setTimeout(() => {
      notification.style.animation = 'slideIn 0.3s ease-out reverse';
      setTimeout(() => {
        document.body.removeChild(notification);
        document.head.removeChild(style);
      }, 300);
    }, 3000);
    
    // Re-enable button
    if (btn) {
      btn.disabled = false;
      btn.textContent = '🤖 Fix with AI';
      btn.style.opacity = '1';
      btn.style.cursor = 'pointer';
    }
  }, 500);
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
