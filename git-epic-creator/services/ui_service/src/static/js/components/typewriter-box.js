/**
 * TypewriterBox Component
 * 
 * Creates a reusable thinking box with typewriter effect for agent messages.
 * Uses marked.js for markdown-to-HTML conversion and TypewriterJS for typing animation.
 * 
 * @module components/typewriter-box
 */

'use strict';

import { escapeHtml, scrollToBottom, smartScrollToBottom } from '../utils/dom-helpers.js';

/**
 * Creates a reusable thinking box with typewriter effect for agent messages.
 * Displays text character by character with smooth cursor effect.
 */
export class TypewriterBox {
  constructor(containerElement, promptId = null) {
    this.containerElement = containerElement;
    this.promptId = promptId;
    this.element = null;
    this.messagesContainer = null;
    this.summaryLabel = null;
    this.summaryIcon = null;
    this.startedAt = Date.now();
    this.messageQueue = [];
    this.isTyping = false;
    this.currentTypewriter = null;
    this.currentContainer = null;
    
    this._createElements();
    this._configureMarked();
  }
  
  _configureMarked() {
    // Configure marked.js for safe rendering with proper styling
    if (typeof marked !== 'undefined') {
      marked.setOptions({
        breaks: true,
        gfm: true,
        headerIds: false,
        mangle: false
      });
    }
  }
  
  _createElements() {
    // Create details/summary structure
    const wrap = document.createElement('details');
    wrap.className = 'thinking-box border border-indigo-200 rounded-lg p-4 bg-indigo-50/50 backdrop-blur-sm my-3';
    wrap.open = true;
    
    const summary = document.createElement('summary');
    summary.className = 'cursor-pointer select-none text-sm font-medium text-indigo-700 flex items-center gap-2';
    
    const icon = document.createElement('span');
    icon.className = 'thinking-indicator inline-block w-2 h-2 rounded-full bg-indigo-500 animate-pulse';
    
    const label = document.createElement('span');
    label.textContent = 'Agent thought stream';
    
    summary.appendChild(icon);
    summary.appendChild(label);
    
    const messages = document.createElement('div');
    messages.className = 'mt-3 space-y-1.5 text-sm text-slate-600 break-words overflow-hidden';
    
    // Create animated thinking footer
    const thinkingFooter = document.createElement('div');
    thinkingFooter.className = 'thinking-footer mt-3 pt-3 border-t border-indigo-200/50 flex items-center justify-center gap-2 opacity-70';
    
    const spinner = document.createElement('div');
    spinner.className = 'thinking-spinner';
    
    const thinkingText = document.createElement('span');
    thinkingText.className = 'text-xs text-indigo-600 font-medium';
    thinkingText.textContent = 'thinking...';
    
    thinkingFooter.appendChild(spinner);
    thinkingFooter.appendChild(thinkingText);
    
    wrap.appendChild(summary);
    wrap.appendChild(messages);
    wrap.appendChild(thinkingFooter);
    
    this.element = wrap;
    this.messagesContainer = messages;
    this.summaryLabel = label;
    this.summaryIcon = icon;
    this.thinkingFooter = thinkingFooter;
    
    this.containerElement.appendChild(wrap);
    scrollToBottom(this.containerElement);
  }
  
  setPromptId(newId) {
    if (newId) {
      this.promptId = newId;
    }
  }
  
  /**
   * Removes cursor from container
   * Handles both custom cursor class and Typewriter library's cursor
   */
  _removeCursor(container) {
    if (!container) return;
    
    // Try to find and remove custom cursor class inside container
    const customCursor = container.querySelector('.typewriter-cursor');
    if (customCursor) {
      customCursor.remove();
    }
    
    // Try to find and remove Typewriter library's cursor class inside container
    const libraryCursor = container.querySelector('.Typewriter__cursor');
    if (libraryCursor) {
      libraryCursor.remove();
    }
    
    // Check for cursor as next sibling (library sometimes adds it as sibling)
    if (container.nextSibling) {
      const nextEl = container.nextSibling;
      if (nextEl.nodeType === 1) { // Element node
        if (nextEl.classList && (nextEl.classList.contains('Typewriter__cursor') || nextEl.classList.contains('typewriter-cursor'))) {
          nextEl.remove();
        }
      }
    }
    
    // Comprehensive cleanup: find all cursors in parent container
    if (container.parentElement) {
      const allCursors = container.parentElement.querySelectorAll('.Typewriter__cursor, .typewriter-cursor');
      allCursors.forEach(cursor => {
        // Only remove if it's related to this container or has no content
        if (!cursor.previousSibling || cursor.previousSibling === container || cursor.textContent.trim() === '▎' || cursor.textContent.trim() === '|') {
          cursor.remove();
        }
      });
    }
  }
  
  /**
   * Appends markdown text with typewriter effect.
   * Uses marked.js to convert markdown to HTML, then TypewriterJS to animate.
   */
  appendMarkdown(md) {
    if (!md) return;
    
    // Create message container
    const messageDiv = document.createElement('div');
    messageDiv.className = 'typewriter-message break-words';
    this.messagesContainer.appendChild(messageDiv);
    
    // Add to queue and process
    this.messageQueue.push({
      text: String(md),
      container: messageDiv,
      isMarkdown: true
    });
    
    this._processQueue();
  }
  
  /**
   * Appends plain text stream with typewriter effect.
   */
  appendStream(text) {
    if (!text) return;
    
    const messageDiv = document.createElement('div');
    messageDiv.className = 'text-xs text-slate-500 break-words typewriter-message';
    this.messagesContainer.appendChild(messageDiv);
    
    this.messageQueue.push({
      text: String(text),
      container: messageDiv,
      isMarkdown: false
    });
    
    this._processQueue();
  }
  
  async _processQueue() {
    if (this.isTyping || this.messageQueue.length === 0) return;
    
    this.isTyping = true;
    
    while (this.messageQueue.length > 0) {
      const message = this.messageQueue.shift();
      await this._typeMessage(message);
    }
    
    this.isTyping = false;
  }
  
  async _typeMessage(message) {
    const { text, container, isMarkdown } = message;
    
    return new Promise((resolve) => {
      try {
        // Remove cursor from previous container
        if (this.currentContainer && this.currentContainer !== container) {
          this._removeCursor(this.currentContainer);
        }
        this.currentContainer = container;
        
        // Convert markdown to HTML if needed
        let htmlContent = text;
        if (isMarkdown) {
          if (typeof marked === 'undefined') {
            console.error('[TypewriterBox] marked.js library not loaded! Cannot render markdown.');
            container.innerHTML = `<div class="text-rose-600 text-xs">Error: Markdown renderer not available. Please check console.</div>`;
            scrollToBottom(this.containerElement);
            resolve();
            return;
          }
          // Use marked.js for full GFM markdown support
          htmlContent = marked.parseInline(text);
        }
        
        // Check if TypewriterJS is available
        if (typeof Typewriter === 'undefined') {
          // Fallback: display instantly if TypewriterJS not loaded
          console.warn('[TypewriterBox] TypewriterJS not loaded, displaying content instantly');
          container.innerHTML = htmlContent;
          scrollToBottom(this.containerElement);
          resolve();
          return;
        }
        
        // Initial scroll (smart scroll - only if user is near bottom)
        smartScrollToBottom(this.containerElement);
        
        // Periodically scroll during typing (smart scroll - respects user position)
        const scrollInterval = setInterval(() => {
          smartScrollToBottom(this.containerElement);
        }, 150);
        
        // Create typewriter instance with proper options including onComplete callback
        this.currentTypewriter = new Typewriter(container, {
          loop: false,
          delay: 0.1,
          cursor: '▎', // Custom cursor
          html: true
        });
        
        // Type the HTML content
        this.currentTypewriter
          .typeString(htmlContent)
          .callFunction(() => {
            // Clear scroll interval and remove cursor after typing completes
            clearInterval(scrollInterval);
            this._removeCursor(container);
            resolve();
          })
          .start();
          
      } catch (error) {
        console.error('[TypewriterBox] Error in _typeMessage:', error);
        // Emergency fallback: display escaped text
        container.innerHTML = `<div class="text-rose-600 text-xs">Error displaying message: ${escapeHtml(error.message)}</div>`;
        scrollToBottom(this.containerElement);
        resolve();
      }
    });
  }
  
  /**
   * Marks the thinking box as finished.
   * Immediately completes any ongoing typing animation and displays remaining text.
   * @param {string} status - 'ok' or 'error'
   */
  finish(status = 'ok') {
    // Complete any pending messages immediately without animation
    if (this.messageQueue.length > 0) {
      this.messageQueue.forEach(msg => {
        const { text, container, isMarkdown } = msg;
        if (isMarkdown && typeof marked !== 'undefined') {
          container.innerHTML = marked.parseInline(text);
        } else {
          container.textContent = text;
        }
      });
      this.messageQueue = [];
    }
    
    // Stop any active typewriter and ensure typing is marked as complete
    if (this.currentTypewriter) {
      this.currentTypewriter.stop();
    }
    this.isTyping = false;
    
    const seconds = Math.max(0, Math.round((Date.now() - this.startedAt) / 1000));
    this.summaryLabel.textContent = `Agent completed in ${seconds}s`;
    this.summaryIcon.className = status === 'ok' 
      ? 'inline-block w-2 h-2 rounded-full bg-emerald-500' 
      : 'inline-block w-2 h-2 rounded-full bg-rose-500';
    
    // Remove any remaining cursors
    if (this.currentContainer) {
      this._removeCursor(this.currentContainer);
    }
    // Also check all message containers for stray cursors
    const allContainers = this.messagesContainer.querySelectorAll('.typewriter-message');
    allContainers.forEach(container => this._removeCursor(container));
    
    // Hide thinking footer
    if (this.thinkingFooter) {
      this.thinkingFooter.style.display = 'none';
    }
    
    // Close the details box
    this.element.open = false;
  }
  
  /**
   * Returns the underlying DOM element.
   */
  getElement() {
    return this.element;
  }
}

