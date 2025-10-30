/**
 * Chat UI Component
 * 
 * Provides consistent chat message rendering across screens.
 * Handles user messages, assistant messages, and system notifications.
 * 
 * @module components/chat-ui
 */

'use strict';

import { escapeHtml, scrollToBottom } from '../utils/dom-helpers.js';

/**
 * Provides consistent chat message rendering across screens.
 */
export class ChatUI {
  /**
   * @param {HTMLElement} containerElement - Container for chat messages
   */
  constructor(containerElement) {
    if (!containerElement) {
      throw new Error('ChatUI requires a container element');
    }
    this.container = containerElement;
  }
  
  /**
   * Appends a user message to the chat.
   * @param {string} text - Message text (will be HTML escaped)
   */
  appendUserMessage(text) {
    if (!text) return;
    
    const wrap = document.createElement('div');
    wrap.className = 'flex flex-col gap-1';
    wrap.innerHTML = `
      <div class="text-xs text-slate-500 font-medium">You</div>
      <div class="bg-white border border-slate-200 rounded-lg p-3 shadow-sm">
        <p class="text-sm text-slate-800">${escapeHtml(text)}</p>
      </div>
    `;
    this.container.appendChild(wrap);
    scrollToBottom(this.container);
  }
  
  /**
   * Appends an assistant message to the chat.
   * @param {string} html - HTML content (will NOT be escaped)
   * @param {string} meta - Meta label for the message (default: 'AI Agent')
   */
  appendAssistantMessage(html, meta = 'AI Agent') {
    if (!html) return;
    
    const wrap = document.createElement('div');
    wrap.className = 'flex flex-col gap-1';
    wrap.innerHTML = `
      <div class="text-xs text-slate-500 font-medium">${escapeHtml(meta)}</div>
      <div class="bg-gradient-to-br from-indigo-50 to-purple-50 border border-indigo-200 rounded-lg p-4 shadow-sm">
        ${html}
      </div>
    `;
    this.container.appendChild(wrap);
    scrollToBottom(this.container);
  }
  
  /**
   * Appends a system notification message.
   * @param {string} text - Notification text (will be HTML escaped)
   */
  appendSystemMessage(text) {
    if (!text) return;
    
    const wrap = document.createElement('div');
    wrap.className = 'flex items-center justify-center my-2';
    wrap.innerHTML = `
      <div class="bg-slate-100 border border-slate-200 rounded-full px-3 py-1 text-xs text-slate-600">
        ${escapeHtml(text)}
      </div>
    `;
    this.container.appendChild(wrap);
    scrollToBottom(this.container);
  }
}

