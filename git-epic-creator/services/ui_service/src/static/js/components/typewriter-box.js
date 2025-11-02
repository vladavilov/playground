/**
 * TypewriterBox Component
 * 
 * Creates a reusable thinking box with typewriter effect for markdown messages.
 * 
 * @module components/typewriter-box
 */

'use strict';

import { scrollToBottom, smartScrollToBottom } from '../utils/dom-helpers.js';
import { renderMarkdown } from '../utils/markdown-renderer.js';

export class TypewriterBox {
  constructor(containerElement, promptId = null) {
    this.containerElement = containerElement;
    this.promptId = promptId;
    this.startedAt = Date.now();
    this.messageQueue = [];
    this.isTyping = false;
    this.currentTypewriter = null;
    this.currentContainer = null;
    this.currentMessage = null;
    
    this._createElements();
  }
  
  _createElements() {
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
    if (newId) this.promptId = newId;
  }
  
  _removeCursor(container) {
    if (!container) return;
    
    container.querySelectorAll('.Typewriter__cursor, .typewriter-cursor').forEach(c => c.remove());
    
    if (container.nextSibling?.nodeType === 1 && 
        container.nextSibling.classList?.contains('Typewriter__cursor')) {
      container.nextSibling.remove();
    }
    
    if (container.parentElement) {
      container.parentElement.querySelectorAll('.Typewriter__cursor, .typewriter-cursor').forEach(cursor => {
        const text = cursor.textContent.trim();
        if (!cursor.previousSibling || cursor.previousSibling === container || text === '▎' || text === '|') {
          cursor.remove();
        }
      });
    }
  }
  
  appendMarkdown(md) {
    if (!md) return;
    
    const messageDiv = document.createElement('div');
    messageDiv.className = 'typewriter-message break-words';
    this.messagesContainer.appendChild(messageDiv);
    
    this.messageQueue.push({ text: String(md), container: messageDiv });
    
    if (this.isTyping && this.currentTypewriter) {
      this._finishCurrentMessage();
    }
    
    this._processQueue();
  }
  
  _finishCurrentMessage() {
    if (!this.currentTypewriter || !this.currentContainer) return;
    
    this.currentTypewriter.stop();
    
    if (this.currentMessage) {
      this.currentMessage.container.innerHTML = renderMarkdown(this.currentMessage.text, { inline: true });
    }
    
    this._removeCursor(this.currentContainer);
    this.currentTypewriter = null;
    this.currentMessage = null;
    this.isTyping = false;
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
    const { text, container } = message;
    this.currentMessage = message;
    
    return new Promise((resolve) => {
      if (this.currentContainer && this.currentContainer !== container) {
        this._removeCursor(this.currentContainer);
      }
      this.currentContainer = container;
      
      const htmlContent = renderMarkdown(text, { inline: true });
      
      if (typeof Typewriter === 'undefined') {
        container.innerHTML = htmlContent;
        scrollToBottom(this.containerElement);
        this.currentMessage = null;
        resolve();
        return;
      }
      
      smartScrollToBottom(this.containerElement);
      
      const scrollInterval = setInterval(() => smartScrollToBottom(this.containerElement), 150);
      
      this.currentTypewriter = new Typewriter(container, {
        loop: false,
        delay: 0.001,
        natural: false,
        cursor: '▎',
        html: true
      });
      
      this.currentTypewriter
        .typeString(htmlContent)
        .callFunction(() => {
          clearInterval(scrollInterval);
          this._removeCursor(container);
          this.currentMessage = null;
          resolve();
        })
        .start();
    });
  }
  
  finish(status = 'ok') {
    if (this.currentMessage) {
      this.currentMessage.container.innerHTML = renderMarkdown(this.currentMessage.text, { inline: false });
      this.currentMessage = null;
    }
    
    this.messageQueue.forEach(msg => {
      msg.container.innerHTML = renderMarkdown(msg.text, { inline: true });
    });
    this.messageQueue = [];
    
    if (this.currentTypewriter) {
      this.currentTypewriter.stop();
    }
    this.isTyping = false;
    
    const seconds = Math.max(0, Math.round((Date.now() - this.startedAt) / 1000));
    this.summaryLabel.textContent = `Agent completed in ${seconds}s`;
    this.summaryIcon.className = status === 'ok' 
      ? 'inline-block w-2 h-2 rounded-full bg-emerald-500' 
      : 'inline-block w-2 h-2 rounded-full bg-rose-500';
    
    if (this.currentContainer) {
      this._removeCursor(this.currentContainer);
    }
    
    this.messagesContainer.querySelectorAll('.typewriter-message').forEach(c => this._removeCursor(c));
    
    if (this.thinkingFooter) {
      this.thinkingFooter.style.display = 'none';
    }
    
    this.element.open = false;
  }
  
  getElement() {
    return this.element;
  }
}

