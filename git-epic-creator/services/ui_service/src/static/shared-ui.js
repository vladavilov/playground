/**
 * Shared UI Utilities
 * Common functionality used across all screens in the application
 */

'use strict';

// ============================================================================
// Utility Functions
// ============================================================================

export function escapeHtml(s) {
  return String(s ?? '').replace(/[&<>]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));
}

export function scrollToBottom(element) {
  if (element) element.scrollTop = element.scrollHeight;
}

/**
 * Smart scroll: only scrolls to bottom if user is already near bottom (within 100px)
 * This prevents disrupting user's manual scrolling
 */
export function smartScrollToBottom(element, threshold = 100) {
  if (!element) return false;
  
  const isNearBottom = element.scrollHeight - element.scrollTop - element.clientHeight <= threshold;
  
  if (isNearBottom) {
    element.scrollTop = element.scrollHeight;
    return true;
  }
  return false;
}

export function formatDate(isoString) {
  if (!isoString) return 'N/A';
  try {
    const date = new Date(isoString);
    if (isNaN(date.getTime())) return 'N/A';
    const day = String(date.getDate()).padStart(2, '0');
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const year = date.getFullYear();
    return `${day}/${month}/${year}`;
  } catch {
    return 'N/A';
  }
}

// ============================================================================
// Status Badges
// ============================================================================

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

// ============================================================================
// Loading Overlay
// ============================================================================

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

// ============================================================================
// Connection Status Management
// ============================================================================

export function updateConnectionsPanel(state) {
  try {
    // Update auth indicator
    const authIndicator = document.getElementById('authIndicator');
    if (authIndicator) {
      if (state.authenticated) {
        authIndicator.className = 'w-2 h-2 rounded-full bg-emerald-500';
        authIndicator.title = 'Authenticated';
      } else {
        authIndicator.className = 'w-2 h-2 rounded-full bg-rose-500';
        authIndicator.title = 'Not authenticated';
      }
    }

    // Update GitLab indicator
    const gitlabIndicator = document.getElementById('gitlabIndicator');
    const gitlabConnectBtn = document.getElementById('gitlabConnectBtn');
    if (gitlabIndicator) {
      const s = (state.gitlab?.status || '').toLowerCase();
      let cls = 'w-2 h-2 rounded-full bg-slate-400';
      let title = 'GitLab status';
      
      if (s === 'connected') {
        cls = 'w-2 h-2 rounded-full bg-emerald-500';
        title = 'Connected to GitLab';
      } else if (s === 'connecting') {
        cls = 'w-2 h-2 rounded-full bg-blue-500 animate-pulse';
        title = 'Checking GitLab connection…';
      } else if (s === 'not_connected') {
        cls = 'w-2 h-2 rounded-full bg-rose-500';
        title = state.gitlab?.configured ? 'Not connected to GitLab' : 'GitLab SSO not configured';
      }
      
      gitlabIndicator.className = cls;
      gitlabIndicator.title = title;
    }
    
    // Update GitLab connect button state
    if (gitlabConnectBtn) {
      const disabled = !state.gitlab?.configured;
      const title = disabled ? 'GitLab SSO not configured' : 'Connect to GitLab';
      gitlabConnectBtn.disabled = disabled;
      gitlabConnectBtn.title = title;
    }

    // Update RT Events indicator
    const rtEventsIndicator = document.getElementById('rtEventsIndicator');
    if (rtEventsIndicator) {
      const s = (state.rtEvents?.status || '').toLowerCase();
      let cls = 'w-2 h-2 rounded-full bg-slate-400';
      let title = 'Real-time events';
      
      if (s === 'connected') {
        cls = 'w-2 h-2 rounded-full bg-emerald-500';
        title = 'Connected to real-time events';
      } else if (s === 'connecting') {
        cls = 'w-2 h-2 rounded-full bg-blue-500 animate-pulse';
        title = 'Connecting to real-time events…';
      } else {
        cls = 'w-2 h-2 rounded-full bg-slate-400';
        title = 'Disconnected from real-time events';
      }
      
      rtEventsIndicator.className = cls;
      rtEventsIndicator.title = title;
    }
  } catch (err) {
    console.error('Failed to update connections panel:', err);
  }
}

// ============================================================================
// Authentication
// ============================================================================

export async function checkAuthStatus(state) {
  try {
    const res = await fetch('/auth/me');
    const data = await res.json();
    state.authenticated = data.authenticated || false;
    
    // Update UI
    const profile = document.getElementById('userProfile');
    const avatar = document.getElementById('userAvatar');
    if (profile) {
      const username = data.username || 'User';
      profile.textContent = username;
      profile.title = `Logged in as ${username}`;
    }
    if (avatar) {
      avatar.textContent = (data.username || 'U')[0].toUpperCase();
    }
    
    updateConnectionsPanel(state);
    
    if (!state.authenticated) {
      // Redirect to login with current URL to return after auth
      const returnTo = encodeURIComponent(window.location.pathname + window.location.search + window.location.hash);
      window.location.href = `/auth/login?redirect_uri=${returnTo}`;
      return false;
    }
    return true;
  } catch (e) {
    console.error('Auth check failed:', e);
    // Redirect to login with current URL to return after auth
    const returnTo = encodeURIComponent(window.location.pathname + window.location.search + window.location.hash);
    window.location.href = `/auth/login?redirect_uri=${returnTo}`;
    return false;
  }
}

export async function handleLogout(loadingManager) {
  try {
    loadingManager.show();
    await fetch('/auth/logout', { method: 'POST' });
    window.location.href = '/auth/login';
  } catch (err) {
    console.error('Logout failed:', err);
    alert('Logout failed. Please try again.');
  } finally {
    loadingManager.hide();
  }
}

// ============================================================================
// GitLab Integration
// ============================================================================

export async function fetchGitLabStatus(state, config) {
  try {
    if (!state.gitlab) state.gitlab = { status: 'connecting', configured: false };
    state.gitlab.status = 'connecting';
    updateConnectionsPanel(state);
    
    const statusPath = config.gitlabAuthStatusPath || '/auth/gitlab/status';
    const res = await fetch(statusPath);
    if (!res.ok) throw new Error('status failed');
    
    const data = await res.json();
    state.gitlab.configured = Boolean(data && data.configured);
    state.gitlab.status = data && data.connected ? 'connected' : 'not_connected';
    console.log('GitLab status fetched:', { configured: state.gitlab.configured, status: state.gitlab.status });
  } catch (err) {
    console.error('Failed to fetch GitLab status:', err);
    state.gitlab.status = 'not_connected';
    state.gitlab.configured = false;
  } finally {
    updateConnectionsPanel(state);
  }
}

export function startGitLabSSO(config) {
  try {
    const returnTo = window.location.href;
    const authorizePath = config.gitlabAuthAuthorizePath || '/auth/gitlab/authorize';
    const url = `${authorizePath}?redirect_uri=${encodeURIComponent(returnTo)}`;
    window.location.href = url;
  } catch (err) {
    console.error('Failed to start GitLab SSO:', err);
  }
}

// ============================================================================
// SSE Connection
// ============================================================================

export function connectSSE(state, eventHandlers = {}) {
  try {
    if (state.evtSource) state.evtSource.close();
  } catch (e) {}
  
  state.evtSource = new EventSource('/events');
  
  state.evtSource.addEventListener('open', () => {
    if (!state.rtEvents) state.rtEvents = {};
    state.rtEvents.status = 'connected';
    updateConnectionsPanel(state);
  });
  
  state.evtSource.addEventListener('error', () => {
    if (!state.rtEvents) state.rtEvents = {};
    state.rtEvents.status = 'connecting';
    updateConnectionsPanel(state);
  });
  
  state.evtSource.addEventListener('hello', () => {
    if (!state.rtEvents) state.rtEvents = {};
    state.rtEvents.status = 'connected';
    updateConnectionsPanel(state);
  });
  
  // Register custom event handlers
  Object.entries(eventHandlers).forEach(([eventName, handler]) => {
    state.evtSource.addEventListener(eventName, handler);
  });
  
  return state.evtSource;
}

// ============================================================================
// Configuration
// ============================================================================

export async function fetchConfig() {
  const res = await fetch('/config');
  return await res.json();
}

// ============================================================================
// TypewriterBox Component (Using marked.js + TypewriterJS)
// ============================================================================

/**
 * Creates a reusable thinking box with typewriter effect for agent messages.
 * Uses marked.js for markdown-to-HTML conversion and TypewriterJS for typing animation.
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
          html: true,
          onComplete: () => {
            // Clear scroll interval
            clearInterval(scrollInterval);
            
            // Remove cursor after typing completes using proper API callback
            this._removeCursor(container);
            
            resolve();
          }
        });
        
        // Type the HTML content
        this.currentTypewriter
          .typeString(htmlContent)
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
   * @param {string} status - 'ok' or 'error'
   */
  finish(status = 'ok') {
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
    
    // Stop any active typewriter
    if (this.currentTypewriter) {
      this.currentTypewriter.stop();
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

