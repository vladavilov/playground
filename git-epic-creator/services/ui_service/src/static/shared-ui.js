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
// Markdown Rendering
// ============================================================================

export function renderMarkdown(md) {
  let html = escapeHtml(md || '');
  
  // 1. Code blocks (protect from other replacements)
  html = html.replace(/```([\s\S]*?)```/g, '<pre class="bg-slate-900 text-slate-100 p-3 rounded-lg overflow-auto text-xs font-mono mt-2 break-words"><code>$1</code></pre>');
  
  // 2. Headers (must be processed before other formatting)
  html = html
    .replace(/^####\s+(.*)$/gm, '<h4 class="text-sm font-semibold mt-2 text-slate-800 break-words">$1</h4>')
    .replace(/^###\s+(.*)$/gm, '<h3 class="text-base font-semibold mt-3 text-slate-800 break-words">$1</h3>')
    .replace(/^##\s+(.*)$/gm, '<h2 class="text-lg font-semibold mt-4 text-slate-800 break-words">$1</h2>')
    .replace(/^#\s+(.*)$/gm, '<h1 class="text-xl font-bold mt-5 text-slate-800 break-words">$1</h1>');
  
  // 3. Horizontal rules
  html = html.replace(/^(?:---+|\*\*\*+|___+)$/gm, '<hr class="my-4 border-t border-slate-300">');
  
  // 4. Blockquotes
  html = html.replace(/^>\s+(.*)$/gm, '<blockquote class="border-l-4 border-indigo-300 pl-3 py-1 text-slate-600 italic bg-indigo-50/30 break-words">$1</blockquote>');
  
  // 5. Lists - Ordered lists (must come before unordered to avoid conflicts)
  html = html.replace(/^\s*(\d+)\.\s+(.*)$/gm, '<li class="ml-6 list-decimal text-slate-700 break-words">$2</li>');
  
  // 6. Lists - Unordered lists
  html = html.replace(/^\s*[-*+]\s+(.*)$/gm, '<li class="ml-6 list-disc text-slate-700 break-words">$1</li>');
  
  // 7. Inline formatting - Bold and Italic (order matters: bold before italic)
  html = html
    .replace(/\*\*\*(.+?)\*\*\*/g, '<strong class="font-bold italic">$1</strong>') // Bold + Italic
    .replace(/___(.+?)___/g, '<strong class="font-bold italic">$1</strong>') // Bold + Italic
    .replace(/\*\*(.+?)\*\*/g, '<strong class="font-semibold">$1</strong>') // Bold
    .replace(/__(.+?)__/g, '<strong class="font-semibold">$1</strong>') // Bold
    .replace(/\*(.+?)\*/g, '<em class="italic">$1</em>') // Italic
    .replace(/_(.+?)_/g, '<em class="italic">$1</em>'); // Italic
  
  // 8. Strikethrough
  html = html.replace(/~~(.+?)~~/g, '<del class="line-through text-slate-500">$1</del>');
  
  // 9. Inline code
  html = html.replace(/`([^`]+)`/g, '<code class="bg-slate-100 px-1.5 py-0.5 rounded text-xs font-mono break-all">$1</code>');
  
  // 10. Links
  html = html.replace(/\[(.+?)\]\((https?:\/\/[^\s)]+)\)/g, '<a href="$2" target="_blank" rel="noopener noreferrer" class="text-sky-600 underline hover:text-sky-700 break-all">$1</a>');
  
  // 11. Wrap consecutive list items in ul/ol tags
  html = html.replace(/((?:<li class="ml-6 list-disc[\s\S]*?<\/li>\s*)+)/g, '<ul class="space-y-1 my-2">$1</ul>');
  html = html.replace(/((?:<li class="ml-6 list-decimal[\s\S]*?<\/li>\s*)+)/g, '<ol class="space-y-1 my-2">$1</ol>');
  
  // 12. Paragraph breaks and line breaks
  // Double newlines -> paragraph break
  html = html.replace(/\n\n+/g, '<br><br>');
  // Single newlines -> line break (but avoid breaking inside tags)
  html = html.replace(/(?<!\>)\n(?!\<)/g, '<br>');
  
  return html;
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

