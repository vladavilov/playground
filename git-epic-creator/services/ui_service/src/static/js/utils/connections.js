/**
 * Connection Management Utilities
 * 
 * Functions for managing authentication, GitLab, and SSE connections.
 * 
 * @module utils/connections
 */

'use strict';

/**
 * Updates the connections panel UI based on application state.
 * @param {Object} state - Application state object
 */
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

/**
 * Checks authentication status and updates UI.
 * Redirects to login if not authenticated.
 * @param {Object} state - Application state object
 * @returns {Promise<boolean>} True if authenticated, false otherwise
 */
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

/**
 * Handles user logout.
 * @param {Object} loadingManager - Loading manager instance
 */
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

/**
 * Fetches GitLab connection status.
 * @param {Object} state - Application state object
 * @param {Object} config - Configuration object
 */
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

/**
 * Starts GitLab SSO flow.
 * @param {Object} config - Configuration object
 */
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

/**
 * Connects to SSE event stream.
 * @param {Object} state - Application state object
 * @param {Object} eventHandlers - Map of event names to handler functions
 * @returns {EventSource} EventSource instance
 */
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

/**
 * Starts periodic GitLab status checking.
 * @param {Object} state - Application state object
 * @param {Object} config - Configuration object
 * @param {number} intervalMs - Check interval in milliseconds (default: 5 minutes)
 * @returns {number} Interval ID for manual cleanup if needed
 */
export function startPeriodicGitLabStatusCheck(state, config, intervalMs = 5 * 60 * 1000) {
  if (!state || !config) {
    console.warn('startPeriodicGitLabStatusCheck: state and config required');
    return null;
  }
  
  const intervalId = setInterval(() => {
    fetchGitLabStatus(state, config).catch(err => {
      console.error('Periodic GitLab status check failed:', err);
    });
  }, intervalMs);
  
  // Cleanup on page unload
  window.addEventListener('beforeunload', () => {
    clearInterval(intervalId);
  });
  
  console.log(`Started periodic GitLab status checks (every ${intervalMs / 1000 / 60} minutes)`);
  return intervalId;
}

/**
 * Fetches application configuration.
 * @returns {Promise<Object>} Configuration object
 */
export async function fetchConfig() {
  const res = await fetch('/config');
  return await res.json();
}

