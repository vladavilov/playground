'use strict';

import { GitLabBacklogProjectsManager } from '../helpers/gitlab-projects-manager.js';
import { ApiClient, ApiError } from '../services/api-client.js';

const state = {
  config: null,
  projects: [],
  selected: null,
  evtSource: null,
  loadingCount: 0,
  editTargetId: null,
  filterText: '',
  gitlab: { status: 'connecting', configured: false },
  authenticated: false,
  rtEvents: { status: 'disconnected' },
  cachingEmbeddings: false,
  gitlabBacklogProjectsManager: new GitLabBacklogProjectsManager(),
};

async function loadConfig() {
  const res = await fetch('/config');
  state.config = await res.json();
}

/**
 * Wrapper around ApiClient to handle loading states and base URL.
 */
class ProjectApiClient {
  constructor(getConfigFn, on401Handler) {
    this.getConfig = getConfigFn;
    this.apiClient = new ApiClient(null, on401Handler);
  }

  _getBaseUrl() {
    const cfg = this.getConfig();
    if (!cfg || !cfg.projectManagementApiBase) {
      throw new Error('API base not configured');
    }
    return cfg.projectManagementApiBase.replace(/\/$/, '');
  }

  async _wrapRequest(fn) {
    showLoading();
    try {
      return await fn();
    } finally {
      hideLoading();
    }
  }

  async get(path) {
    return this._wrapRequest(() => 
      this.apiClient.get(`${this._getBaseUrl()}${path}`)
    );
  }

  async post(path, body) {
    return this._wrapRequest(() => 
      this.apiClient.post(`${this._getBaseUrl()}${path}`, body)
    );
  }

  async put(path, body) {
    return this._wrapRequest(() => 
      this.apiClient.put(`${this._getBaseUrl()}${path}`, body)
    );
  }

  async delete(path) {
    return this._wrapRequest(() => 
      this.apiClient.delete(`${this._getBaseUrl()}${path}`)
    );
  }

  async postForm(path, formData) {
    return this._wrapRequest(async () => {
      const url = `${this._getBaseUrl()}${path}`;
      const response = await fetch(url, {
        method: 'POST',
        body: formData  // Don't set Content-Type for FormData
      });

      if (response.status === 401) {
        if (this.apiClient.on401Handler) {
          this.apiClient.on401Handler();
        }
        throw new ApiError('Unauthorized', response.status, response);
      }

      if (!response.ok) {
        const detail = await response.text().catch(() => 'Upload failed');
        throw new ApiError(detail, response.status, response);
      }

      // Handle No Content responses
      if (response.status === 204 || response.status === 205) return null;
      
      const contentType = response.headers.get('content-type') || '';
      if (contentType.includes('application/json')) {
        return response.json();
      }
      return response.text();
    });
  }
}

let api = null;

function el(tag, attrs = {}, children = []) {
  const node = document.createElement(tag);
  Object.entries(attrs).forEach(([k, v]) => {
    if (k === 'class') node.className = v;
    else if (k.startsWith('on') && typeof v === 'function') node.addEventListener(k.substring(2), v);
    else if (v === null || v === undefined || v === false) {
      // Skip null, undefined, or false values (don't set attribute)
      return;
    }
    else if (v === true) {
      // For true boolean values, set attribute without value (e.g., disabled, checked)
      node.setAttribute(k, '');
    }
    else node.setAttribute(k, v);
  });
  for (const child of [].concat(children)) {
    if (child == null) continue;
    node.append(typeof child === 'string' ? document.createTextNode(child) : child);
  }
  return node;
}

function formatDate(isoString) {
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

function renderProjects() {
  const list = document.getElementById('projectsList');
  list.innerHTML = '';
  const q = (state.filterText || '').toLowerCase();
  const filtered = state.projects.filter(p => (p.name || '').toLowerCase().includes(q));
  
  if (filtered.length === 0) {
    const emptyMsg = el('div', { class: 'text-center text-sm text-slate-500 py-8' }, 
      q ? 'No projects match your search' : 'No projects yet. Click "Create" to get started.'
    );
    list.appendChild(emptyMsg);
    return;
  }
  
  filtered.forEach(p => {
    const isSelected = state.selected && state.selected.id === p.id;
    const item = el('li', { 
      class: 'border rounded p-3 cursor-pointer transition-colors ' + 
             (isSelected 
               ? 'bg-indigo-50 border-indigo-300 ring-1 ring-indigo-300' 
               : 'hover:bg-slate-50 border-slate-200'),
      onclick: () => selectProject(p), 
      role: 'button', 
      tabindex: '0',
      onkeypress: (e) => { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); selectProject(p); } }
    }, [
      el('div', { class: 'flex items-start justify-between gap-2' }, [
        el('div', { class: 'flex-1 min-w-0' }, [
          el('div', { class: 'font-semibold truncate ' + (isSelected ? 'text-indigo-900' : 'text-slate-900') }, p.name || '(untitled)'),
          p.description ? el('div', { class: 'text-sm mt-1 truncate ' + (isSelected ? 'text-indigo-700' : 'text-slate-600') }, p.description) : null,
        ]),
        el('div', { class: 'mt-0.5 flex-shrink-0' }, [getStatusBadge(p.status)])
      ])
    ]);
    list.appendChild(item);
  });
}

function renderDetails() {
  const box = document.getElementById('projectDetails');
  const upload = document.getElementById('uploadSection');
  const chat = document.getElementById('chatSection');
  if (!state.selected) {
    box.classList.remove('hidden');
    upload.classList.add('hidden');
    if (chat) chat.classList.add('hidden');
    // Show helpful empty state
    box.innerHTML = '';
    box.append(
      el('div', { class: 'flex flex-col items-center justify-center h-full text-center py-12 px-6' }, [
        el('div', { class: 'w-16 h-16 rounded-full bg-slate-100 flex items-center justify-center text-slate-400 text-3xl mb-4' }, '📁'),
        el('h3', { class: 'text-lg font-semibold text-slate-900 mb-2' }, 'No project selected'),
        el('p', { class: 'text-sm text-slate-600 max-w-sm' }, 
          state.projects.length === 0 
            ? 'Create your first project to get started with document processing and AI-powered analysis.'
            : 'Select a project from the sidebar to view details, upload documents, and manage requirements.'
        )
      ])
    );
    return;
  }
  box.classList.remove('hidden');
  upload.classList.remove('hidden');
  if (chat) chat.classList.remove('hidden');
  box.innerHTML = '';
  const p = state.selected;
  box.append(
    el('div', { class: 'flex items-center gap-2 flex-wrap' }, [
      el('div', { class: 'text-lg font-semibold' }, p.name || '(untitled)'),
      getStatusBadge(p.status)
    ]),
    p.description ? el('div', { class: 'mt-2 text-slate-700' }, p.description) : null,
    el('div', { class: 'mt-3 grid grid-cols-2 gap-x-4 gap-y-1 text-sm text-slate-600' }, [
      el('div', {}, [
        el('span', { class: 'font-medium' }, 'Created: '),
        formatDate(p.created_at)
      ]),
      el('div', {}, [
        el('span', { class: 'font-medium' }, 'Updated: '),
        formatDate(p.updated_at)
      ])
    ]),
    el('div', { class: 'mt-3' }, [
      renderGitLabInfo(p)
    ]),
    el('div', { class: 'mt-4 flex items-center justify-between flex-wrap gap-2' }, [
      el('div', { class: 'flex items-center gap-2 flex-wrap' }, [
        el('button', { id: 'openRequirementsBtn', class: 'px-3 py-1.5 border rounded text-sm text-slate-700 hover:bg-slate-50', title: 'Capture requirements', onclick: () => { if (!state.selected) return; window.location.href = `/pages/requirements.html?project_id=${state.selected.id}`; } }, 'Requirements'),
        el('button', { id: 'openTasksBtn', class: 'px-3 py-1.5 border rounded text-sm text-slate-700 hover:bg-slate-50', title: 'Open tasks screen', onclick: () => { if (!state.selected) return; window.location.href = `/pages/tasks.html?project_id=${state.selected.id}`; } }, 'Tasks'),
      ]),
      el('div', { class: 'flex items-center gap-2 flex-wrap' }, [
        el('button', { class: 'px-3 py-1.5 border rounded text-sm text-slate-700 hover:bg-slate-50', onclick: () => openProjectModal(p), 'aria-label': 'Edit project' }, 'Edit'),
        el('button', { class: 'px-3 py-1.5 bg-rose-600 text-white rounded text-sm hover:bg-rose-700', onclick: () => openDeleteModal(p), 'aria-label': 'Delete project' }, 'Delete')
      ])
    ])
  );
}

async function fetchProjects() {
  const projects = await api.get('/projects');
  state.projects = projects;
  renderProjects();
}

// UI helpers
function renderGitLabInfo(project) {
  const repoSection = el('div', { class: 'mb-3' }, [
    el('div', { class: 'text-xs font-semibold text-slate-600 mb-1' }, 'Repository (Source Code)'),
    project.gitlab_repository_url 
      ? el('a', { 
          href: project.gitlab_repository_url, 
          target: '_blank',
          class: 'text-sm text-indigo-600 hover:underline flex items-center gap-1'
        }, [
          '🔗 ',
          project.gitlab_repository_url
        ])
      : el('div', { class: 'text-sm text-slate-400 italic' }, 'No repository URL')
  ]);
  
  const backlogSection = renderGitLabBacklogProjects(
    project.gitlab_backlog_project_ids,
    project.gitlab_backlog_project_urls
  );
  
  return el('div', {}, [repoSection, backlogSection]);
}

function renderGitLabBacklogProjects(gitlabProjectIds, gitlabProjectUrls) {
  // Header with cache embeddings button as a tag
  const header = el('div', { class: 'flex items-center gap-2 mb-1' }, [
    el('div', { class: 'text-xs font-semibold text-slate-600' }, 'Backlog Projects (Issues/Epics)'),
    gitlabProjectIds && gitlabProjectIds.length > 0 ? el('button', {
      class: 'inline-flex items-center gap-1 px-2 py-0.5 rounded text-xs font-medium transition-colors ' + 
             (state.cachingEmbeddings 
               ? 'bg-blue-100 text-blue-700 cursor-wait' 
               : 'bg-indigo-100 text-indigo-700 hover:bg-indigo-200'),
      disabled: state.cachingEmbeddings,
      onclick: () => handleCacheEmbeddings(),
      title: state.cachingEmbeddings 
        ? 'Caching embeddings in progress...' 
        : 'Cache embeddings for all linked backlog projects to enable AI-powered search and retrieval'
    }, [
      el('svg', { 
        class: 'w-3 h-3' + (state.cachingEmbeddings ? ' animate-spin' : ''),
        fill: 'none',
        stroke: 'currentColor',
        viewBox: '0 0 24 24',
        innerHTML: '<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 10V3L4 14h7v7l9-11h-7z"></path>'
      }),
      state.cachingEmbeddings ? 'Caching...' : 'Cache'
    ]) : null
  ]);
  
  if (!gitlabProjectIds || gitlabProjectIds.length === 0) {
    return el('div', {}, [
      header,
      el('div', { class: 'text-sm text-slate-400 italic' }, 'No backlog projects linked')
    ]);
  }
  
  return el('div', { class: 'space-y-2' }, [
    header,
    el('div', { class: 'grid gap-2' }, 
      gitlabProjectIds.map((id, idx) => {
        const url = gitlabProjectUrls[idx];
        const path = extractPathFromUrl(url);
        
        return el('div', { 
          class: 'flex items-center justify-between p-2 bg-slate-50 border border-slate-200 rounded'
        }, [
          el('div', { class: 'flex items-center gap-2 flex-1' }, [
            el('div', { class: 'w-4 h-4 rounded bg-orange-500 flex items-center justify-center text-white text-xs font-bold' }, 'G'),
            el('a', { 
              href: url, 
              target: '_blank',
              class: 'text-sm text-sky-600 hover:underline',
              title: `ID: ${id}`
            }, path)
          ])
        ]);
      })
    )
  ]);
}

function extractPathFromUrl(url) {
  try {
    const parsed = new URL(url);
    return parsed.pathname.replace(/^\//, '').replace(/\.git$/, '');
  } catch {
    return url.replace(/\.git$/, '');
  }
}


function getStatusBadge(status) {
  const s = (status || '').toLowerCase();
  const base = 'inline-flex items-center px-2 py-0.5 rounded text-xs font-medium ';
  // Use a consistent ramp: processing (blue) -> active (emerald) -> rag_processing (teal) -> rag_ready (green)
  let cls = 'bg-slate-100 text-slate-700';
  if (s === 'processing') cls = 'bg-blue-100 text-blue-700';
  else if (s === 'active') cls = 'bg-emerald-100 text-emerald-700';
  else if (s === 'rag_processing') cls = 'bg-teal-100 text-teal-700';
  else if (s === 'rag_ready') cls = 'bg-green-100 text-green-700';
  else if (s.includes('failed') || s === 'rag_failed') cls = 'bg-rose-100 text-rose-700';
  return el('span', { class: base + cls }, s || 'unknown');
}

// Strict helpers for ProjectProgressMessage payload
function getMsgProjectId(msg) { return msg?.project_id ?? null; }
function getMsgStatus(msg) { return msg?.status ?? null; }
function getMsgProcessedPct(msg) { return Number.isFinite(msg?.processed_pct) ? msg.processed_pct : null; }

// Update project state with incoming progress event and indicate if UI should re-render
function updateProjectStateFromProgress(msg) {
  try {
    const pid = getMsgProjectId(msg);
    if (pid == null) return false;
    const idStr = String(pid);
    const rawStatus = getMsgStatus(msg);
    const nextStatus = (rawStatus || '').toLowerCase();
    const pctVal = getMsgProcessedPct(msg);
    let changed = false;

    // Update item in projects list (mutate in place to preserve references)
    // Match against gitlab_backlog_project_ids since messages contain GitLab project IDs, not UUIDs
    for (const project of state.projects) {
      const gitlabIds = project.gitlab_backlog_project_ids || [];
      if (!gitlabIds.includes(idStr)) continue;
      if (nextStatus && nextStatus !== (project.status || '').toLowerCase()) {
        project.status = nextStatus;
        changed = true;
      }
      if (pctVal != null && pctVal !== project.processed_pct) {
        project.processed_pct = pctVal;
        changed = true;
      }
      if (msg.timestamp) {
        const ts = new Date(msg.timestamp);
        if (!isNaN(ts.getTime())) {
          const iso = ts.toISOString();
          if (project.updated_at !== iso) {
            project.updated_at = iso;
            changed = true;
          }
        }
      }
      break;
    }

    // Ensure selected reference reflects latest values too
    if (state.selected) {
      const selectedGitlabIds = state.selected.gitlab_backlog_project_ids || [];
      if (selectedGitlabIds.includes(idStr)) {
        if (nextStatus && nextStatus !== (state.selected.status || '').toLowerCase()) {
          state.selected.status = nextStatus;
          changed = true;
        }
        if (pctVal != null && pctVal !== state.selected.processed_pct) {
          state.selected.processed_pct = pctVal;
          changed = true;
        }
        if (msg.timestamp) {
          const ts = new Date(msg.timestamp);
          if (!isNaN(ts.getTime())) {
            const iso = ts.toISOString();
            if (state.selected.updated_at !== iso) {
              state.selected.updated_at = iso;
              changed = true;
            }
          }
        }
      }
    }

    return changed;
  } catch {
    return false;
  }
}

async function createProject() { openProjectModal(null); }

async function openEdit(p) { openProjectModal(p); }

async function removeProject(p, options = { skipConfirm: false }) {
  if (!options || options.skipConfirm !== true) {
    const ok = confirm('Delete project?');
    if (!ok) return;
  }
  try {
    await api.delete(`/projects/${p.id}`);
  } catch (e) { alert('Delete failed'); return; }
  if (state.selected && state.selected.id === p.id) state.selected = null;
  await fetchProjects();
  renderDetails();
}

function selectProject(p) {
  state.selected = p;
  renderDetails();
  // Reset upload pane progress visuals on project switch
  try {
    const text = document.getElementById('progressText');
    const bar = document.getElementById('progressBar');
    const pct = document.getElementById('progressPct');
    const input = document.getElementById('fileInput');
    const badgeHost = document.getElementById('statusBadge');
    const log = document.getElementById('progressLog');
    if (text) text.textContent = 'No processing yet';
    if (bar) bar.style.width = '0%';
    if (pct) pct.textContent = '0%';
    if (input) input.value = '';
    if (badgeHost) { badgeHost.innerHTML = ''; }
    if (log) log.innerHTML = '';
  } catch {}
}

async function uploadFiles() {
  if (!state.selected) return;
  const files = document.getElementById('fileInput').files;
  if (!files || files.length === 0) return;
  const form = new FormData();
  for (const f of files) form.append('files', f);
  try {
    // Reset progress UI and log at the start of upload
    const text = document.getElementById('progressText');
    const bar = document.getElementById('progressBar');
    const pct = document.getElementById('progressPct');
    const badgeHost = document.getElementById('statusBadge');
    const log = document.getElementById('progressLog');
    if (text) text.textContent = 'Uploading files…';
    if (bar) bar.style.width = '0%';
    if (pct) pct.textContent = '0%';
    if (badgeHost) badgeHost.innerHTML = '';
    if (log) log.innerHTML = '';

    await api.postForm(`/projects/${state.selected.id}/documents/upload`, form);
    // Reset input after successful upload
    const input = document.getElementById('fileInput');
    if (input) input.value = '';
    const fileCount = document.getElementById('fileCount');
    const selectedFiles = document.getElementById('selectedFiles');
    if (fileCount) fileCount.textContent = 'No files selected';
    if (selectedFiles) { selectedFiles.textContent = ''; selectedFiles.classList.add('hidden'); }
  } catch (e) { alert('Upload failed'); return; }
}

async function handleCacheEmbeddings() {
  if (!state.selected || state.cachingEmbeddings) return;
  
  const projectIds = state.selected.gitlab_backlog_project_ids;
  if (!projectIds || projectIds.length === 0) {
    alert('No GitLab backlog projects linked');
    return;
  }
  
  try {
    state.cachingEmbeddings = true;
    renderDetails();
    
    const cfg = state.config || {};
    const gitlabApiBase = (cfg.gitlabApiBase || '').replace(/\/$/, '');
    if (!gitlabApiBase) {
      throw new Error('GitLab API base not configured');
    }
    
    // Use ApiClient for consistent authentication via proxy
    const gitlabApiClient = new ApiClient(null, async () => {
      await fetchGitLabStatus();
    });
    
    const url = `${gitlabApiBase}/projects/multi/cache-embeddings?project_ids=${projectIds.join(',')}`;
    await gitlabApiClient.post(url, {});
    
    const log = document.getElementById('progressLog');
    if (log) {
      const ts = new Date().toLocaleTimeString();
      const line = document.createElement('div');
      line.textContent = `${ts} | Caching embeddings for ${projectIds.length} project(s)`;
      line.className = 'text-emerald-600';
      log.appendChild(line);
      log.scrollTop = log.scrollHeight;
    }
  } catch (err) {
    console.error('Failed to cache embeddings:', err);
    const message = ApiClient.formatError(err);
    alert(`Failed to cache embeddings: ${message}`);
  } finally {
    state.cachingEmbeddings = false;
    renderDetails();
  }
}

function connectSSE() {
  // Import shared SSE connection would go here if this was a module
  // For now, replicate the shared connection logic
  try { if (state.evtSource) state.evtSource.close(); } catch {}
  state.evtSource = new EventSource('/events');
  
  state.evtSource.addEventListener('open', () => {
    state.rtEvents.status = 'connected';
    updateConnectionsPanel();
  });
  
  state.evtSource.addEventListener('error', () => {
    state.rtEvents.status = 'connecting';
    updateConnectionsPanel();
  });
  
  state.evtSource.addEventListener('hello', () => {
    state.rtEvents.status = 'connected';
    updateConnectionsPanel();
  });
  
  const handleProjectProgress = (msg) => {
    try {
      const changed = updateProjectStateFromProgress(msg);
      if (changed) { renderProjects(); }
      const pid = getMsgProjectId(msg);
      // Match against gitlab_backlog_project_ids since messages contain GitLab project IDs, not UUIDs
      const gitlabIds = state.selected?.gitlab_backlog_project_ids || [];
      const isSelected = !!state.selected && pid != null && gitlabIds.includes(String(pid));
      if (!isSelected) return;
      const text = document.getElementById('progressText');
      const bar = document.getElementById('progressBar');
      const badgeHost = document.getElementById('statusBadge');
      const logHost = document.getElementById('progressLog');
      const pct = document.getElementById('progressPct');
      const statusForBadge = getMsgStatus(msg);
      if (badgeHost) { badgeHost.innerHTML = ''; badgeHost.appendChild(getStatusBadge(statusForBadge)); }
      const stepLabel = (msg.process_step && String(msg.process_step).trim() !== '') ? String(msg.process_step) : (statusForBadge || 'unknown');
      const pctVal = getMsgProcessedPct(msg);
      const safePct = (pctVal != null ? Math.max(0, Math.min(100, Math.round(pctVal))) : null);
      if (text) { text.textContent = stepLabel; }
      if (bar && safePct != null) { bar.style.width = `${safePct}%`; }
      if (pct && safePct != null) { pct.textContent = `${safePct}%`; }
      if (logHost) {
        const ts = (msg.timestamp ? new Date(msg.timestamp) : new Date());
        const tsStr = isNaN(ts.getTime()) ? new Date().toLocaleTimeString() : ts.toLocaleTimeString();
        const line = document.createElement('div');
        line.textContent = `${tsStr} | ${stepLabel}`;
        logHost.appendChild(line);
        while (logHost.children.length > 200) { logHost.removeChild(logHost.firstChild); }
        logHost.scrollTop = logHost.scrollHeight;
      }
      if (changed) { renderDetails(); }
    } catch {}
  };

  state.evtSource.addEventListener('project_progress', (evt) => {
    try { handleProjectProgress(JSON.parse(evt.data)); } catch {}
  });
}

function setupPanelToggle() {
  const panel = document.getElementById('projectsPanel');
  const openBtn = document.getElementById('openPanel');
  const hideBtn = document.getElementById('togglePanel');
  
  openBtn.addEventListener('click', () => {
    panel.classList.remove('hidden');
    // On mobile, show panel as overlay
    if (window.innerWidth < 1024) {
      panel.style.position = 'fixed';
      panel.style.top = '0';
      panel.style.left = '0';
      panel.style.bottom = '0';
      panel.style.zIndex = '40';
      panel.classList.add('shadow-2xl');
      hideBtn.classList.remove('lg:hidden');
    }
  });
  
  const hidePanel = () => {
    panel.classList.add('hidden');
    // Reset mobile overlay styles
    if (window.innerWidth < 1024) {
      panel.style.position = '';
      panel.style.top = '';
      panel.style.left = '';
      panel.style.bottom = '';
      panel.style.zIndex = '';
      panel.classList.remove('shadow-2xl');
      hideBtn.classList.add('lg:hidden');
    }
  };
  
  if (hideBtn) hideBtn.addEventListener('click', hidePanel);
  
  // Close mobile panel when clicking outside (on project select)
  const list = document.getElementById('projectsList');
  if (list) {
    list.addEventListener('click', () => {
      if (window.innerWidth < 1024) {
        hidePanel();
      }
    });
  }
}

function setupActions() {
  document.getElementById('createProject').addEventListener('click', createProject);
  const search = document.getElementById('projectName');
  if (search) search.addEventListener('input', (e) => { state.filterText = e.target.value || ''; renderProjects(); });
  const uploadBtn = document.getElementById('uploadBtn');
  if (uploadBtn) uploadBtn.addEventListener('click', uploadFiles);
  
  // Logout button
  const logoutBtn = document.getElementById('logoutBtn');
  if (logoutBtn) logoutBtn.addEventListener('click', handleLogout);

  // GitLab connect button
  const gitlabConnectBtn = document.getElementById('gitlabConnectBtn');
  if (gitlabConnectBtn) gitlabConnectBtn.addEventListener('click', () => {
    if (state.gitlab.configured) startGitLabSSO();
  });

  // Dropzone wiring
  const dropzone = document.getElementById('dropzone');
  const fileInput = document.getElementById('fileInput');
  const fileCount = document.getElementById('fileCount');
  const selectedFiles = document.getElementById('selectedFiles');
  const updateSelected = () => {
    if (!fileInput || !fileCount || !selectedFiles) return;
    const files = Array.from(fileInput.files || []);
    const uploadBtn = document.getElementById('uploadBtn');
    if (files.length === 0) {
      fileCount.textContent = 'No files selected';
      selectedFiles.classList.add('hidden');
      selectedFiles.textContent = '';
      if (uploadBtn) uploadBtn.disabled = true;
      return;
    }
    fileCount.textContent = `${files.length} file${files.length > 1 ? 's' : ''} selected`;
    selectedFiles.classList.remove('hidden');
    selectedFiles.textContent = files.map(f => `• ${f.name}`).join(', ');
    if (uploadBtn) uploadBtn.disabled = false;
  };
  if (fileInput) fileInput.addEventListener('change', updateSelected);
  if (dropzone && fileInput) {
    dropzone.addEventListener('click', () => fileInput.click());
    dropzone.addEventListener('dragover', (e) => { 
      e.preventDefault(); 
      dropzone.classList.add('border-indigo-500', 'bg-indigo-50'); 
    });
    dropzone.addEventListener('dragleave', () => { 
      dropzone.classList.remove('border-indigo-500', 'bg-indigo-50'); 
    });
    dropzone.addEventListener('drop', (e) => {
      e.preventDefault();
      dropzone.classList.remove('border-indigo-500', 'bg-indigo-50');
      if (e.dataTransfer && e.dataTransfer.files && e.dataTransfer.files.length) {
        fileInput.files = e.dataTransfer.files;
        updateSelected();
      }
    });
  }
  const openRequirementsBtn = document.getElementById('openRequirementsBtn');
  if (openRequirementsBtn) openRequirementsBtn.addEventListener('click', () => {
    if (!state.selected) return;
    window.location.href = `/pages/requirements.html?project_id=${state.selected.id}`;
  });
  const openTasksBtn = document.getElementById('openTasksBtn');
  if (openTasksBtn) openTasksBtn.addEventListener('click', () => {
    if (!state.selected) return;
    window.location.href = `/pages/tasks.html?project_id=${state.selected.id}`;
  });
  // Modal wiring
  const close = () => toggleProjectModal(false);
  const btnClose = document.getElementById('projectModalClose');
  const btnCancel = document.getElementById('projectFormCancel');
  if (btnClose) btnClose.addEventListener('click', close);
  if (btnCancel) btnCancel.addEventListener('click', close);
  const form = document.getElementById('projectForm');
  if (form) form.addEventListener('submit', submitProjectForm);
  
  // Add keyboard navigation for project modal
  const projectModal = document.getElementById('projectModal');
  if (projectModal) {
    projectModal.addEventListener('keydown', (ev) => { 
      if (ev.key === 'Escape') close(); 
    });
  }

  // Delete modal wiring
  const deleteClose = () => toggleDeleteModal(false);
  const dm = document.getElementById('deleteModal');
  const dmClose = document.getElementById('deleteModalClose');
  const dmCancel = document.getElementById('deleteCancelBtn');
  const dmConfirm = document.getElementById('deleteConfirmBtn');
  if (dmClose) dmClose.addEventListener('click', deleteClose);
  if (dmCancel) dmCancel.addEventListener('click', deleteClose);
  if (dm) dm.addEventListener('keydown', (ev) => { if (ev.key === 'Escape') deleteClose(); });
  if (dmConfirm) dmConfirm.addEventListener('click', async () => {
    if (!state.selected) { deleteClose(); return; }
    await removeProject(state.selected, { skipConfirm: true });
    deleteClose();
  });
}

async function init() {
  setupPanelToggle();
  setupActions();
  await loadConfig();
  
  // Initialize API client with 401 handler
  const handle401 = async () => {
    console.warn('Received 401 Unauthorized, refreshing GitLab status');
    try {
      await fetchGitLabStatus();
    } catch (err) {
      console.error('Failed to refresh GitLab status after 401:', err);
    }
  };
  
  api = new ProjectApiClient(() => state.config, handle401);
  
  // Check authentication status
  try {
    const authStatus = await fetch('/auth/me').then(r => r.json());
    state.authenticated = authStatus.authenticated || false;
    
    // Update auth UI
    updateAuthUI(authStatus);
    
    if (!state.authenticated) {
      // Redirect to login with current URL to return after auth
      const returnTo = encodeURIComponent(window.location.pathname + window.location.search + window.location.hash);
      window.location.href = `/auth/login?redirect_uri=${returnTo}`;
      return;
    }
  } catch (err) {
    console.error('Failed to check auth status:', err);
    updateAuthUI({ authenticated: false, username: null });
    // Redirect to login with current URL to return after auth
    const returnTo = encodeURIComponent(window.location.pathname + window.location.search + window.location.hash);
    window.location.href = `/auth/login?redirect_uri=${returnTo}`;
    return;
  }
  
  connectSSE();
  await fetchProjects();
  // Fetch GitLab connection status (session-level)
  try { 
    await fetchGitLabStatus();
    startPeriodicGitLabStatusCheck();
  } catch {}
}

// Loading overlay helpers
function showLoading() {
  state.loadingCount += 1;
  const overlay = document.getElementById('loadingOverlay');
  if (overlay && state.loadingCount > 0) overlay.classList.remove('hidden');
}

function hideLoading() {
  state.loadingCount = Math.max(0, state.loadingCount - 1);
  const overlay = document.getElementById('loadingOverlay');
  if (overlay && state.loadingCount === 0) overlay.classList.add('hidden');
}

// Project modal helpers
function toggleProjectModal(show) {
  const modal = document.getElementById('projectModal');
  if (!modal) return;
  if (show) modal.classList.remove('hidden'); else modal.classList.add('hidden');
}

function openProjectModal(project) {
  state.editTargetId = project ? project.id : null;
  const title = document.getElementById('projectModalTitle');
  if (title) title.textContent = project ? 'Edit Project' : 'Create Project';
  document.getElementById('f_name').value = project?.name || '';
  document.getElementById('f_description').value = project?.description || '';
  
  // Initialize GitLab Backlog Projects Manager
  state.gitlabBacklogProjectsManager.init();
  
  // Set repository URL (single field)
  const repoUrlInput = document.getElementById('f_gitlab_repository_url');
  if (repoUrlInput) {
    repoUrlInput.value = project?.gitlab_repository_url || '';
  }
  
  // Set backlog project URLs (multi-URL tags)
  if (project && project.gitlab_backlog_project_urls) {
    state.gitlabBacklogProjectsManager.setUrls(project.gitlab_backlog_project_urls);
  } else {
    state.gitlabBacklogProjectsManager.clear();
  }
  
  toggleProjectModal(true);
}

// Delete modal helpers
function toggleDeleteModal(show) {
  const modal = document.getElementById('deleteModal');
  if (!modal) return;
  if (show) modal.classList.remove('hidden'); else modal.classList.add('hidden');
}

function openDeleteModal(project) {
  const nameEl = document.getElementById('deleteProjectName');
  if (nameEl) nameEl.textContent = project?.name || '(untitled)';
  toggleDeleteModal(true);
}

async function submitProjectForm(ev) {
  ev.preventDefault();
  
  // Get repository URL (single field)
  const repoUrlInput = document.getElementById('f_gitlab_repository_url');
  const gitlabRepositoryUrl = repoUrlInput?.value?.trim() || null;
  
  // Get backlog project URLs (multi-URL tags)
  const gitlabBacklogUrls = state.gitlabBacklogProjectsManager.getUrls();
  
  const payload = {
    name: document.getElementById('f_name').value.trim(),
    description: valOrNull(document.getElementById('f_description').value),
    gitlab_repository_url: gitlabRepositoryUrl,
    gitlab_backlog_project_urls: gitlabBacklogUrls.length > 0 ? gitlabBacklogUrls : null,
  };
  
  if (!payload.name) { 
    alert('Name is required'); 
    return; 
  }
  
  try {
    if (state.editTargetId) {
      await api.put(`/projects/${state.editTargetId}`, payload);
    } else {
      await api.post('/projects', payload);
    }
    toggleProjectModal(false);
    await fetchProjects();
    if (state.editTargetId && state.selected && String(state.selected.id) === String(state.editTargetId)) {
      state.selected = state.projects.find(x => String(x.id) === String(state.editTargetId)) || null;
      renderDetails();
    }
    state.editTargetId = null;
  } catch (e) {
    console.error('Project save failed:', e);
    alert('Save failed: ' + (e.message || 'Unknown error'));
  }
}

function valOrNull(v) { const s = (v || '').trim(); return s === '' ? null : s; }

async function fetchGitLabStatus() {
  try {
    state.gitlab.status = 'connecting';
    updateConnectionsPanel();
    const cfg = state.config || {};
    const statusPath = cfg.gitlabAuthStatusPath || '/auth/gitlab/status';
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
    updateConnectionsPanel();
  }
}

function startPeriodicGitLabStatusCheck() {
  // Check GitLab status every 5 minutes
  const intervalId = setInterval(() => {
    fetchGitLabStatus().catch(err => {
      console.error('Periodic GitLab status check failed:', err);
    });
  }, 5 * 60 * 1000); // 5 minutes
  
  // Clear interval on page unload
  window.addEventListener('beforeunload', () => {
    clearInterval(intervalId);
  });
  
  console.log('Started periodic GitLab status checks (every 5 minutes)');
}

function startGitLabSSO() {
  try {
    state.gitlab.status = 'connecting';
    const returnTo = window.location.href;
    const cfg = state.config || {};
    const authorizePath = cfg.gitlabAuthAuthorizePath || '/auth/gitlab/authorize';
    const url = `${authorizePath}?redirect_uri=${encodeURIComponent(returnTo)}`;
    window.location.href = url;
  } catch {}
}

function updateAuthUI(authStatus) {
  try {
    const profile = document.getElementById('userProfile');
    const logoutBtn = document.getElementById('logoutBtn');
    
    if (authStatus.authenticated) {
      // Update user profile
      if (profile) {
        const username = authStatus.username || 'User';
        profile.textContent = username;
        profile.title = `Logged in as ${username}`;
      }
      
      // Show logout button
      if (logoutBtn) {
        logoutBtn.classList.remove('hidden');
      }
    } else {
      // Update user profile
      if (profile) {
        profile.textContent = 'Not logged in';
        profile.title = 'Not authenticated';
      }
      
      // Hide logout button
      if (logoutBtn) {
        logoutBtn.classList.add('hidden');
      }
    }
    
    // Update connections panel
    updateConnectionsPanel();
  } catch (err) {
    console.error('Failed to update auth UI:', err);
  }
}

function updateConnectionsPanel() {
  try {
    // Update auth indicator
    const authIndicator = document.getElementById('authIndicator');
    if (authIndicator) {
      if (state.authenticated) {
        authIndicator.className = 'w-2.5 h-2.5 rounded-full bg-emerald-500 ring-2 ring-white shadow-sm';
        authIndicator.title = '✓ Authenticated';
      } else {
        authIndicator.className = 'w-2.5 h-2.5 rounded-full bg-rose-500 ring-2 ring-white shadow-sm';
        authIndicator.title = '✗ Not authenticated';
      }
    }
    
    // Update GitLab indicator
    const gitlabIndicator = document.getElementById('gitlabIndicator');
    const gitlabConnectBtn = document.getElementById('gitlabConnectBtn');
    if (gitlabIndicator) {
      const s = (state.gitlab.status || '').toLowerCase();
      let cls = 'w-2.5 h-2.5 rounded-full bg-slate-400 ring-2 ring-white shadow-sm';
      let title = 'GitLab status';
      
      if (s === 'connected') {
        cls = 'w-2.5 h-2.5 rounded-full bg-emerald-500 ring-2 ring-white shadow-sm';
        title = '✓ Connected to GitLab';
      } else if (s === 'connecting') {
        cls = 'w-2.5 h-2.5 rounded-full bg-amber-500 ring-2 ring-white shadow-sm animate-pulse';
        title = '⟳ Checking GitLab connection…';
      } else if (s === 'not_connected') {
        cls = 'w-2.5 h-2.5 rounded-full bg-rose-500 ring-2 ring-white shadow-sm';
        title = state.gitlab.configured ? '✗ Not connected to GitLab (click link icon to connect)' : '○ GitLab SSO not configured';
      }
      
      gitlabIndicator.className = cls;
      gitlabIndicator.title = title;
    }
    
    // Update GitLab connect button state
    if (gitlabConnectBtn) {
      const disabled = !state.gitlab.configured;
      const title = disabled ? 'GitLab SSO not configured' : 'Connect to GitLab';
      gitlabConnectBtn.disabled = disabled;
      gitlabConnectBtn.title = title;
    }
    
    // Update RT Events indicator
    const rtEventsIndicator = document.getElementById('rtEventsIndicator');
    if (rtEventsIndicator) {
      const s = (state.rtEvents.status || '').toLowerCase();
      let cls = 'w-2.5 h-2.5 rounded-full bg-slate-400 ring-2 ring-white shadow-sm';
      let title = 'Real-time events';
      
      if (s === 'connected') {
        cls = 'w-2.5 h-2.5 rounded-full bg-emerald-500 ring-2 ring-white shadow-sm';
        title = '✓ Real-time events connected';
      } else if (s === 'connecting') {
        cls = 'w-2.5 h-2.5 rounded-full bg-amber-500 ring-2 ring-white shadow-sm animate-pulse';
        title = '⟳ Connecting to real-time events…';
      } else {
        cls = 'w-2.5 h-2.5 rounded-full bg-slate-400 ring-2 ring-white shadow-sm';
        title = '○ Real-time events disconnected';
      }
      
      rtEventsIndicator.className = cls;
      rtEventsIndicator.title = title;
    }
  } catch (err) {
    console.error('Failed to update connections panel:', err);
  }
}

async function handleLogout() {
  try {
    showLoading();
    await fetch('/auth/logout', { method: 'POST' });
    // Redirect to login after logout
    window.location.href = '/auth/login';
  } catch (err) {
    console.error('Logout failed:', err);
    alert('Logout failed. Please try again.');
  } finally {
    hideLoading();
  }
}

init().catch(err => console.error(err));




