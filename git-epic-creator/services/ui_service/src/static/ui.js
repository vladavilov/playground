'use strict';

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
};

async function loadConfig() {
  const res = await fetch('/config');
  state.config = await res.json();
}

class ApiClient {
  constructor(getConfigFn) {
    this.getConfig = getConfigFn;
  }

  _buildHeaders(extra = {}, isJson = false) {
    const headers = { ...extra };
    if (isJson) headers['Content-Type'] = 'application/json';
    return headers;
  }

  _base() {
    const cfg = this.getConfig();
    if (!cfg || !cfg.projectManagementApiBase) throw new Error('API base not configured');
    return cfg.projectManagementApiBase.replace(/\/$/, '');
  }

  async request(method, path, { json, formData, headers } = {}) {
    const url = `${this._base()}${path}`;
    const init = { method, headers: this._buildHeaders(headers || {}, Boolean(json)) };
    if (json !== undefined) init.body = JSON.stringify(json);
    if (formData !== undefined) init.body = formData;
    showLoading();
    let res;
    try {
      res = await fetch(url, init);
    } finally {
      hideLoading();
    }
    if (!res.ok) {
      const detail = await res.text().catch(() => '');
      throw new Error(`${method} ${path} failed: ${res.status} ${detail}`);
    }
    // No Content responses should not be parsed
    if (res.status === 204 || res.status === 205) return null;
    const len = res.headers.get('content-length');
    if (len === '0') return null;
    const contentType = res.headers.get('content-type') || '';
    if (contentType.includes('application/json')) return res.json();
    return res.text();
  }

  get(path) { return this.request('GET', path); }
  postJson(path, body) { return this.request('POST', path, { json: body }); }
  putJson(path, body) { return this.request('PUT', path, { json: body }); }
  delete(path) { return this.request('DELETE', path); }
  postForm(path, formData) { return this.request('POST', path, { formData }); }
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
  state.projects
    .filter(p => (p.name || '').toLowerCase().includes(q))
    .forEach(p => {
      const item = el('li', { class: 'border rounded p-3 hover:bg-slate-50 cursor-pointer', onclick: () => selectProject(p), role: 'button', tabindex: '0' }, [
        el('div', { class: 'flex items-start justify-between gap-2' }, [
          el('div', {}, [
            el('div', { class: 'font-semibold' }, p.name || '(untitled)'),
            p.description ? el('div', { class: 'text-sm text-slate-600 mt-1 truncate' }, p.description) : null,
          ]),
          el('div', { class: 'mt-0.5' }, [getStatusBadge(p.status)])
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
    box.classList.add('hidden');
    upload.classList.add('hidden');
    if (chat) chat.classList.add('hidden');
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
    el('div', { class: 'mt-3 space-y-1' }, [
      el('div', { class: 'text-sm' }, [renderGitInfoLine(p.gitlab_url, 'project')]),
      el('div', { class: 'text-sm' }, [renderGitRepoLine(p.gitlab_repository_url)])
    ]),
    el('div', { class: 'mt-4 flex items-center justify-between' }, [
      el('div', { class: 'flex items-center gap-2' }, [
        el('button', { id: 'openChatBtn', class: 'w-32 px-3 py-2 border rounded text-slate-700 hover:bg-slate-50 text-center', title: 'Open chat to capture requirements', onclick: () => { if (!state.selected) return; window.location.href = `/chat.html#${state.selected.id}`; } }, 'Open Chat'),
      ]),
      el('div', { class: 'flex items-center gap-2' }, [
        el('button', { class: 'w-32 px-3 py-2 border rounded text-slate-700 hover:bg-slate-50 text-center', onclick: () => openProjectModal(p), 'aria-label': 'Edit project' }, 'Edit'),
        el('button', { class: 'w-32 px-3 py-2 bg-rose-600 text-white rounded hover:bg-rose-700 text-center', onclick: () => openDeleteModal(p), 'aria-label': 'Delete project' }, 'Delete')
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
function renderGitInfoLine(value, type) {
  const has = value && String(value).trim() !== '';
  if (has) {
    return el('a', { class: 'text-sm text-sky-600 underline block', href: value, target: '_blank' }, type === 'project' ? 'GitLab Project' : 'Repository');
  }
  return el('div', { class: 'text-sm text-slate-500' }, type === 'project' ? 'N/A project' : 'N/A repository');
}

function getGitLabStatusIndicator(status, configured) {
  const s = (status || '').toLowerCase();
  const titleByState = {
    connected: configured ? 'Connected to GitLab' : 'Connected (configuration missing?)',
    connecting: 'Checking GitLab connection…',
    'not_connected': configured ? 'Not connected to GitLab' : 'GitLab SSO not configured',
  };
  let cls = 'inline-block w-2.5 h-2.5 rounded-full bg-slate-400';
  if (s === 'connected') cls = 'inline-block w-2.5 h-2.5 rounded-full bg-emerald-500';
  else if (s === 'connecting') cls = 'inline-block w-2.5 h-2.5 rounded-full bg-blue-500 animate-pulse';
  else if (s === 'not_connected') cls = 'inline-block w-2.5 h-2.5 rounded-full bg-rose-500';
  return el('span', { class: cls, title: titleByState[s] || 'GitLab status', 'aria-label': titleByState[s] || 'GitLab status' }, '');
}

function renderGitRepoLine(repoUrl) {
  const link = (repoUrl && String(repoUrl).trim() !== '')
    ? el('a', { class: 'text-sky-600 underline', href: repoUrl, target: '_blank' }, 'Repository')
    : el('span', { class: 'text-slate-500' }, 'N/A repository');
  return link;
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
    for (const project of state.projects) {
      if (String(project.id) !== idStr) continue;
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
    if (state.selected && String(state.selected.id) === idStr) {
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

function connectSSE() {
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
      const isSelected = !!state.selected && pid != null && String(state.selected.id) === String(pid);
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
  document.getElementById('openPanel').addEventListener('click', () => panel.classList.remove('hidden'));
  const hideBtn = document.getElementById('togglePanel');
  if (hideBtn) hideBtn.addEventListener('click', () => panel.classList.add('hidden'));
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
    if (files.length === 0) {
      fileCount.textContent = 'No files selected';
      selectedFiles.classList.add('hidden');
      selectedFiles.textContent = '';
      return;
    }
    fileCount.textContent = `${files.length} file${files.length > 1 ? 's' : ''} selected`;
    selectedFiles.classList.remove('hidden');
    selectedFiles.textContent = files.map(f => `• ${f.name}`).join(', ');
  };
  if (fileInput) fileInput.addEventListener('change', updateSelected);
  if (dropzone && fileInput) {
    dropzone.addEventListener('click', () => fileInput.click());
    dropzone.addEventListener('dragover', (e) => { e.preventDefault(); dropzone.classList.add('border-slate-400'); });
    dropzone.addEventListener('dragleave', () => dropzone.classList.remove('border-slate-400'));
    dropzone.addEventListener('drop', (e) => {
      e.preventDefault();
      dropzone.classList.remove('border-slate-400');
      if (e.dataTransfer && e.dataTransfer.files && e.dataTransfer.files.length) {
        fileInput.files = e.dataTransfer.files;
        updateSelected();
      }
    });
  }
  const openChatBtn = document.getElementById('openChatBtn');
  if (openChatBtn) openChatBtn.addEventListener('click', () => {
    if (!state.selected) return;
    window.location.href = `/chat.html#${state.selected.id}`;
  });
  // Modal wiring
  const close = () => toggleProjectModal(false);
  const btnClose = document.getElementById('projectModalClose');
  const btnCancel = document.getElementById('projectFormCancel');
  if (btnClose) btnClose.addEventListener('click', close);
  if (btnCancel) btnCancel.addEventListener('click', close);
  const form = document.getElementById('projectForm');
  if (form) form.addEventListener('submit', submitProjectForm);

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
  api = new ApiClient(() => state.config);
  
  // Check authentication status
  try {
    const authStatus = await fetch('/auth/me').then(r => r.json());
    state.authenticated = authStatus.authenticated || false;
    
    // Update auth UI
    updateAuthUI(authStatus);
    
    if (!state.authenticated) {
      // Redirect to login if not authenticated
      window.location.href = '/auth/login';
      return;
    }
  } catch (err) {
    console.error('Failed to check auth status:', err);
    updateAuthUI({ authenticated: false, username: null });
    window.location.href = '/auth/login';
    return;
  }
  
  connectSSE();
  await fetchProjects();
  // Fetch GitLab connection status (session-level)
  try { await fetchGitLabStatus(); } catch {}
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
  document.getElementById('f_gitlab_url').value = project?.gitlab_url || '';
  document.getElementById('f_gitlab_repository_url').value = project?.gitlab_repository_url || '';
  document.getElementById('f_status').value = (project?.status || 'active').toLowerCase();
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
  const payload = {
    name: document.getElementById('f_name').value.trim(),
    description: valOrNull(document.getElementById('f_description').value),
    gitlab_url: emptyToNull(document.getElementById('f_gitlab_url').value),
    gitlab_repository_url: emptyToNull(document.getElementById('f_gitlab_repository_url').value),
    status: document.getElementById('f_status').value,
  };
  if (!payload.name) { alert('Name is required'); return; }
  try {
    if (state.editTargetId) {
      await api.putJson(`/projects/${state.editTargetId}`, payload);
    } else {
      await api.postJson('/projects', payload);
    }
    toggleProjectModal(false);
    await fetchProjects();
    if (state.editTargetId && state.selected && String(state.selected.id) === String(state.editTargetId)) {
      state.selected = state.projects.find(x => String(x.id) === String(state.editTargetId)) || null;
      renderDetails();
    }
    state.editTargetId = null;
  } catch (e) {
    alert('Save failed');
  }
}

function emptyToNull(v) { const s = (v || '').trim(); return s === '' ? null : s; }
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
      const s = (state.gitlab.status || '').toLowerCase();
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
        title = state.gitlab.configured ? 'Not connected to GitLab' : 'GitLab SSO not configured';
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



