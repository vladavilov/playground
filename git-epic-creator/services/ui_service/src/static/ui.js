'use strict';

const state = {
  config: null,
  projects: [],
  selected: null,
  evtSource: null,
  token: null,
  loadingCount: 0,
  editTargetId: null,
  filterText: '',
};

async function loadConfig() {
  const res = await fetch('/config');
  state.config = await res.json();
}

class ApiClient {
  constructor(getConfigFn, getTokenFn) {
    this.getConfig = getConfigFn;
    this.getToken = getTokenFn;
  }

  _buildHeaders(extra = {}, isJson = false) {
    const headers = { ...extra };
    const token = this.getToken();
    if (token) headers['Authorization'] = `Bearer ${token}`;
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
    else node.setAttribute(k, v);
  });
  for (const child of [].concat(children)) {
    if (child == null) continue;
    node.append(typeof child === 'string' ? document.createTextNode(child) : child);
  }
  return node;
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
    el('div', { class: 'text-lg font-semibold' }, p.name || '(untitled)'),
    p.description ? el('div', { class: 'mt-1 text-slate-700' }, p.description) : null,
    el('div', { class: 'mt-2' }, [getStatusBadge(p.status)]),
    el('div', { class: 'text-sm text-slate-600' }, `Created: ${p.created_at}`),
    el('div', { class: 'text-sm text-slate-600' }, `Updated: ${p.updated_at}`),
    el('div', { class: 'text-sm mt-2' }, [renderGitInfoLine(p.gitlab_url, 'project')]),
    el('div', { class: 'text-sm' }, [renderGitInfoLine(p.gitlab_repository_url, 'repository')]),
    el('div', { class: 'mt-4 flex items-center justify-between' }, [
      el('div', { class: 'flex items-center gap-2' }, [
        el('button', { id: 'openChatBtn', class: 'px-3 py-2 border rounded text-slate-700 hover:bg-slate-50', title: 'Open chat to capture requirements', onclick: () => { if (!state.selected) return; window.location.href = `/chat.html#${state.selected.id}`; } }, 'Open Chat'),
      ]),
      el('div', { class: 'flex items-center gap-2' }, [
        el('button', { class: 'px-3 py-2 border rounded text-slate-700 hover:bg-slate-50', onclick: () => openProjectModal(p), 'aria-label': 'Edit project' }, 'Edit'),
        el('button', { class: 'px-3 py-2 bg-rose-600 text-white rounded hover:bg-rose-700', onclick: () => openDeleteModal(p), 'aria-label': 'Delete project' }, 'Delete')
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
  const statusBar = document.getElementById('statusBar');
  state.evtSource = new EventSource('/events');
  state.evtSource.addEventListener('open', () => statusBar.textContent = 'Connected');
  state.evtSource.addEventListener('error', () => statusBar.textContent = 'Reconnecting...');
  state.evtSource.addEventListener('hello', () => statusBar.textContent = 'Connected');
  state.evtSource.addEventListener('project_progress', (evt) => {
    try {
      const msg = JSON.parse(evt.data);
      if (!state.selected || String(state.selected.id) !== String(msg.project_id)) return;
      const text = document.getElementById('progressText');
      const bar = document.getElementById('progressBar');
      const badgeHost = document.getElementById('statusBadge');
      const logHost = document.getElementById('progressLog');
      const pct = document.getElementById('progressPct');
      if (badgeHost) { badgeHost.innerHTML = ''; badgeHost.appendChild(getStatusBadge(msg.status)); }
      const stepLabel = (msg.process_step && String(msg.process_step).trim() !== '') ? String(msg.process_step) : (msg.status || 'unknown');
      const safePct = (Number.isFinite(msg.processed_pct) ? Math.max(0, Math.min(100, Math.round(msg.processed_pct))) : null);

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
    } catch (err) {
      // Swallow errors to avoid breaking SSE loop
      // Optionally, could console.debug(err);
    }
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
  const clearBtn = document.getElementById('clearLogBtn');
  if (clearBtn) clearBtn.addEventListener('click', () => {
    const log = document.getElementById('progressLog');
    if (log) log.innerHTML = '';
  });
  const clearBtnTop = document.getElementById('clearLogBtnTop');
  if (clearBtnTop) clearBtnTop.addEventListener('click', () => {
    const log = document.getElementById('progressLog');
    if (log) log.innerHTML = '';
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
  api = new ApiClient(() => state.config, () => state.token);
  // Get dev token when available
  try {
    const t = await fetch('/dev-token', { method: 'POST' }).then(r => r.json());
    state.token = t && t.access_token ? t.access_token : null;
  } catch {}
  connectSSE();
  await fetchProjects();
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

init().catch(err => console.error(err));



