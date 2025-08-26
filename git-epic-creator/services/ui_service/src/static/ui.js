'use strict';

const state = {
  config: null,
  projects: [],
  selected: null,
  evtSource: null,
  token: null,
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
    const res = await fetch(url, init);
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
  state.projects.forEach(p => {
    const item = el('li', { class: 'border rounded p-3 hover:bg-slate-50' }, [
      el('div', { class: 'flex items-start justify-between gap-2' }, [
        el('div', {}, [
          el('div', { class: 'font-semibold' }, p.name || '(untitled)'),
          el('div', { class: 'text-sm text-slate-600 mt-1' }, `Status: ${p.status}`),
          el('div', { class: 'text-xs text-slate-500' }, `ID: ${p.id}`),
          p.description ? el('div', { class: 'text-sm text-slate-600' }, p.description) : null,
          p.gitlab_url ? el('a', { class: 'text-sm text-sky-600 underline', href: p.gitlab_url, target: '_blank' }, 'GitLab Project') : null,
          p.gitlab_repository_url ? el('a', { class: 'text-sm text-sky-600 underline block', href: p.gitlab_repository_url, target: '_blank' }, 'Repository') : null,
        ]),
        el('div', { class: 'flex flex-col gap-2' }, [
          el('button', { class: 'px-2 py-1 text-sm border rounded', onclick: () => selectProject(p) }, 'Open'),
          el('button', { class: 'px-2 py-1 text-sm border rounded', onclick: () => openEdit(p) }, 'Edit'),
          el('button', { class: 'px-2 py-1 text-sm border rounded text-rose-600', onclick: () => removeProject(p) }, 'Delete'),
        ])
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
    chat.classList.add('hidden');
    return;
  }
  box.classList.remove('hidden');
  upload.classList.remove('hidden');
  chat.classList.remove('hidden');
  box.innerHTML = '';
  const p = state.selected;
  box.append(
    el('div', { class: 'text-lg font-semibold' }, p.name || '(untitled)'),
    p.description ? el('div', { class: 'mt-1 text-slate-700' }, p.description) : null,
    el('div', { class: 'text-sm text-slate-600 mt-2' }, `Status: ${p.status}`),
    el('div', { class: 'text-sm text-slate-600' }, `Created: ${p.created_at}`),
    el('div', { class: 'text-sm text-slate-600' }, `Updated: ${p.updated_at}`),
    p.gitlab_url ? el('a', { class: 'text-sm text-sky-600 underline block mt-2', href: p.gitlab_url, target: '_blank' }, 'GitLab Project') : null,
    p.gitlab_repository_url ? el('a', { class: 'text-sm text-sky-600 underline block', href: p.gitlab_repository_url, target: '_blank' }, 'Repository') : null,
  );
}

async function fetchProjects() {
  const projects = await api.get('/projects');
  state.projects = projects;
  renderProjects();
}

async function createProject() {
  const name = document.getElementById('projectName').value.trim();
  if (!name) return;
  try {
    await api.postJson('/projects', { name });
  } catch (e) {
    alert('Create failed');
    return;
  }
  document.getElementById('projectName').value = '';
  await fetchProjects();
}

async function openEdit(p) {
  const name = prompt('Project name', p.name || '');
  if (name == null) return;
  try {
    await api.putJson(`/projects/${p.id}`, { name });
  } catch (e) { alert('Update failed'); return; }
  await fetchProjects();
}

async function removeProject(p) {
  const ok = confirm('Delete project?');
  if (!ok) return;
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
}

async function uploadFiles() {
  if (!state.selected) return;
  const files = document.getElementById('fileInput').files;
  if (!files || files.length === 0) return;
  const form = new FormData();
  for (const f of files) form.append('files', f);
  try {
    await api.postForm(`/projects/${state.selected.id}/documents/upload`, form);
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
      const total = msg.total_count || 0;
      const processed = msg.processed_count || 0;
      const pct = msg.processed_pct != null ? msg.processed_pct : (total > 0 ? processed / total : 0);
      text.textContent = `Status: ${msg.status} — ${Math.round(pct * 100)}% (${processed}/${total})`;
      bar.style.width = `${Math.round(pct * 100)}%`;
    } catch {}
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
  document.getElementById('uploadBtn').addEventListener('click', uploadFiles);
  document.getElementById('openChatBtn').addEventListener('click', () => {
    if (!state.selected) return;
    window.location.href = `/chat.html#${state.selected.id}`;
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

init().catch(err => console.error(err));


