'use strict';

// Import shared utilities
import {
  escapeHtml as esc,
  scrollToBottom,
  renderMarkdown,
  getStatusBadge,
  createLoadingManager,
  checkAuthStatus,
  fetchGitLabStatus,
  startGitLabSSO as sharedStartGitLabSSO,
  connectSSE as sharedConnectSSE,
  fetchConfig
} from './shared-ui.js';

// Application State
const state = {
  config: null,
  projectId: null,
  project: null,
  authenticated: false,
  gitlab: { status: 'connecting', configured: false },
  rtEvents: { status: 'disconnected' },
  boxesByPromptId: {},
  thinkingBoxes: [],
  activeBox: null,
  pendingBox: null,
  currentPrompt: null,
  currentBundle: null,
  evtSource: null
};

// Create loading manager
const loadingManager = createLoadingManager();

// DOM Elements
let chatOutput, chatForm, chatInput, projectTitleEl, projectStatusEl, requirementsContent, requirementsSummary, editRequirementsBtn, confirmRequirementsBtn;

// ============================================================================
// Thinking Box UI
// ============================================================================

function createThinkingBox(promptId = null) {
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
  messages.className = 'mt-3 space-y-1.5 text-sm text-slate-600';

  wrap.appendChild(summary);
  wrap.appendChild(messages);
  chatOutput.appendChild(wrap);
  scrollToBottom(chatOutput);

  const startedAt = Date.now();
  const box = {
    promptId: promptId,
    element: wrap,
    setPromptId(newId) {
      if (!newId) return;
      this.promptId = newId;
      state.boxesByPromptId[newId] = this;
    },
    appendStream(text) {
      if (!text) return;
      const line = document.createElement('div');
      line.className = 'text-xs text-slate-500';
      line.textContent = String(text);
      messages.appendChild(line);
      scrollToBottom(chatOutput);
    },
    appendMarkdown(md) {
      if (!md) return;
      const line = document.createElement('div');
      line.innerHTML = renderMarkdown(String(md));
      messages.appendChild(line);
      scrollToBottom(chatOutput);
    },
    finish(status = 'ok') {
      const seconds = Math.max(0, Math.round((Date.now() - startedAt) / 1000));
      label.textContent = `Agent completed in ${seconds}s`;
      icon.className = status === 'ok' 
        ? 'inline-block w-2 h-2 rounded-full bg-emerald-500' 
        : 'inline-block w-2 h-2 rounded-full bg-rose-500';
      wrap.open = false;
    }
  };

  state.thinkingBoxes.push(box);
  if (promptId) state.boxesByPromptId[promptId] = box;
  state.activeBox = box;
  return box;
}

function getOrCreateBoxForPromptId(promptId) {
  if (!promptId) return state.activeBox || state.pendingBox || createThinkingBox(null);
  if (state.boxesByPromptId[promptId]) return state.boxesByPromptId[promptId];
  if (state.pendingBox && !state.pendingBox.promptId) {
    state.pendingBox.setPromptId(promptId);
    const b = state.pendingBox;
    state.pendingBox = null;
    state.activeBox = b;
    return b;
  }
  return createThinkingBox(promptId);
}

// ============================================================================
// Chat UI
// ============================================================================

function appendUserMessage(text) {
  const wrap = document.createElement('div');
  wrap.className = 'flex flex-col gap-1';
  wrap.innerHTML = `
    <div class="text-xs text-slate-500 font-medium">You</div>
    <div class="bg-white border border-slate-200 rounded-lg p-3 shadow-sm">
      <p class="text-sm text-slate-800">${esc(text)}</p>
    </div>
  `;
  chatOutput.appendChild(wrap);
  scrollToBottom(chatOutput);
}

function appendAssistantHtml(html, meta = 'AI Agent') {
  const wrap = document.createElement('div');
  wrap.className = 'flex flex-col gap-1';
  wrap.innerHTML = `
    <div class="text-xs text-slate-500 font-medium">${esc(meta)}</div>
    <div class="bg-gradient-to-br from-indigo-50 to-purple-50 border border-indigo-200 rounded-lg p-4 shadow-sm">
      ${html}
    </div>
  `;
  chatOutput.appendChild(wrap);
  scrollToBottom(chatOutput);
}

function appendSystemMessage(text) {
  const wrap = document.createElement('div');
  wrap.className = 'flex items-center justify-center my-2';
  wrap.innerHTML = `
    <div class="bg-slate-100 border border-slate-200 rounded-full px-3 py-1 text-xs text-slate-600">
      ${esc(text)}
    </div>
  `;
  chatOutput.appendChild(wrap);
  scrollToBottom(chatOutput);
}

// ============================================================================
// Requirements Rendering
// ============================================================================

function renderRequirements(bundle) {
  if (!bundle || (!bundle.business_requirements?.length && !bundle.functional_requirements?.length)) {
    requirementsContent.innerHTML = `
      <div class="flex flex-col items-center justify-center h-full text-center">
        <div class="w-16 h-16 rounded-full bg-slate-200 flex items-center justify-center mb-4">
          <svg class="w-8 h-8 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"></path>
          </svg>
        </div>
        <p class="text-slate-500">No requirements generated yet</p>
      </div>
    `;
    requirementsSummary.textContent = 'No requirements yet';
    editRequirementsBtn.classList.add('hidden');
    confirmRequirementsBtn.classList.add('hidden');
    return;
  }

  // Update summary
  const businessCount = bundle.business_requirements?.length || 0;
  const functionalCount = bundle.functional_requirements?.length || 0;
  requirementsSummary.textContent = `${businessCount + functionalCount} requirement${(businessCount + functionalCount) !== 1 ? 's' : ''} (${businessCount} business, ${functionalCount} functional)`;
  editRequirementsBtn.classList.remove('hidden');
  confirmRequirementsBtn.classList.remove('hidden');

  // Helper functions
  const priorityPill = (p) => {
    const val = String(p || '').toLowerCase();
    let cls = 'bg-slate-100 text-slate-700 border-slate-200';
    if (val === 'must') cls = 'bg-amber-50 text-amber-700 border-amber-200';
    else if (val === 'should') cls = 'bg-sky-50 text-sky-700 border-sky-200';
    else if (val === 'could') cls = 'bg-emerald-50 text-emerald-700 border-emerald-200';
    return `<span class="inline-flex items-center px-2 py-0.5 rounded border ${cls} text-xs font-medium">${esc(p)}</span>`;
  };

  const renderReqCard = (req, idx, type) => `
    <div class="req-card bg-white border border-slate-200 rounded-lg p-4 shadow-sm hover:shadow-md transition-shadow">
      <div class="flex items-start justify-between gap-2 mb-2">
        <h3 class="font-semibold text-slate-800 flex-1">${esc(req.title || `Requirement ${idx + 1}`)}</h3>
        <span class="px-2 py-0.5 bg-indigo-100 text-indigo-700 rounded text-xs font-medium whitespace-nowrap">${type}${idx + 1}</span>
      </div>
      <p class="text-sm text-slate-600 mb-3">${esc(req.description || '')}</p>
      <div class="flex items-center gap-2 mb-2">
        <span class="text-xs font-medium text-slate-700">Priority:</span>
        ${priorityPill(req.priority)}
      </div>
      ${req.acceptance_criteria?.length ? `
        <div class="mt-3 pt-3 border-t border-slate-100">
          <div class="text-xs font-semibold text-slate-700 mb-1.5">Acceptance Criteria</div>
          <ul class="space-y-1">
            ${req.acceptance_criteria.map(ac => `<li class="text-xs text-slate-600 ml-4 list-disc">${esc(ac)}</li>`).join('')}
          </ul>
        </div>
      ` : ''}
    </div>
  `;

  // Build HTML
  let html = '';

  // Business Requirements
  if (bundle.business_requirements?.length > 0) {
    html += `
      <div class="mb-6">
        <h3 class="text-xs uppercase tracking-wide font-semibold text-slate-500 mb-3">Business Requirements</h3>
        <div class="space-y-3">
          ${bundle.business_requirements.map((req, idx) => renderReqCard(req, idx, 'BR')).join('')}
    </div>
      </div>
    `;
  }

  // Functional Requirements
  if (bundle.functional_requirements?.length > 0) {
    html += `
      <div class="mb-6">
        <h3 class="text-xs uppercase tracking-wide font-semibold text-slate-500 mb-3">Functional Requirements</h3>
        <div class="space-y-3">
          ${bundle.functional_requirements.map((req, idx) => renderReqCard(req, idx, 'FR')).join('')}
        </div>
      </div>
    `;
  }

  // Assumptions and Risks
  if ((bundle.assumptions?.length > 0) || (bundle.risks?.length > 0)) {
    html += `<div class="grid grid-cols-1 sm:grid-cols-2 gap-4 mb-4">`;
    
    if (bundle.assumptions?.length > 0) {
      html += `
        <div class="bg-white border border-slate-200 rounded-lg p-4 shadow-sm">
          <h4 class="text-sm font-semibold text-slate-700 mb-2">Assumptions</h4>
          <ul class="space-y-1">
            ${bundle.assumptions.map(a => `<li class="text-xs text-slate-600 ml-4 list-disc">${esc(a)}</li>`).join('')}
          </ul>
        </div>
      `;
    }
    
    if (bundle.risks?.length > 0) {
      html += `
        <div class="bg-white border border-rose-100 rounded-lg p-4 shadow-sm">
          <h4 class="text-sm font-semibold text-rose-700 mb-2">Risks</h4>
          <ul class="space-y-1">
            ${bundle.risks.map(r => `<li class="text-xs text-rose-600 ml-4 list-disc">${esc(r)}</li>`).join('')}
          </ul>
        </div>
      `;
    }
    
    html += `</div>`;
  }

  // Quality Score
  if (bundle.score != null) {
    const scoreValue = Math.round(bundle.score * 100);
    const scoreColor = scoreValue >= 75 ? 'emerald' : scoreValue >= 50 ? 'amber' : 'rose';
    html += `
      <div class="p-3 bg-gradient-to-r from-${scoreColor}-50 to-${scoreColor}-100 border border-${scoreColor}-200 rounded-lg">
        <div class="flex items-center justify-between">
          <span class="text-sm font-medium text-slate-700">Quality Score</span>
          <span class="px-3 py-1 bg-${scoreColor}-100 text-${scoreColor}-700 rounded-full text-sm font-semibold">${scoreValue}%</span>
      </div>
    </div>
    `;
  }

  requirementsContent.innerHTML = html;
}

// ============================================================================
// Requirements Editor Modal
// ============================================================================

function openEditorModal() {
  if (!state.currentBundle) return;
  
  const modal = document.getElementById('editorModal');
  const content = document.getElementById('editorContent');
  
  // Build editor UI
  let html = '';
  
  // Business Requirements
  if (state.currentBundle.business_requirements?.length > 0) {
    html += `
      <div class="border border-slate-200 rounded-lg p-4 bg-white">
        <h3 class="text-base font-semibold text-slate-800 mb-4">Business Requirements</h3>
        <div class="space-y-4" data-req-type="business">
    `;
    
    state.currentBundle.business_requirements.forEach((req, idx) => {
      html += `
        <div class="border border-slate-200 rounded-lg p-3 bg-slate-50">
          <div class="mb-2">
            <label class="block text-xs font-medium text-slate-700 mb-1">Title</label>
            <input type="text" class="w-full border border-slate-300 rounded px-3 py-2 text-sm" value="${esc(req.title || '')}" data-req-idx="${idx}" data-field="title" />
          </div>
          <div class="mb-2">
            <label class="block text-xs font-medium text-slate-700 mb-1">Description</label>
            <textarea class="w-full border border-slate-300 rounded px-3 py-2 text-sm" rows="2" data-req-idx="${idx}" data-field="description">${esc(req.description || '')}</textarea>
          </div>
          <div class="mb-2">
            <label class="block text-xs font-medium text-slate-700 mb-1">Priority</label>
            <select class="border border-slate-300 rounded px-3 py-2 text-sm" data-req-idx="${idx}" data-field="priority">
              <option value="MUST" ${req.priority === 'MUST' ? 'selected' : ''}>MUST</option>
              <option value="SHOULD" ${req.priority === 'SHOULD' ? 'selected' : ''}>SHOULD</option>
              <option value="COULD" ${req.priority === 'COULD' ? 'selected' : ''}>COULD</option>
              <option value="WONT" ${req.priority === 'WONT' ? 'selected' : ''}>WONT</option>
            </select>
          </div>
          <div class="mb-2">
            <label class="block text-xs font-medium text-slate-700 mb-1">Acceptance Criteria (one per line)</label>
            <textarea class="w-full border border-slate-300 rounded px-3 py-2 text-sm" rows="3" data-req-idx="${idx}" data-field="acceptance_criteria">${(req.acceptance_criteria || []).join('\n')}</textarea>
          </div>
        </div>
      `;
    });
    
    html += `
        </div>
      </div>
    `;
  }
  
  // Functional Requirements
  if (state.currentBundle.functional_requirements?.length > 0) {
    html += `
      <div class="border border-slate-200 rounded-lg p-4 bg-white">
        <h3 class="text-base font-semibold text-slate-800 mb-4">Functional Requirements</h3>
        <div class="space-y-4" data-req-type="functional">
    `;
    
    state.currentBundle.functional_requirements.forEach((req, idx) => {
      html += `
        <div class="border border-slate-200 rounded-lg p-3 bg-slate-50">
          <div class="mb-2">
            <label class="block text-xs font-medium text-slate-700 mb-1">Title</label>
            <input type="text" class="w-full border border-slate-300 rounded px-3 py-2 text-sm" value="${esc(req.title || '')}" data-req-idx="${idx}" data-field="title" />
          </div>
          <div class="mb-2">
            <label class="block text-xs font-medium text-slate-700 mb-1">Description</label>
            <textarea class="w-full border border-slate-300 rounded px-3 py-2 text-sm" rows="2" data-req-idx="${idx}" data-field="description">${esc(req.description || '')}</textarea>
          </div>
          <div class="mb-2">
            <label class="block text-xs font-medium text-slate-700 mb-1">Priority</label>
            <select class="border border-slate-300 rounded px-3 py-2 text-sm" data-req-idx="${idx}" data-field="priority">
              <option value="MUST" ${req.priority === 'MUST' ? 'selected' : ''}>MUST</option>
              <option value="SHOULD" ${req.priority === 'SHOULD' ? 'selected' : ''}>SHOULD</option>
              <option value="COULD" ${req.priority === 'COULD' ? 'selected' : ''}>COULD</option>
              <option value="WONT" ${req.priority === 'WONT' ? 'selected' : ''}>WONT</option>
            </select>
          </div>
          <div class="mb-2">
            <label class="block text-xs font-medium text-slate-700 mb-1">Acceptance Criteria (one per line)</label>
            <textarea class="w-full border border-slate-300 rounded px-3 py-2 text-sm" rows="3" data-req-idx="${idx}" data-field="acceptance_criteria">${(req.acceptance_criteria || []).join('\n')}</textarea>
          </div>
        </div>
      `;
    });
    
    html += `
        </div>
      </div>
    `;
  }
  
  content.innerHTML = html;
  modal.classList.remove('hidden');
  modal.classList.add('flex');
}

function closeEditorModal() {
  const modal = document.getElementById('editorModal');
  modal.classList.add('hidden');
  modal.classList.remove('flex');
}

function saveEditorChanges() {
  const content = document.getElementById('editorContent');
  
  // Collect changes from business requirements
  const businessContainer = content.querySelector('[data-req-type="business"]');
  if (businessContainer && state.currentBundle.business_requirements) {
    businessContainer.querySelectorAll('[data-req-idx]').forEach(input => {
      const idx = parseInt(input.getAttribute('data-req-idx'));
      const field = input.getAttribute('data-field');
      
      if (field === 'acceptance_criteria') {
        const text = input.value.trim();
        state.currentBundle.business_requirements[idx][field] = text ? text.split('\n').filter(l => l.trim()) : [];
      } else {
        state.currentBundle.business_requirements[idx][field] = input.value;
      }
    });
  }
  
  // Collect changes from functional requirements
  const functionalContainer = content.querySelector('[data-req-type="functional"]');
  if (functionalContainer && state.currentBundle.functional_requirements) {
    functionalContainer.querySelectorAll('[data-req-idx]').forEach(input => {
      const idx = parseInt(input.getAttribute('data-req-idx'));
      const field = input.getAttribute('data-field');
      
      if (field === 'acceptance_criteria') {
        const text = input.value.trim();
        state.currentBundle.functional_requirements[idx][field] = text ? text.split('\n').filter(l => l.trim()) : [];
      } else {
        state.currentBundle.functional_requirements[idx][field] = input.value;
      }
    });
  }
  
  // Re-render
  renderRequirements(state.currentBundle);
  closeEditorModal();
  
  appendSystemMessage('Changes saved locally. Requirements ready for confirmation.');
}

// ============================================================================
// API Communication
// ============================================================================

async function loadProject() {
  const id = location.hash ? location.hash.substring(1) : '';
  state.projectId = id;
  if (!id) return;
  
  const base = state.config.projectManagementApiBase.replace(/\/$/, '');
  const res = await fetch(`${base}/projects/${id}`);
  if (!res.ok) return;
  
  state.project = await res.json();
  projectTitleEl.textContent = state.project.name || '(untitled)';
  projectStatusEl.innerHTML = '';
  projectStatusEl.appendChild(getStatusBadge(state.project.status));
}

async function sendRequirementsRequest(text) {
  try {
    const base = state.config.aiWorkflowApiBase.replace(/\/$/, '');
    const headers = { 'Content-Type': 'application/json' };
    const url = `${base}/requirements`;
    
    const res = await fetch(url, {
      method: 'POST',
      headers,
      body: JSON.stringify({ project_id: state.projectId, prompt: text })
    });
    
    // If 401 Unauthorized, refresh GitLab connection status immediately
    if (res.status === 401) {
      console.warn('Received 401 Unauthorized, refreshing GitLab status');
      try {
        await fetchGitLabStatus(state, state.config);
      } catch (err) {
        console.error('Failed to refresh GitLab status after 401:', err);
      }
    }
    
    if (res.ok) {
      const bundle = await res.json();
      state.currentBundle = bundle;
      
      // Render in right panel instead of inline chat
      renderRequirements(bundle);
      
      // Show success message in chat
      const businessCount = bundle.business_requirements?.length || 0;
      const functionalCount = bundle.functional_requirements?.length || 0;
      appendAssistantHtml(
        `<div class="text-sm text-indigo-700">✓ Generated ${businessCount + functionalCount} requirement(s) (${businessCount} business, ${functionalCount} functional)</div>`, 
        'Assistant'
      );
      
      const pid = bundle.prompt_id;
      if (state.pendingBox && !state.pendingBox.promptId && pid) {
        state.pendingBox.setPromptId(pid);
        state.pendingBox = null;
      }
      const box = (pid && state.boxesByPromptId[pid]) ? state.boxesByPromptId[pid] : state.activeBox;
      if (box) box.finish('ok');
    } else {
      let errorMsg = `Error ${res.status}: failed to generate requirements`;
      try {
        const errorData = await res.json();
        if (errorData.detail) errorMsg = `Error ${res.status}: ${errorData.detail}`;
      } catch {}
      appendSystemMessage(errorMsg);
      if (state.pendingBox) {
        state.pendingBox.finish('error');
        state.pendingBox = null;
      }
    }
  } catch (err) {
    const errorMsg = err.message ? `Network error: ${err.message}` : 'Network error while contacting AI workflow';
    appendSystemMessage(errorMsg);
    if (state.pendingBox) {
      state.pendingBox.finish('error');
      state.pendingBox = null;
    }
  }
}

// ============================================================================
// Event Handlers
// ============================================================================

function setupInputBehavior() {
  chatInput.addEventListener('keydown', (e) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      chatForm.requestSubmit();
    }
  });
  
  chatForm.addEventListener('submit', async (e) => {
    e.preventDefault();
    const text = chatInput.value.trim();
    if (!text) return;
    
    appendUserMessage(text);
    chatInput.value = '';
    state.currentPrompt = text;
    
    const pending = createThinkingBox(null);
    state.pendingBox = pending;
    
    await sendRequirementsRequest(text);
  });

  // Edit requirements button
  if (editRequirementsBtn) {
    editRequirementsBtn.addEventListener('click', openEditorModal);
  }

  // Confirm requirements button
  if (confirmRequirementsBtn) {
    confirmRequirementsBtn.addEventListener('click', () => {
      if (!state.currentBundle?.prompt_id) return;
      const params = new URLSearchParams({
        project_id: state.projectId,
        prompt_id: state.currentBundle.prompt_id
      });
      window.location.href = `/tasks.html?${params.toString()}`;
    });
  }

  // GitLab connect button
  const gitlabConnectBtn = document.getElementById('gitlabConnectBtn');
  if (gitlabConnectBtn) {
    gitlabConnectBtn.addEventListener('click', () => {
      if (state.gitlab.configured) sharedStartGitLabSSO(state.config);
    });
  }

  // Editor modal buttons
  const editorClose = document.getElementById('editorModalClose');
  const editorCancel = document.getElementById('editorCancel');
  const editorSave = document.getElementById('editorSave');
  
  if (editorClose) editorClose.addEventListener('click', closeEditorModal);
  if (editorCancel) editorCancel.addEventListener('click', closeEditorModal);
  if (editorSave) editorSave.addEventListener('click', saveEditorChanges);
}

// ============================================================================
// SSE Subscription
// ============================================================================

function subscribeSSE() {
  sharedConnectSSE(state, {
    'ai_requirements_progress': (evt) => {
      try {
        const msg = JSON.parse(evt.data);
        if (!state.projectId || String(state.projectId) !== String(msg.project_id)) return;
        
        projectStatusEl.innerHTML = '';
        projectStatusEl.appendChild(getStatusBadge(msg.status));
        
        const md = msg.details_md || msg.thought_summary || '...';
        const pid = msg.prompt_id || null;
        const box = getOrCreateBoxForPromptId(pid);
        if (box) box.appendMarkdown(md);
      } catch (err) {
        console.error('Failed to process ai_requirements_progress event:', err);
      }
    }
  });
}

// ============================================================================
// Periodic GitLab Status Check
// ============================================================================

function startPeriodicGitLabStatusCheck() {
  // Check GitLab status every 5 minutes
  const intervalId = setInterval(() => {
    fetchGitLabStatus(state, state.config).catch(err => {
      console.error('Periodic GitLab status check failed:', err);
    });
  }, 5 * 60 * 1000); // 5 minutes
  
  // Clear interval on page unload
  window.addEventListener('beforeunload', () => {
    clearInterval(intervalId);
  });
  
  console.log('Started periodic GitLab status checks (every 5 minutes)');
}

// ============================================================================
// Initialization
// ============================================================================

async function init() {
  // Get DOM elements
  chatOutput = document.getElementById('chatOutput');
  chatForm = document.getElementById('chatForm');
  chatInput = document.getElementById('chatInput');
  projectTitleEl = document.getElementById('chatProjectTitle');
  projectStatusEl = document.getElementById('chatProjectStatus');
  requirementsContent = document.getElementById('requirementsContent');
  requirementsSummary = document.getElementById('requirementsSummary');
  editRequirementsBtn = document.getElementById('editRequirementsBtn');
  confirmRequirementsBtn = document.getElementById('confirmRequirementsBtn');

  console.log('Requirements.html initializing...');
  
  state.config = await fetchConfig();
  console.log('Config loaded:', state.config);
  
  const authenticated = await checkAuthStatus(state);
  if (!authenticated) return;
  
  await loadProject();
  console.log('Project loaded:', state.project);
  
  // Fetch GitLab status
  try { 
    await fetchGitLabStatus(state, state.config);
    startPeriodicGitLabStatusCheck();
  } catch {}
  
  setupInputBehavior();
  subscribeSSE();
  
  console.log('Requirements.html initialized successfully');
}

// Start
init().catch(console.error);

