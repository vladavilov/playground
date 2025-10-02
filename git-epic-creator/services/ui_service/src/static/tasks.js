'use strict';

// Application State
const state = {
  config: null,
  projectId: null,
  promptId: null,
  authenticated: false,
  backlogBundle: null,
  rtEvents: { status: 'disconnected' },
  tasksService: { status: 'disconnected' },
  evtSource: null,
  thinkingBoxes: [],
  boxesByPromptId: {},
  activeBox: null,
  pendingBox: null,
  loadingCount: 0,
};

// ============================================================================
// Utility Functions
// ============================================================================

function showLoading() {
  state.loadingCount += 1;
  const overlay = document.getElementById('loadingOverlay');
  if (overlay && state.loadingCount > 0) {
    overlay.classList.remove('hidden');
    overlay.classList.add('flex');
  }
}

function hideLoading() {
  state.loadingCount = Math.max(0, state.loadingCount - 1);
  const overlay = document.getElementById('loadingOverlay');
  if (overlay && state.loadingCount === 0) {
    overlay.classList.add('hidden');
    overlay.classList.remove('flex');
  }
}

function scrollToBottom(element) {
  if (element) element.scrollTop = element.scrollHeight;
}

function esc(s) {
  return String(s ?? '').replace(/[&<>]/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));
}

function renderMarkdown(md) {
  let html = esc(md || '');
  html = html
    .replace(/^###\s+(.*)$/gm, '<h3 class="text-base font-semibold mt-3 text-slate-800">$1</h3>')
    .replace(/^##\s+(.*)$/gm, '<h2 class="text-lg font-semibold mt-4 text-slate-800">$1</h2>')
    .replace(/^#\s+(.*)$/gm, '<h1 class="text-xl font-bold mt-5 text-slate-800">$1</h1>')
    .replace(/\*\*(.*?)\*\*/g, '<strong class="font-semibold">$1</strong>')
    .replace(/\*(.*?)\*/g, '<em class="italic">$1</em>')
    .replace(/`([^`]+)`/g, '<code class="bg-slate-100 px-1.5 py-0.5 rounded text-xs font-mono">$1</code>')
    .replace(/```([\s\S]*?)```/g, '<pre class="bg-slate-900 text-slate-100 p-3 rounded-lg overflow-auto text-xs font-mono mt-2"><code>$1</code></pre>')
    .replace(/^\s*[-*]\s+(.*)$/gm, '<li class="ml-6 list-disc text-slate-700">$1</li>')
    .replace(/\[(.+?)\]\((https?:[^\s]+)\)/g, '<a href="$2" target="_blank" class="text-sky-600 underline hover:text-sky-700">$1</a>');
  html = html.replace(/(<li[\s\S]*?<\/li>)/g, '<ul class="space-y-1">$1</ul>');
  return html;
}

function getStatusBadge(status) {
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
  else if (s === 'completed') cls = 'bg-emerald-100 text-emerald-700';
  else if (s.includes('error') || s.includes('failed')) cls = 'bg-rose-100 text-rose-700';
  
  const span = document.createElement('span');
  span.className = base + cls;
  span.textContent = s || 'unknown';
  return span;
}

// ============================================================================
// Connection Status Management
// ============================================================================

function updateConnectionStatus() {
  // Auth Indicator
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

  // Tasks Service Indicator
  const tasksIndicator = document.getElementById('tasksServiceIndicator');
  if (tasksIndicator) {
    const s = state.tasksService.status;
    if (s === 'connected') {
      tasksIndicator.className = 'w-2 h-2 rounded-full bg-emerald-500';
      tasksIndicator.title = 'AI Tasks Service connected';
    } else if (s === 'connecting') {
      tasksIndicator.className = 'w-2 h-2 rounded-full bg-blue-500 animate-pulse';
      tasksIndicator.title = 'Connecting...';
    } else {
      tasksIndicator.className = 'w-2 h-2 rounded-full bg-slate-400';
      tasksIndicator.title = 'Disconnected';
    }
  }

  // RT Events Indicator
  const rtIndicator = document.getElementById('rtEventsIndicator');
  if (rtIndicator) {
    const s = state.rtEvents.status;
    if (s === 'connected') {
      rtIndicator.className = 'w-2 h-2 rounded-full bg-emerald-500';
      rtIndicator.title = 'Real-time events connected';
    } else if (s === 'connecting') {
      rtIndicator.className = 'w-2 h-2 rounded-full bg-blue-500 animate-pulse';
      rtIndicator.title = 'Connecting...';
    } else {
      rtIndicator.className = 'w-2 h-2 rounded-full bg-slate-400';
      rtIndicator.title = 'Disconnected';
    }
  }
}

// ============================================================================
// Thinking Box (Agent Stream Visualization)
// ============================================================================

function createThinkingBox(promptId = null) {
  const chatOutput = document.getElementById('chatOutput');
  
  const wrap = document.createElement('details');
  wrap.className = 'thinking-box border border-indigo-200 rounded-lg p-4 bg-indigo-50/50 backdrop-blur-sm';
  wrap.open = true;

  const summary = document.createElement('summary');
  summary.className = 'cursor-pointer select-none text-sm font-medium text-indigo-700 flex items-center gap-2';
  
  const icon = document.createElement('span');
  icon.className = 'thinking-indicator inline-block w-2 h-2 rounded-full bg-indigo-500';
  
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
    icon: icon,
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
      line.className = 'text-sm';
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
  const chatOutput = document.getElementById('chatOutput');
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

function appendAssistantMessage(html, meta = 'AI Agent') {
  const chatOutput = document.getElementById('chatOutput');
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
  const chatOutput = document.getElementById('chatOutput');
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
// Backlog Rendering
// ============================================================================

function renderBacklog(bundle) {
  const content = document.getElementById('backlogContent');
  const summary = document.getElementById('backlogSummary');
  const editBtn = document.getElementById('editBacklogBtn');
  
  if (!bundle || !bundle.epics || bundle.epics.length === 0) {
    content.innerHTML = `
      <div class="flex flex-col items-center justify-center h-full text-center">
        <div class="w-16 h-16 rounded-full bg-slate-200 flex items-center justify-center mb-4">
          <svg class="w-8 h-8 text-slate-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2"></path>
          </svg>
        </div>
        <p class="text-slate-500">No epics generated yet</p>
      </div>
    `;
    summary.textContent = 'No backlog yet';
    editBtn.classList.add('hidden');
    return;
  }

  // Update summary
  const epicCount = bundle.epics.length;
  const taskCount = bundle.epics.reduce((sum, epic) => sum + (epic.tasks?.length || 0), 0);
  summary.textContent = `${epicCount} epic${epicCount !== 1 ? 's' : ''}, ${taskCount} task${taskCount !== 1 ? 's' : ''}`;
  editBtn.classList.remove('hidden');

  // Render epics
  content.innerHTML = '';
  bundle.epics.forEach((epic, epicIdx) => {
    const epicCard = document.createElement('div');
    epicCard.className = 'epic-card bg-white border border-slate-200 rounded-lg p-4 mb-4 shadow-sm';
    
    let epicHtml = `
      <div class="flex items-start justify-between gap-3 mb-3">
        <div class="flex-1">
          <h3 class="font-semibold text-slate-800">${esc(epic.title)}</h3>
          <p class="text-sm text-slate-600 mt-1">${esc(epic.description)}</p>
        </div>
        <span class="px-2 py-1 bg-indigo-100 text-indigo-700 rounded text-xs font-medium whitespace-nowrap">Epic ${epicIdx + 1}</span>
      </div>
    `;

    // Similar matches
    if (epic.similar && epic.similar.length > 0) {
      epicHtml += `<div class="mb-3 p-2 bg-amber-50 border border-amber-200 rounded text-xs">
        <div class="font-medium text-amber-800 mb-1">⚠️ Similar items found:</div>`;
      epic.similar.forEach(sim => {
        const simUrl = sim.url || '#';
        epicHtml += `<div class="text-amber-700">• ${esc(sim.kind)}: <a href="${esc(simUrl)}" target="_blank" class="underline">${esc(sim.id)}</a> (${Math.round((sim.similarity || 0) * 100)}% match)</div>`;
      });
      epicHtml += `</div>`;
    }

    // Tasks
    if (epic.tasks && epic.tasks.length > 0) {
      epicHtml += `<div class="space-y-2">`;
      epic.tasks.forEach((task, taskIdx) => {
        epicHtml += `
          <div class="task-item border border-slate-200 rounded-lg p-3 bg-slate-50">
            <div class="flex items-start justify-between gap-2">
              <div class="flex-1">
                <div class="font-medium text-sm text-slate-800">${esc(task.title)}</div>
                <div class="text-xs text-slate-600 mt-1">${esc(task.description)}</div>
              </div>
              <span class="px-1.5 py-0.5 bg-slate-200 text-slate-700 rounded text-xs whitespace-nowrap">T${taskIdx + 1}</span>
            </div>
        `;
        
        // Acceptance criteria
        if (task.acceptance_criteria && task.acceptance_criteria.length > 0) {
          epicHtml += `<div class="mt-2 text-xs"><div class="font-medium text-slate-700 mb-1">Acceptance Criteria:</div><ul class="space-y-0.5">`;
          task.acceptance_criteria.forEach(ac => {
            epicHtml += `<li class="ml-4 list-disc text-slate-600">${esc(ac)}</li>`;
          });
          epicHtml += `</ul></div>`;
        }

        // Dependencies
        if (task.dependencies && task.dependencies.length > 0) {
          epicHtml += `<div class="mt-2 text-xs text-slate-600">Dependencies: ${task.dependencies.map(d => esc(d)).join(', ')}</div>`;
        }

        // Similar matches
        if (task.similar && task.similar.length > 0) {
          epicHtml += `<div class="mt-2 p-2 bg-amber-50 border border-amber-200 rounded text-xs">
            <div class="font-medium text-amber-800 mb-1">Similar:</div>`;
          task.similar.forEach(sim => {
            const simUrl = sim.url || '#';
            epicHtml += `<div class="text-amber-700">• <a href="${esc(simUrl)}" target="_blank" class="underline">${esc(sim.id)}</a> (${Math.round((sim.similarity || 0) * 100)}%)</div>`;
          });
          epicHtml += `</div>`;
        }

        epicHtml += `</div>`;
      });
      epicHtml += `</div>`;
    }

    epicCard.innerHTML = epicHtml;
    content.appendChild(epicCard);
  });

  // Assumptions and Risks
  if ((bundle.assumptions && bundle.assumptions.length > 0) || (bundle.risks && bundle.risks.length > 0)) {
    const metaCard = document.createElement('div');
    metaCard.className = 'bg-white border border-slate-200 rounded-lg p-4 shadow-sm';
    let metaHtml = '';
    
    if (bundle.assumptions && bundle.assumptions.length > 0) {
      metaHtml += `<div class="mb-3"><div class="text-sm font-semibold text-slate-700 mb-1">Assumptions</div><ul class="space-y-1">`;
      bundle.assumptions.forEach(a => {
        metaHtml += `<li class="text-xs text-slate-600 ml-4 list-disc">${esc(a)}</li>`;
      });
      metaHtml += `</ul></div>`;
    }

    if (bundle.risks && bundle.risks.length > 0) {
      metaHtml += `<div><div class="text-sm font-semibold text-slate-700 mb-1">Risks</div><ul class="space-y-1">`;
      bundle.risks.forEach(r => {
        metaHtml += `<li class="text-xs text-rose-600 ml-4 list-disc">${esc(r)}</li>`;
      });
      metaHtml += `</ul></div>`;
    }

    metaCard.innerHTML = metaHtml;
    content.appendChild(metaCard);
  }

  // Score indicator
  if (bundle.score != null) {
    const scoreCard = document.createElement('div');
    scoreCard.className = 'mt-4 p-3 bg-gradient-to-r from-emerald-50 to-teal-50 border border-emerald-200 rounded-lg';
    const scoreValue = Math.round(bundle.score * 100);
    const scoreColor = scoreValue >= 75 ? 'emerald' : scoreValue >= 50 ? 'amber' : 'rose';
    scoreCard.innerHTML = `
      <div class="flex items-center justify-between">
        <span class="text-sm font-medium text-slate-700">Quality Score</span>
        <span class="px-3 py-1 bg-${scoreColor}-100 text-${scoreColor}-700 rounded-full text-sm font-semibold">${scoreValue}%</span>
      </div>
    `;
    content.appendChild(scoreCard);
  }
}

// ============================================================================
// Task Editor Modal
// ============================================================================

function openEditorModal() {
  if (!state.backlogBundle) return;
  
  const modal = document.getElementById('editorModal');
  const content = document.getElementById('editorContent');
  
  // Build editor UI
  let html = '';
  state.backlogBundle.epics.forEach((epic, epicIdx) => {
    html += `
      <div class="border border-slate-200 rounded-lg p-4 bg-white">
        <div class="mb-3">
          <label class="block text-sm font-medium text-slate-700 mb-1">Epic Title</label>
          <input type="text" class="w-full border border-slate-300 rounded-lg px-3 py-2 text-sm" value="${esc(epic.title)}" data-epic-idx="${epicIdx}" data-field="title" />
        </div>
        <div class="mb-3">
          <label class="block text-sm font-medium text-slate-700 mb-1">Epic Description</label>
          <textarea class="w-full border border-slate-300 rounded-lg px-3 py-2 text-sm" rows="2" data-epic-idx="${epicIdx}" data-field="description">${esc(epic.description)}</textarea>
        </div>
    `;

    // GitLab links for epic
    html += `
      <div class="mb-3">
        <label class="block text-sm font-medium text-slate-700 mb-1">GitLab Epic Links</label>
        <div class="space-y-2" data-epic-idx="${epicIdx}" data-links-container="epic">
    `;
    
    if (epic.similar && epic.similar.length > 0) {
      epic.similar.forEach((sim, simIdx) => {
        html += `
          <div class="flex items-center gap-2">
            <input type="text" class="flex-1 border border-slate-300 rounded px-2 py-1 text-xs" value="${esc(sim.url || '')}" placeholder="GitLab URL" data-sim-idx="${simIdx}" />
            <button class="px-2 py-1 bg-rose-100 text-rose-700 rounded text-xs hover:bg-rose-200" data-action="remove-link" data-sim-idx="${simIdx}">Remove</button>
          </div>
        `;
      });
    }
    
    html += `
          <button class="px-2 py-1 bg-indigo-100 text-indigo-700 rounded text-xs hover:bg-indigo-200" data-action="add-link">+ Add Link</button>
        </div>
      </div>
    `;

    // Tasks
    if (epic.tasks && epic.tasks.length > 0) {
      html += `<div class="mt-4 space-y-3">`;
      epic.tasks.forEach((task, taskIdx) => {
        html += `
          <div class="border border-slate-200 rounded-lg p-3 bg-slate-50">
            <div class="mb-2">
              <label class="block text-xs font-medium text-slate-700 mb-1">Task Title</label>
              <input type="text" class="w-full border border-slate-300 rounded px-2 py-1 text-xs" value="${esc(task.title)}" data-epic-idx="${epicIdx}" data-task-idx="${taskIdx}" data-field="title" />
            </div>
            <div class="mb-2">
              <label class="block text-xs font-medium text-slate-700 mb-1">Task Description</label>
              <textarea class="w-full border border-slate-300 rounded px-2 py-1 text-xs" rows="2" data-epic-idx="${epicIdx}" data-task-idx="${taskIdx}" data-field="description">${esc(task.description)}</textarea>
            </div>
          </div>
        `;
      });
      html += `</div>`;
    }

    html += `</div>`;
  });

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
  
  // Collect changes from form
  const updatedEpics = [...state.backlogBundle.epics];
  
  // Update epic fields
  content.querySelectorAll('input[data-epic-idx][data-field], textarea[data-epic-idx][data-field]').forEach(input => {
    const epicIdx = parseInt(input.getAttribute('data-epic-idx'));
    const taskIdxStr = input.getAttribute('data-task-idx');
    const field = input.getAttribute('data-field');
    
    if (taskIdxStr === null) {
      // Epic field
      updatedEpics[epicIdx][field] = input.value;
    } else {
      // Task field
      const taskIdx = parseInt(taskIdxStr);
      updatedEpics[epicIdx].tasks[taskIdx][field] = input.value;
    }
  });

  // Update state
  state.backlogBundle.epics = updatedEpics;
  
  // Re-render
  renderBacklog(state.backlogBundle);
  closeEditorModal();
  
  appendSystemMessage('Changes saved. Ready to submit to GitLab.');
}

// ============================================================================
// API Communication
// ============================================================================

async function fetchConfig() {
  const res = await fetch('/config');
  state.config = await res.json();
}

async function checkAuthStatus() {
  try {
    const res = await fetch('/auth/me');
    const data = await res.json();
    state.authenticated = data.authenticated || false;
    
    // Update UI
    const profile = document.getElementById('userProfile');
    const avatar = document.getElementById('userAvatar');
    if (profile) profile.textContent = data.username || 'User';
    if (avatar) avatar.textContent = (data.username || 'U')[0].toUpperCase();
    
    updateConnectionStatus();
    
    if (!state.authenticated) {
      window.location.href = '/auth/login';
      return false;
    }
    return true;
  } catch (e) {
    console.error('Auth check failed:', e);
    window.location.href = '/auth/login';
    return false;
  }
}

async function sendMessage(message) {
  if (!message || !message.trim()) return;
  
  const base = state.config.aiTasksApiBase.replace(/\/$/, '');
  const headers = { 'Content-Type': 'application/json' };
  
  const payload = {
    project_id: state.projectId,
    message: message.trim(),
  };
  
  if (state.promptId) payload.prompt_id = state.promptId;
  
  showLoading();
  state.tasksService.status = 'connecting';
  updateConnectionStatus();
  
  try {
    const res = await fetch(`${base}/tasks/generate`, {
      method: 'POST',
      headers,
      body: JSON.stringify(payload),
    });
    
    if (!res.ok) {
      let errorMsg = `HTTP ${res.status}`;
      try {
        const errorData = await res.json();
        if (errorData.detail) errorMsg += `: ${errorData.detail}`;
      } catch {
        const errorText = await res.text().catch(() => 'Unknown error');
        if (errorText) errorMsg += `: ${errorText}`;
      }
      throw new Error(errorMsg);
    }
    
    const bundle = await res.json();
    
    // Update state
    state.backlogBundle = bundle;
    state.promptId = bundle.prompt_id;
    state.tasksService.status = 'connected';
    updateConnectionStatus();
    
    // Render response
    const html = `<div class="text-sm text-indigo-700">✓ Generated ${bundle.epics?.length || 0} epic(s) with ${bundle.epics?.reduce((s, e) => s + (e.tasks?.length || 0), 0) || 0} task(s)</div>`;
    appendAssistantMessage(html);
    
    renderBacklog(bundle);
    
    // Finalize thinking box
    const box = getOrCreateBoxForPromptId(bundle.prompt_id);
    if (box) box.finish('ok');
    
  } catch (e) {
    console.error('Failed to generate backlog:', e);
    const errorMsg = e.message || 'Unknown error occurred';
    appendAssistantMessage(`<div class="text-sm text-rose-700">Error: ${esc(errorMsg)}</div>`, 'System Error');
    state.tasksService.status = 'disconnected';
    updateConnectionStatus();
    
    if (state.pendingBox) {
      state.pendingBox.finish('error');
      state.pendingBox = null;
    }
  } finally {
    hideLoading();
  }
}

// Add console logging for debugging
console.log('Tasks.js loaded successfully');

// ============================================================================
// SSE Subscription
// ============================================================================

function connectSSE() {
  try {
    if (state.evtSource) state.evtSource.close();
  } catch (e) {}
  
  state.evtSource = new EventSource('/events');
  
  state.evtSource.addEventListener('open', () => {
    state.rtEvents.status = 'connected';
    updateConnectionStatus();
  });
  
  state.evtSource.addEventListener('error', () => {
    state.rtEvents.status = 'connecting';
    updateConnectionStatus();
  });
  
  state.evtSource.addEventListener('hello', () => {
    state.rtEvents.status = 'connected';
    updateConnectionStatus();
  });
  
  // Listen for ai_tasks_progress events
  state.evtSource.addEventListener('ai_tasks_progress', (evt) => {
    try {
      const msg = JSON.parse(evt.data);
      
      // Filter by project
      if (msg.project_id && state.projectId && String(msg.project_id) !== String(state.projectId)) {
        return;
      }
      
      // Update status badge
      const statusEl = document.getElementById('taskProjectStatus');
      if (statusEl) {
        statusEl.innerHTML = '';
        statusEl.appendChild(getStatusBadge(msg.status));
      }
      
      // Route to thinking box
      const promptId = msg.prompt_id || null;
      const box = getOrCreateBoxForPromptId(promptId);
      
      const thoughtText = msg.thought_summary || msg.details_md || 'Processing...';
      if (box) box.appendMarkdown(thoughtText);
      
    } catch (e) {
      console.error('Failed to process ai_tasks_progress event:', e);
    }
  });
}

// ============================================================================
// Event Handlers
// ============================================================================

function setupEventHandlers() {
  // Chat form
  const chatForm = document.getElementById('chatForm');
  const chatInput = document.getElementById('chatInput');
  
  if (chatInput) {
    chatInput.addEventListener('keydown', (e) => {
      if (e.key === 'Enter' && !e.shiftKey) {
        e.preventDefault();
        chatForm.requestSubmit();
      }
    });
  }
  
  if (chatForm) {
    chatForm.addEventListener('submit', async (e) => {
      e.preventDefault();
      const text = chatInput.value.trim();
      if (!text) return;
      
      appendUserMessage(text);
      chatInput.value = '';
      
      // Create pending thinking box
      const pending = createThinkingBox(null);
      state.pendingBox = pending;
      
      await sendMessage(text);
    });
  }
  
  // Edit backlog button
  const editBtn = document.getElementById('editBacklogBtn');
  if (editBtn) {
    editBtn.addEventListener('click', openEditorModal);
  }
  
  // Editor modal
  const editorClose = document.getElementById('editorModalClose');
  const editorCancel = document.getElementById('editorCancel');
  const editorSave = document.getElementById('editorSave');
  
  if (editorClose) editorClose.addEventListener('click', closeEditorModal);
  if (editorCancel) editorCancel.addEventListener('click', closeEditorModal);
  if (editorSave) editorSave.addEventListener('click', saveEditorChanges);
}

// ============================================================================
// Initialization
// ============================================================================

async function init() {
  // Parse URL params
  const params = new URLSearchParams(window.location.search);
  state.projectId = params.get('project_id');
  state.promptId = params.get('prompt_id');
  
  if (!state.projectId) {
    appendSystemMessage('Error: No project ID provided');
    return;
  }
  
  // Setup
  setupEventHandlers();
  await fetchConfig();
  
  const authenticated = await checkAuthStatus();
  if (!authenticated) return;
  
  connectSSE();
  
  // Auto-send initial message if we have a prompt_id
  if (state.promptId) {
    const pending = createThinkingBox(state.promptId);
    state.pendingBox = pending;
    
    appendSystemMessage('Generating initial backlog from confirmed requirements...');
    await sendMessage('Generate epics and tasks based on the confirmed requirements.');
  } else {
    appendSystemMessage('Ready to generate tasks. Describe what you need.');
  }
}

// Start
init().catch(err => console.error('Initialization failed:', err));

