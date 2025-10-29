'use strict';

// Import shared utilities
import {
  escapeHtml as esc,
  scrollToBottom,
  getStatusBadge,
  checkAuthStatus as sharedCheckAuthStatus,
  fetchGitLabStatus as sharedFetchGitLabStatus,
  startGitLabSSO as sharedStartGitLabSSO,
  connectSSE as sharedConnectSSE,
  fetchConfig as sharedFetchConfig,
  TypewriterBox
} from './shared-ui.js';

// Application State
const state = {
  config: null,
  projectId: null,  // Internal project management service ID
  gitlabProjectId: null,  // GitLab project ID (resolved from project)
  promptId: null,
  authenticated: false,
  backlogBundle: null,
  rtEvents: { status: 'disconnected' },
  gitlab: { status: 'connecting', configured: false },
  evtSource: null,
  thinkingBoxes: [],
  boxesByPromptId: {},
  activeBox: null,
  pendingBox: null,
};

// ============================================================================
// Thinking Box (Using Shared TypewriterBox Component)
// ============================================================================

function createThinkingBox(promptId = null) {
  const chatOutput = document.getElementById('chatOutput');
  const box = new TypewriterBox(chatOutput, promptId);
  
  // Wrap the TypewriterBox instance to maintain compatibility with existing code
  const boxWrapper = {
    promptId: box.promptId,
    element: box.getElement(),
    setPromptId(newId) {
      box.setPromptId(newId);
      if (newId) {
        this.promptId = newId;
        state.boxesByPromptId[newId] = this;
      }
    },
    appendStream(text) {
      box.appendStream(text);
    },
    appendMarkdown(md) {
      box.appendMarkdown(md);
    },
    finish(status = 'ok') {
      box.finish(status);
    }
  };

  state.thinkingBoxes.push(boxWrapper);
  if (promptId) state.boxesByPromptId[promptId] = boxWrapper;
  state.activeBox = boxWrapper;
  return boxWrapper;
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
  
  appendSystemMessage('Changes saved. Submitting to GitLab...');
  
  // Submit to GitLab
  submitToGitLab();
}

async function submitToGitLab() {
  if (!state.backlogBundle || !state.projectId) {
    appendAssistantMessage('<div class="text-sm text-rose-700">Error: No backlog or project ID available</div>', 'System Error');
    return;
  }
  
  // Check if GitLab project ID is available
  if (!state.gitlabProjectId) {
    appendAssistantMessage('<div class="text-sm text-rose-700">Error: Project does not have a GitLab project ID. Please set the GitLab project path first.</div>', 'System Error');
    return;
  }
  
  try {
    // Transform backlog bundle to GitLab API format
    // Use gitlabProjectId for GitLab API calls
    const payload = {
      project_id: state.gitlabProjectId,  // GitLab project ID (for GitLab API)
      internal_project_id: state.projectId,  // Internal project management ID (for tracking)
      prompt_id: state.promptId || state.backlogBundle.prompt_id || '',
      epics: [],
      issues: []
    };
    
    // Process epics and flatten tasks to issues
    state.backlogBundle.epics.forEach((epic) => {
      // Add epic (without tasks)
      payload.epics.push({
        id: epic.similar && epic.similar.length > 0 ? epic.similar[0].id : null,
        title: epic.title,
        description: epic.description || '',
        labels: []
      });
      
      // Add tasks as issues
      if (epic.tasks && epic.tasks.length > 0) {
        epic.tasks.forEach((task) => {
          payload.issues.push({
            id: task.similar && task.similar.length > 0 ? task.similar[0].id : null,
            title: task.title,
            description: task.description || '',
            labels: [],
            epic_id: epic.similar && epic.similar.length > 0 ? epic.similar[0].id : null
          });
        });
      }
    });
    
    // Call GitLab client service via proxy
    // Use gitlabProjectId instead of internal projectId
    const base = state.config.gitlabApiBase.replace(/\/$/, '');
    const url = `${base}/projects/${state.gitlabProjectId}/apply-backlog`;
    
    const res = await fetch(url, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload)
    });
    
    // If 401 Unauthorized, refresh GitLab connection status immediately
    if (res.status === 401) {
      console.warn('Received 401 Unauthorized, refreshing GitLab status');
      try {
        await sharedFetchGitLabStatus(state, state.config);
      } catch (err) {
        console.error('Failed to refresh GitLab status after 401:', err);
      }
    }
    
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
    
    const response = await res.json();
    
    // Display results
    const epicsCreated = response.results.epics.filter(e => e.action === 'created').length;
    const epicsUpdated = response.results.epics.filter(e => e.action === 'updated').length;
    const issuesCreated = response.results.issues.filter(i => i.action === 'created').length;
    const issuesUpdated = response.results.issues.filter(i => i.action === 'updated').length;
    const errorCount = response.errors.length;
    
    let html = `<div class="text-sm">`;
    html += `<div class="font-semibold text-emerald-700 mb-2">✓ Backlog submitted to GitLab successfully!</div>`;
    html += `<div class="space-y-1 text-slate-700">`;
    html += `<div>• Epics: ${epicsCreated} created, ${epicsUpdated} updated</div>`;
    html += `<div>• Issues: ${issuesCreated} created, ${issuesUpdated} updated</div>`;
    if (errorCount > 0) {
      html += `<div class="text-rose-600 mt-2">⚠ ${errorCount} error(s) occurred</div>`;
    }
    html += `</div>`;
    
    // Add links to created items
    if (response.results.epics.length > 0 || response.results.issues.length > 0) {
      html += `<div class="mt-3 text-xs">`;
      if (response.results.epics.slice(0, 3).length > 0) {
        html += `<div class="font-semibold mb-1">Sample Epics:</div>`;
        response.results.epics.slice(0, 3).forEach(epic => {
          html += `<div>• <a href="${esc(epic.web_url)}" target="_blank" class="text-indigo-600 hover:underline">${epic.action} #${esc(epic.id)}</a></div>`;
        });
      }
      if (response.results.issues.slice(0, 3).length > 0) {
        html += `<div class="font-semibold mb-1 mt-2">Sample Issues:</div>`;
        response.results.issues.slice(0, 3).forEach(issue => {
          html += `<div>• <a href="${esc(issue.web_url)}" target="_blank" class="text-indigo-600 hover:underline">${issue.action} #${esc(issue.id)}</a></div>`;
        });
      }
      html += `</div>`;
    }
    
    html += `</div>`;
    appendAssistantMessage(html, 'GitLab Submission');
    
  } catch (e) {
    console.error('Failed to submit to GitLab:', e);
    const errorMsg = e.message || 'Unknown error occurred';
    appendAssistantMessage(`<div class="text-sm text-rose-700">Failed to submit to GitLab: ${esc(errorMsg)}</div>`, 'System Error');
  }
}

// ============================================================================
// API Communication
// ============================================================================

async function fetchConfig() {
  state.config = await sharedFetchConfig();
}

async function checkAuthStatus() {
  return await sharedCheckAuthStatus(state);
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
  
  try {
    const res = await fetch(`${base}/generate`, {
      method: 'POST',
      headers,
      body: JSON.stringify(payload),
    });
    
    // If 401 Unauthorized, refresh GitLab connection status immediately
    if (res.status === 401) {
      console.warn('Received 401 Unauthorized, refreshing GitLab status');
      try {
        await sharedFetchGitLabStatus(state, state.config);
      } catch (err) {
        console.error('Failed to refresh GitLab status after 401:', err);
      }
    }
    
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
    
    // Link pending box to prompt_id if it exists
    const pid = bundle.prompt_id;
    if (state.pendingBox && !state.pendingBox.promptId && pid) {
      console.log('[Tasks] Linking pending box to prompt_id:', pid);
      state.pendingBox.setPromptId(pid);
      state.pendingBox = null; // Clear pending box after linking
    }
    
    // Render response
    const html = `<div class="text-sm text-indigo-700">✓ Generated ${bundle.epics?.length || 0} epic(s) with ${bundle.epics?.reduce((s, e) => s + (e.tasks?.length || 0), 0) || 0} task(s)</div>`;
    appendAssistantMessage(html);
    
    renderBacklog(bundle);
    
    // Note: Don't finish the box here - let SSE events handle it
    // The box will be finished when status="completed" comes through SSE
    
  } catch (e) {
    console.error('Failed to generate backlog:', e);
    const errorMsg = e.message || 'Unknown error occurred';
    appendAssistantMessage(`<div class="text-sm text-rose-700">Error: ${esc(errorMsg)}</div>`, 'System Error');
    
    if (state.pendingBox) {
      state.pendingBox.finish('error');
      state.pendingBox = null;
    }
  }
}

// Add console logging for debugging
console.log('Tasks.js loaded successfully');

// ============================================================================
// GitLab Integration
// ============================================================================

async function fetchGitLabStatus() {
  await sharedFetchGitLabStatus(state, state.config);
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
  sharedStartGitLabSSO(state.config);
}

// ============================================================================
// SSE Subscription
// ============================================================================

function connectSSE() {
  sharedConnectSSE(state, {
    'ai_tasks_progress': (evt) => {
      try {
        const msg = JSON.parse(evt.data);
        console.log('[Tasks] SSE event received:', {
          project_id: msg.project_id,
          prompt_id: msg.prompt_id,
          status: msg.status,
          has_thought_summary: !!msg.thought_summary,
          has_details_md: !!msg.details_md
        });
        
        // Filter by project
        if (msg.project_id && state.projectId && String(msg.project_id) !== String(state.projectId)) {
          console.log('[Tasks] Ignoring event for different project');
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
        console.log('[Tasks] Got thinking box for prompt_id:', promptId, 'box exists:', !!box);
        
        if (box) {
          // Add thought message if present - prioritize details_md for richer content
          const thoughtText = msg.details_md || msg.thought_summary;
          if (thoughtText) {
            console.log('[Tasks] Adding message to box:', thoughtText.substring(0, 100));
            box.appendMarkdown(thoughtText);
          }
          
          // Finish the box when status is completed or failed
          const status = (msg.status || '').toLowerCase();
          if (status === 'completed') {
            console.log('[Tasks] Finishing box with success');
            box.finish('ok');
          } else if (status.includes('error') || status.includes('failed')) {
            console.log('[Tasks] Finishing box with error');
            box.finish('error');
          }
        }
        
      } catch (e) {
        console.error('Failed to process ai_tasks_progress event:', e);
      }
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
  
  // GitLab connect button
  const gitlabConnectBtn = document.getElementById('gitlabConnectBtn');
  if (gitlabConnectBtn) {
    gitlabConnectBtn.addEventListener('click', () => {
      if (state.gitlab.configured) startGitLabSSO();
    });
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
  
  // Fetch project details to get GitLab project ID
  try {
    const base = state.config.projectManagementApiBase.replace(/\/$/, '');
    const projectUrl = `${base}/projects/${state.projectId}`;
    const response = await fetch(projectUrl);
    
    if (response.ok) {
      const project = await response.json();
      state.gitlabProjectId = project.gitlab_project_id;
      
      if (!state.gitlabProjectId) {
        appendSystemMessage('Warning: Project does not have a GitLab project ID configured. GitLab features will be unavailable.');
        console.warn('Project missing gitlab_project_id:', project);
      } else {
        console.log('GitLab project ID loaded:', state.gitlabProjectId);
      }
    } else {
      console.error('Failed to fetch project details:', response.status);
      appendSystemMessage('Warning: Could not load project details. GitLab features may not work.');
    }
  } catch (err) {
    console.error('Error fetching project:', err);
    appendSystemMessage('Warning: Could not load project details. GitLab features may not work.');
  }
  
  connectSSE();
  
  // Fetch GitLab connection status
  try { 
    await fetchGitLabStatus();
    startPeriodicGitLabStatusCheck();
  } catch {}
  
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

