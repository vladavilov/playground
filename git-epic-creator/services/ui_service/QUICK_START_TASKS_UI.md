# Quick Start: Task Generation UI

## What's New

A new **Task Generation Chat Interface** has been added that allows users to generate, refine, and manage project backlogs (epics and issues) through an AI agent conversation.

## User Flow

```
┌─────────────┐    Score > 70%     ┌──────────────┐    AI Agent      ┌────────────────┐
│ chat.html   │  "Confirm Tasks"   │ tasks.html   │  generates      │ Backlog View   │
│ Requirements│───────────────────>│ Agent Chat   │───────────────> │ Epics & Tasks  │
└─────────────┘                    └──────────────┘                  └────────────────┘
                                           │                                 │
                                           │   Iterate & Refine              │
                                           │<────────────────────────────────┘
                                           │
                                           ▼
                                    Submit to GitLab
```

## Quick Demo Steps

1. **Start**: Open project console → Select a project → Click "Open Chat"

2. **Gather Requirements**: 
   ```
   Chat: "I need a user authentication system with email verification"
   ```

3. **Confirm**: When score reaches >70%, click the green button:
   ```
   ✓ Confirm & Create Tasks
   ```

4. **Generate Backlog**: You're now on `tasks.html` where the AI agent:
   - Analyzes requirements
   - Retrieves technical context from GraphRAG
   - Fetches existing GitLab epics/issues
   - Drafts comprehensive backlog
   - Maps duplicates
   - Evaluates quality

5. **Review**: See generated epics and tasks in the right panel with:
   - Epic descriptions
   - Task breakdowns
   - Acceptance criteria
   - Duplicate warnings (if any exist in GitLab)

6. **Refine**: Continue chatting:
   ```
   Chat: "Add a password reset epic"
   Chat: "Make task 3 more specific"
   ```

7. **Edit**: Click "✎ Edit All" to manually modify:
   - Epic/task titles and descriptions
   - GitLab link associations
   - Acceptance criteria

8. **Submit**: (Coming soon) Push to GitLab as actual epics/issues

## UI Features

### Connection Status Dashboard
- **Auth** 🟢: User authenticated
- **Tasks** 🟢: AI Tasks Service connected
- **Live** 🟢: Real-time events active

### Agent Thought Streams
Expandable boxes showing AI reasoning:
```
┌─ Agent thought stream ──────────────────┐
│ • Analyzing requirements...            │
│ • Retrieved 5 technical documents      │
│ • Found 2 similar epics in GitLab      │
│ • Drafting backlog structure...        │
└────────────────────────────────────────┘
```

### Duplicate Detection
```
⚠️ Similar items found:
• epic: #123 (87% match) → View in GitLab
```

## API Endpoints

### Frontend Configuration
```javascript
GET /config
Response:
{
  "aiTasksApiBase": "/tasks",
  ...
}
```

### Generate Backlog
```javascript
POST /tasks/tasks/generate
Request:
{
  "project_id": "uuid",
  "message": "User requirements text",
  "prompt_id": "uuid (optional for iteration)"
}

Response:
{
  "prompt_id": "uuid",
  "epics": [
    {
      "id": "epic-1",
      "title": "User Authentication",
      "description": "...",
      "tasks": [
        {
          "id": "task-1",
          "title": "Login form",
          "description": "...",
          "acceptance_criteria": ["Given...", "When...", "Then..."],
          "dependencies": [],
          "similar": [...]
        }
      ],
      "similar": [...]
    }
  ],
  "assumptions": [...],
  "risks": [...],
  "score": 0.82
}
```

### Real-Time Events
```javascript
EventSource: /events
Event Type: ai_tasks_progress

Event Data:
{
  "message_type": "ai_tasks_progress",
  "project_id": "uuid",
  "prompt_id": "uuid",
  "status": "drafting_backlog",
  "thought_summary": "Creating user story structure...",
  "score": 0.75,
  "timestamp": "2025-10-02T..."
}
```

## Files Changed

### Backend
- `services/shared/src/configuration/http_client_config.py` - Added AI_TASKS_SERVICE_URL
- `services/ui_service/src/routers/proxy_router.py` - Added /tasks proxy route
- `docker-compose.env` - Added AI_TASKS_SERVICE_URL=http://ai-tasks-service:8000

### Frontend (New)
- `services/ui_service/src/static/tasks.html` - Main task generation page
- `services/ui_service/src/static/tasks.js` - Complete functionality (750+ lines)

### Frontend (Modified)
- `services/ui_service/src/static/chat.html` - Added "Confirm & Create Tasks" button

## Environment Setup

Ensure `docker-compose.env` has:
```env
AI_TASKS_SERVICE_URL=http://ai-tasks-service:8000
```

No other configuration needed - works out of the box!

## Troubleshooting

### Connection Status Shows Red
- Check if `ai-tasks-service` is running: `docker-compose ps`
- Verify environment variable: `echo $AI_TASKS_SERVICE_URL`
- Check logs: `docker-compose logs ai-tasks-service`

### "Not authenticated" Error
- Session expired - refresh page to re-login
- Check auth service: `docker-compose logs mock-auth-service`

### Tasks Not Generating
- Open browser console (F12) for detailed errors
- Verify project_id in URL: `tasks.html?project_id=xxx`
- Check Redis is running: `docker-compose ps redis`

### Thinking Box Stuck on "Thinking..."
- SSE connection may have dropped
- Refresh page to reconnect
- Check network tab for `/events` connection

## Next Steps

1. **Test the Flow**: Go through the complete requirements → tasks workflow
2. **Iterate**: Try refining the backlog with follow-up messages
3. **Review Quality**: Check duplicate detection accuracy
4. **GitLab Integration**: (Future) Implement actual GitLab push functionality

## Support

For detailed implementation information, see:
- `TASKS_UI_IMPLEMENTATION.md` - Full technical documentation
- `services/ai_tasks_service/README.md` - Backend service details
- `services/ai_tasks_service/IMPLEMENTATION_SUMMARY.md` - API specifications

---

**Happy Task Generation! 🚀**

