import { index, route } from "@react-router/dev/routes";
import type { RouteConfig } from "@react-router/dev/routes";

export default [
  index("./routes/_index.tsx"),
  route("favicon.ico", "./routes/favicon.ico.ts"),
  route(".well-known/appspecific/com.chrome.devtools.json", "./routes/chrome-devtools.json.ts"),
  route("projects", "./routes/projects.tsx"),
  route(
    "projects/:projectId/requirements",
    "./routes/projects.$projectId.requirements.tsx",
  ),
  route("projects/:projectId/tasks", "./routes/projects.$projectId.tasks.tsx"),
] satisfies RouteConfig;



