import { ChevronRight, FolderKanban } from "lucide-react";
import { Link, useLocation, useMatches } from "react-router";

import { cn } from "../lib/cn";
import { isRecord } from "../lib/guards";

type BreadcrumbItem = {
  label: string;
  to?: string;
};

function getProjectCrumb(matches: ReturnType<typeof useMatches>): { projectId: string; projectName: string } | null {
  for (const m of matches) {
    const data: unknown = m.data;
    if (!isRecord(data)) continue;

    const projectId = typeof data.projectId === "string" ? data.projectId : null;
    const project = isRecord(data.project) ? data.project : null;
    const projectName = project && typeof project.name === "string" ? project.name : null;
    if (projectId && projectName) return { projectId, projectName };
  }
  return null;
}

export function Breadcrumbs(props: { className?: string }) {
  const matches = useMatches();
  const location = useLocation();

  const path = location.pathname;
  if (!path.startsWith("/projects")) return null;
  const sp = new URLSearchParams(location.search);

  const items: BreadcrumbItem[] = [{ label: "Projects", to: "/projects" }];
  const project = getProjectCrumb(matches);
  if (project) {
    items.push({ label: project.projectName, to: `/projects?selected=${encodeURIComponent(project.projectId)}` });
  }

  if (path.includes("/requirements")) items.push({ label: "Requirements" });
  else if (path.includes("/tasks")) {
    // Only show the intermediate crumb when user navigated from Requirements → Tasks.
    if (sp.get("from") === "requirements" && project) {
      items.push({ label: "Requirements", to: `/projects/${encodeURIComponent(project.projectId)}/requirements` });
    }
    items.push({ label: "Tasks" });
  }
  else if (path === "/projects" || path === "/projects/") items.push({ label: "Projects" });

  // If we only have a single meaningful crumb, hide (keeps Projects page clean).
  const meaningful = items.filter((i, idx) => idx === items.length - 1 || i.to);
  if (meaningful.length <= 1) return null;

  return (
    <nav aria-label="Breadcrumb" className={cn("flex items-center gap-2 text-sm", props.className)}>
      <FolderKanban className="h-4 w-4 text-muted-foreground" />
      {items.map((item, idx) => {
        const isLast = idx === items.length - 1;
        return (
          <div key={`${item.label}-${idx}`} className="flex items-center gap-2">
            {item.to && !isLast ? (
              <Link to={item.to} className="font-medium text-muted-foreground hover:text-foreground">
                {item.label}
              </Link>
            ) : (
              <span className={cn("font-medium", isLast ? "text-foreground" : "text-muted-foreground")}>
                {item.label}
              </span>
            )}
            {isLast ? null : <ChevronRight className="h-4 w-4 text-muted-foreground/70" />}
          </div>
        );
      })}
    </nav>
  );
}


