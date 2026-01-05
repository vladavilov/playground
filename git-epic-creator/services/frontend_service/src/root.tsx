import type { Route } from "./+types/root";
import {
  Links,
  Meta,
  Outlet,
  Scripts,
  ScrollRestoration,
  useRouteLoaderData,
  useLocation,
  useMatches,
} from "react-router";

import "./styles/app.css";

import { ThemeToggle } from "./components/ThemeToggle";
import { Toaster } from "./components/ui/toaster";
import { RouteErrorBoundary } from "./components/RouteErrorBoundary";
import { Breadcrumbs } from "./components/Breadcrumbs";
import { StatusPill } from "./components/ui/status-pill";
import { parseThemeCookie, serializeThemeCookie, type Theme } from "./lib/theme";
import { redirect } from "react-router";
import { useEffect, useMemo, useRef, useState } from "react";
import { useSseBridge } from "./sse/useSseBridge";
import { useUiStore } from "./state/store";
import { GitBranch, LogOut, PlugZap, WifiOff } from "lucide-react";
import { UI_CONFIG, type UiConfig } from "./lib/uiConfig";
import { uiFetch } from "./lib/uiFetch.server";
import { cn } from "./lib/cn";
import { isRecord } from "./lib/guards";

type AuthMe = { authenticated: boolean; username: string | null };
type GitlabStatus = { connected: boolean; configured: boolean };

type RootLoaderData = {
  theme: Theme;
  auth: AuthMe;
  config: UiConfig;
  gitlabStatus: GitlabStatus;
};

export async function loader({ request }: Route.LoaderArgs) {
  const theme = parseThemeCookie(request.headers.get("cookie"));
  const url = new URL(request.url);

  const authRes = await uiFetch({ request, path: "/auth/me" });
  const authRaw: unknown = await authRes.json();
  if (!isRecord(authRaw) || typeof authRaw.authenticated !== "boolean") {
    throw new Response("Invalid /auth/me response", { status: 502 });
  }
  const auth: AuthMe = {
    authenticated: authRaw.authenticated,
    username: authRaw.username === null || typeof authRaw.username === "string" ? authRaw.username : null,
  };

  if (!auth.authenticated) {
    throw redirect(
      `/auth/login?redirect_uri=${encodeURIComponent(`${url.pathname}${url.search}`)}`,
    );
  }

  const config = UI_CONFIG satisfies UiConfig;

  const gitlabRes = await uiFetch({ request, path: "/auth/gitlab/status" });
  const gitlabRaw: unknown = await gitlabRes.json();
  if (!isRecord(gitlabRaw) || typeof gitlabRaw.connected !== "boolean" || typeof gitlabRaw.configured !== "boolean") {
    throw new Response("Invalid /auth/gitlab/status response", { status: 502 });
  }
  const gitlabStatus: GitlabStatus = {
    connected: gitlabRaw.connected,
    configured: gitlabRaw.configured,
  };

  return { theme, auth, config, gitlabStatus } satisfies RootLoaderData;
}

export async function action({ request }: Route.ActionArgs) {
  const formData = await request.formData();
  const theme = formData.get("theme");
  if (theme !== "light" && theme !== "dark") {
    return Response.json({ ok: false }, { status: 400 });
  }

  return Response.json(
    { ok: true },
    { headers: { "Set-Cookie": serializeThemeCookie(theme) } },
  );
}

export function Layout(props: { children: React.ReactNode }) {
  const data = useRouteLoaderData("root") as RootLoaderData | undefined;
  const theme = data?.theme ?? "light";
  const username = data?.auth.username ?? "unknown";
  const location = useLocation();
  const matches = useMatches();
  const headerRef = useRef<HTMLElement | null>(null);

  const projectId = useMemo(() => {
    for (const m of matches) {
      const d: unknown = m.data;
      if (!isRecord(d)) continue;
      const projectId = typeof d.projectId === "string" ? d.projectId : null;
      if (projectId) return projectId;
    }
    // Projects route stores selection in query string.
    if (location.pathname === "/projects") {
      const sp = new URLSearchParams(location.search);
      const selected = sp.get("selected");
      if (selected) return selected;
    }
    return null;
  }, [location.pathname, location.search, matches]);

  const [rtNonce, setRtNonce] = useState(0);
  const rtStatus = useUiStore((s) => s.sseStatus);
  const rtDisabled = !projectId;

  const gitlab = data?.gitlabStatus;
  const config = data?.config;
  const redirectUri = useMemo(() => {
    // Keep SSR + client identical to avoid hydration mismatches.
    // We only need a path+query (the server knows the origin).
    return `${location.pathname}${location.search}`;
  }, [location.pathname, location.search]);
  const gitlabAuthorizeHref =
    gitlab && config && gitlab.configured && !gitlab.connected
      ? `${config.gitlabAuthAuthorizePath}?redirect_uri=${encodeURIComponent(redirectUri)}`
      : null;

  const rtVariant =
    rtStatus === "connected" ? "success" : rtStatus === "connecting" ? "warning" : "danger";

  const lockViewportScroll = useMemo(() => {
    // Tasks/requirements pages need internal scrolling (chat + editors), not browser scroll.
    const p = location.pathname;
    return p.includes("/requirements") || p.includes("/tasks");
  }, [location.pathname]);

  useEffect(() => {
    const header = headerRef.current;
    if (!header) return;

    const setVar = () => {
      const h = header.getBoundingClientRect().height;
      if (Number.isFinite(h) && h > 0) {
        document.documentElement.style.setProperty("--app-header-h", `${Math.round(h)}px`);
      }
    };

    setVar();

    if (typeof ResizeObserver !== "undefined") {
      const ro = new ResizeObserver(() => setVar());
      ro.observe(header);
      return () => ro.disconnect();
    }

    const onResize = () => setVar();
    window.addEventListener("resize", onResize);
    return () => window.removeEventListener("resize", onResize);
  }, []);

  return (
    <html lang="en" className={theme === "dark" ? "dark" : ""}>
      <head>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <Meta />
        <Links />
      </head>
      <body>
        <div
          className={cn(
            "flex flex-col bg-background text-foreground",
            lockViewportScroll ? "h-dvh overflow-hidden" : "min-h-dvh",
          )}
        >
          <div className="pointer-events-none fixed inset-0 -z-10 bg-[radial-gradient(50%_40%_at_50%_0%,color-mix(in_oklab,var(--primary)_18%,transparent),transparent)]" />
          <header
            ref={headerRef}
            className="sticky top-0 z-60 border-b border-border bg-background/70 backdrop-blur supports-[backdrop-filter]:bg-background/50"
          >
            <div className="mx-auto flex max-w-6xl items-center justify-between px-6 py-3">
              <div className="flex items-center gap-3">
                <div className="flex items-center gap-2">
                  <div className="h-8 w-8 rounded-lg bg-primary/15 ring-1 ring-primary/20" />
                  <div>
                    <div className="text-sm font-semibold tracking-tight">UBS Workflow</div>
                    <div className="mt-0.5 flex flex-wrap items-center gap-2 text-xs text-muted-foreground">
                      <span>{username}</span>
                      <span>·</span>
                      {gitlab?.connected ? (
                        <StatusPill variant="success" icon={<GitBranch className="h-3.5 w-3.5" />}>
                          GitLab connected
                        </StatusPill>
                      ) : gitlab?.configured ? (
                        <StatusPill
                          variant="danger"
                          href={gitlabAuthorizeHref ?? undefined}
                          title="Connect GitLab (SSO)"
                          icon={<GitBranch className="h-3.5 w-3.5" />}
                        >
                          Connect GitLab
                        </StatusPill>
                      ) : (
                        <StatusPill variant="neutral" icon={<GitBranch className="h-3.5 w-3.5" />}>
                          GitLab not configured
                        </StatusPill>
                      )}

                      <StatusPill
                        variant={rtVariant}
                        disabled={rtDisabled}
                        title={rtDisabled ? "Select a project to connect realtime events" : "Reconnect realtime events"}
                        onClick={rtDisabled ? undefined : () => setRtNonce((x) => x + 1)}
                        icon={
                          rtStatus === "connected" ? (
                            <PlugZap className="h-3.5 w-3.5" />
                          ) : (
                            <WifiOff className="h-3.5 w-3.5" />
                          )
                        }
                      >
                        RT {rtStatus}
                      </StatusPill>
                    </div>
                  </div>
                </div>
              </div>
              <div className="flex items-center gap-2">
                <ThemeToggle theme={theme} />
                <form method="post" action="/auth/logout">
                  <button
                    className="inline-flex items-center gap-2 rounded-full border border-border bg-background px-3 py-1 text-xs font-medium hover:bg-accent"
                    title="Logout"
                  >
                    <LogOut className="h-3.5 w-3.5 text-muted-foreground" />
                    Logout
                  </button>
                </form>
              </div>
            </div>
          </header>
          <main className="mx-auto flex w-full max-w-6xl flex-1 flex-col px-6 py-4 min-h-0">
            <Breadcrumbs className="mb-4 shrink-0" />
            <div className="flex min-h-0 flex-1 flex-col">{props.children}</div>
          </main>
        </div>
        <RealtimeConnector key={`${projectId ?? "none"}-${rtNonce}`} projectId={projectId} />
        <Toaster />
        <ScrollRestoration />
        <Scripts />
      </body>
    </html>
  );
}

function RealtimeConnector(props: { projectId: string | null }) {
  useSseBridge(props.projectId);
  return null;
}

export function ErrorBoundary() {
  return <RouteErrorBoundary />;
}

export default function Root() {
  return <Outlet />;
}


