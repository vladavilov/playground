import { isRouteErrorResponse, Link, useRouteError } from "react-router";

export function RouteErrorBoundary() {
  const err = useRouteError();

  if (isRouteErrorResponse(err)) {
    return (
      <main className="mx-auto max-w-3xl p-6">
        <h1 className="text-xl font-semibold">Something went wrong</h1>
        <p className="mt-2 text-sm text-muted-foreground">
          {err.status} {err.statusText}
        </p>
        <pre className="mt-4 overflow-auto rounded-md border border-border bg-muted p-3 text-xs">
          {typeof err.data === "string" ? err.data : JSON.stringify(err.data, null, 2)}
        </pre>
        <div className="mt-4">
          <Link className="underline" to="/projects">
            Back to projects
          </Link>
        </div>
      </main>
    );
  }

  return (
    <main className="mx-auto max-w-3xl p-6">
      <h1 className="text-xl font-semibold">Unexpected error</h1>
      <pre className="mt-4 overflow-auto rounded-md border border-border bg-muted p-3 text-xs">
        {err instanceof Error ? err.message : String(err)}
      </pre>
      <div className="mt-4">
        <Link className="underline" to="/projects">
          Back to projects
        </Link>
      </div>
    </main>
  );
}



