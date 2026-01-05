import type { Route } from "./+types/chrome-devtools.json";

export async function loader() {
  // Chrome (and extensions) may request this; return a tiny JSON instead of a route error.
  return Response.json({}, { status: 200 });
}

export default function ChromeDevtoolsRoute(_props: Route.ComponentProps) {
  return null;
}


