import type { Route } from "./+types/favicon.ico";

export async function loader() {
  // Avoid noisy dev logs from browsers/extensions requesting /favicon.ico.
  return new Response(null, { status: 204 });
}

export default function FaviconRoute(_props: Route.ComponentProps) {
  return null;
}


