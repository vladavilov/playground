import { handlers } from "./handlers";

/**
 * Single mock entrypoint.
 *
 * - Called from `entry.client.tsx` and `entry.server.tsx` behind an env gate.
 * - Uses runtime environment detection to pick the right MSW adapter.
 */
export async function startMocks(): Promise<void> {
  // NOTE: Use Vite's compile-time SSR flag so the browser bundle can tree-shake
  // the Node/MSW server adapter import. `typeof window` is runtime and breaks
  // Vite dependency optimization (it tries to prebundle `msw/node` for the client).
  if (import.meta.env.SSR) {
    // In mock mode, the browser still connects to `/events` via a real EventSource
    // (socket-like connection). Provide a minimal mock sse-bridge-service that
    // exposes `/events` and streams server-emitted mock SSE messages.
    const { ensureMockSseBridgeRunning } = await import("./mockSseBridgeService");
    await ensureMockSseBridgeRunning();

    const { setupServer } = await import("msw/node");
    const server = setupServer(...handlers);
    server.listen({ onUnhandledRequest: "bypass" });
    return;
  }

  const { setupWorker } = await import("msw/browser");
  const worker = setupWorker(...handlers);
  await worker.start({ onUnhandledRequest: "bypass" });
}


