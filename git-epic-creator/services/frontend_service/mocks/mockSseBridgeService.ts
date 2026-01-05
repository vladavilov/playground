import http from "node:http";
import { URL } from "node:url";

import { subscribeServerSse } from "./serverSseBus";

type Server = http.Server;

declare global {
  // eslint-disable-next-line no-var
  var __mockSseBridgeService: { server: Server; origin: string } | undefined;
  // eslint-disable-next-line no-var
  var __mockSseBridgeServiceStarting: Promise<{ origin: string }> | undefined;
  // eslint-disable-next-line no-var
  var __mockSseBridgeServiceSseId: number | undefined;
}

function parseOriginPort(origin: string): { hostname: string; port: number } {
  const url = new URL(origin);
  const port = url.port ? Number.parseInt(url.port, 10) : null;
  // Note: When running tests, we allow port=0 to request an ephemeral port.
  const resolvedPort = port ?? (url.protocol === "https:" ? 443 : 80);
  return { hostname: url.hostname, port: resolvedPort };
}

function isValidPort(p: number): boolean {
  // Allow `0` to request an ephemeral port (used in tests).
  return Number.isFinite(p) && p >= 0 && p < 65536;
}

export function getMockSseBridgeOrigin(): string {
  // In tests, default to an ephemeral port to avoid clashes on dev machines.
  if (process.env.VITEST === "true" && !process.env.SSE_BRIDGE_ORIGIN) {
    return "http://127.0.0.1:0";
  }
  // In mock mode, do NOT default to the real sse-bridge-service port. Use a dedicated mock port
  // to avoid EADDRINUSE when a gateway is already running locally.
  return process.env.SSE_BRIDGE_ORIGIN?.trim() || "http://127.0.0.1:8017";
}

export async function ensureMockSseBridgeRunning(): Promise<{ origin: string }> {
  if (globalThis.__mockSseBridgeService) {
    return { origin: globalThis.__mockSseBridgeService.origin };
  }
  if (globalThis.__mockSseBridgeServiceStarting) {
    return await globalThis.__mockSseBridgeServiceStarting;
  }

  globalThis.__mockSseBridgeServiceStarting = (async () => {
    const requestedOrigin = getMockSseBridgeOrigin();
    const { hostname, port } = parseOriginPort(requestedOrigin);
    if (!isValidPort(port)) {
      throw new Error(`Invalid SSE_BRIDGE_ORIGIN port: ${requestedOrigin}`);
    }

    const server = http.createServer((req, res) => {
      const method = (req.method || "GET").toUpperCase();
      const path = req.url ? new URL(req.url, requestedOrigin).pathname : "/";

      // SSE endpoint (gateway contract)
      if (method === "GET" && path === "/events") {
        res.statusCode = 200;
        res.setHeader("Content-Type", "text/event-stream");
        res.setHeader("Cache-Control", "no-cache, no-transform");
        res.setHeader("Connection", "keep-alive");
        // Prevent proxy buffering (nginx)
        res.setHeader("X-Accel-Buffering", "no");

        // Avoid Node closing idle SSE sockets (which can cause EventSource reconnects and duplicates).
        try {
          req.socket.setTimeout(0);
        } catch {
          // ignore
        }
        try {
          res.socket?.setTimeout(0);
        } catch {
          // ignore
        }

        const send = (event: string, data: unknown) => {
          const id = (globalThis.__mockSseBridgeServiceSseId =
            (globalThis.__mockSseBridgeServiceSseId ?? 0) + 1);
          res.write(`id: ${id}\n`);
          res.write(`event: ${event}\n`);
          res.write(`data: ${JSON.stringify(data)}\n\n`);
        };

        // Connection established marker (matches `SSE_EVENTS.hello` name)
        send("hello", { ok: true });

        const unsubscribe = subscribeServerSse(({ event, data }) => send(event, data));
        const keepAlive = setInterval(() => {
          try {
            // Comment frame: ignored by EventSource but keeps connection alive.
            res.write(`: ping ${Date.now()}\n\n`);
          } catch {
            // ignore
          }
        }, 15_000);

        req.on("close", () => {
          clearInterval(keepAlive);
          unsubscribe();
        });
        return;
      }

      // Keep minimal "gateway" surface for anything that might be hit from the browser.
      res.statusCode = 404;
      res.setHeader("Content-Type", "text/plain");
      res.end("not found");
    });

    try {
      await new Promise<void>((resolve, reject) => {
        server.once("error", reject);
        server.listen(port, hostname, () => resolve());
      });
    } catch (e) {
      // If something else is already bound to that port (e.g. a real sse-bridge-service),
      // don't crash SSR reload. Just assume the gateway exists.
      if (e && typeof e === "object" && "code" in e && (e as any).code === "EADDRINUSE") {
        return { origin: requestedOrigin };
      }
      throw e;
    }

    const addr = server.address();
    const actualPort =
      addr && typeof addr === "object" && "port" in addr && typeof addr.port === "number"
        ? addr.port
        : port;
    const actualOrigin = `http://${hostname}:${actualPort}`;

    globalThis.__mockSseBridgeService = { server, origin: actualOrigin };
    return { origin: actualOrigin };
  })();

  try {
    return await globalThis.__mockSseBridgeServiceStarting;
  } finally {
    // clear startup latch (server instance is cached separately)
    globalThis.__mockSseBridgeServiceStarting = undefined;
  }
}


