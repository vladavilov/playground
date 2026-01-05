export type ServerSseEnvelope = {
  event: string;
  data: unknown;
};

type Listener = (env: ServerSseEnvelope) => void;

type Bus = {
  listeners: Set<Listener>;
};

declare global {
  // eslint-disable-next-line no-var
  var __mockServerSseBus: Bus | undefined;
}

function getBus(): Bus {
  if (!globalThis.__mockServerSseBus) {
    globalThis.__mockServerSseBus = { listeners: new Set() };
  }
  return globalThis.__mockServerSseBus;
}

export function subscribeServerSse(listener: Listener) {
  const bus = getBus();
  bus.listeners.add(listener);
  return () => bus.listeners.delete(listener);
}

export function emitServerSseEvent(event: string, data: unknown) {
  const bus = getBus();
  for (const l of bus.listeners) {
    try {
      l({ event, data });
    } catch {
      // ignore listener errors
    }
  }
}


