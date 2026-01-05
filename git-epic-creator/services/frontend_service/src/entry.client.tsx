import { startTransition, StrictMode } from "react";
import { hydrateRoot } from "react-dom/client";
import { HydratedRouter } from "react-router/dom";

import { maybeStartMocks } from "./lib/mockMode";

(async () => {
  // Important: in mock mode, we must install MSW + mock EventSource BEFORE React mounts,
  // otherwise `useSseBridge()` may create a real EventSource connection first.
  await maybeStartMocks();

  startTransition(() => {
    hydrateRoot(
      document,
      <StrictMode>
        <HydratedRouter />
      </StrictMode>,
    );
  });
})().catch((e) => {
  // eslint-disable-next-line no-console
  console.error(e);
});
